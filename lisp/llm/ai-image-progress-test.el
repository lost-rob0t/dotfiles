;;; ai-image-progress-test.el --- Tests for image progress UX -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'timer)

;; Keep these tests runnable in a minimal Emacs batch job.  ai-image only needs
;; these features to be present while its functions are defined.
(unless (featurep 'ai)
  (provide 'ai))
(unless (featurep 'ai-prompts)
  (provide 'ai-prompts))
(unless (require 'plz nil t)
  (cl-defstruct plz-response version status headers body)
  (cl-defstruct plz-error curl-error response message)
  (defun plz (&rest _args)
    (error "Unexpected network call in ai-image-progress test"))
  (provide 'plz))

(require 'ai-image-progress)

(defun ai/image-progress-test--request (&optional callback age)
  "Create and register a running test request with CALLBACK and AGE seconds."
  (let* ((buffer (generate-new-buffer " *ai-image-progress-test*"))
         (id (cl-incf ai/image-progress--next-id))
         (request
          (ai/image-progress-request-create
           :id id
           :state 'running
           :model "openai/gpt-image-2"
           :output-file (make-temp-name
                         (expand-file-name "ai-image-progress-" temporary-file-directory))
           :callback callback
           :status-buffer buffer
           :status-overlay (ai/image-progress--make-overlay buffer)
           :started-at (- (float-time) (or age 0))
           :sse-buffer "")))
    (puthash id request ai/image-progress--requests)
    request))

(defun ai/image-progress-test--dispose (request)
  "Dispose resources retained by test REQUEST."
  (when request
    (ai/image-progress--stop-heartbeat request)
    (remhash (ai/image-progress-request-id request) ai/image-progress--requests)
    (when-let ((file (ai/image-progress-request-output-file request)))
      (when (file-exists-p file)
        (delete-file file)))
    (when-let ((overlay (ai/image-progress-request-status-overlay request)))
      (when (overlayp overlay)
        (delete-overlay overlay)))
    (when-let ((buffer (ai/image-progress-request-status-buffer request)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ai/image-progress-normal-completion ()
  (let (callback-result request)
    (unwind-protect
        (progn
          (setq request
                (ai/image-progress-test--request
                 (lambda (result) (setq callback-result result)) 3))
          (ai/image-progress--complete
           request
           '((data . (((b64_json . "ZmFrZQ=="))))
             (usage . ((cost . 0.01)))))
          (should (eq (ai/image-progress-request-state request) 'completed))
          (should (file-exists-p (ai/image-progress-request-output-file request)))
          (should (string-match-p "Generated image" callback-result))
          (should (null (gethash (ai/image-progress-request-id request)
                                 ai/image-progress--requests)))
          (should (null (ai/image-progress-request-heartbeat-timer request)))
          (should (string-match-p
                   "Image ready"
                   (overlay-get (ai/image-progress-request-status-overlay request)
                                'after-string))))
      (ai/image-progress-test--dispose request))))

(ert-deftest ai/image-progress-long-running-heartbeat ()
  (let ((request (ai/image-progress-test--request nil 30)))
    (unwind-protect
        (progn
          (ai/image-progress--heartbeat request)
          (let ((status (overlay-get
                         (ai/image-progress-request-status-overlay request)
                         'after-string)))
            (should (string-match-p "taking a little longer" status))
            (should (string-match-p "([0-9]+s)" status))))
      (ai/image-progress-test--dispose request))))

(ert-deftest ai/image-progress-api-failure ()
  (let (callback-result request)
    (unwind-protect
        (progn
          (setq request
                (ai/image-progress-test--request
                 (lambda (result) (setq callback-result result)) 4))
          (ai/image-progress--fail request "provider exploded")
          (should (eq (ai/image-progress-request-state request) 'failed))
          (should (string-match-p "provider exploded" callback-result))
          (should (null (gethash (ai/image-progress-request-id request)
                                 ai/image-progress--requests)))
          (should (string-match-p
                   "Image generation failed"
                   (overlay-get (ai/image-progress-request-status-overlay request)
                                'after-string)))
          (should (equal "provider exploded"
                         (overlay-get (ai/image-progress-request-status-overlay request)
                                      'help-echo))))
      (ai/image-progress-test--dispose request))))

(ert-deftest ai/image-progress-cancellation ()
  (let (callback-result request)
    (unwind-protect
        (progn
          (setq request
                (ai/image-progress-test--request
                 (lambda (result) (setq callback-result result)) 7))
          (ai/image-progress--cancel-request request)
          (should (eq (ai/image-progress-request-state request) 'cancelled))
          (should (equal callback-result "ERROR: Image generation cancelled"))
          (should (null (gethash (ai/image-progress-request-id request)
                                 ai/image-progress--requests)))
          (should (string-match-p
                   "Image generation cancelled"
                   (overlay-get (ai/image-progress-request-status-overlay request)
                                'after-string))))
      (ai/image-progress-test--dispose request))))

(ert-deftest ai/image-progress-cleans-heartbeat-timer ()
  (let ((request (ai/image-progress-test--request nil 1))
        timer)
    (unwind-protect
        (progn
          (setq timer (run-at-time 3600 nil #'ignore))
          (setf (ai/image-progress-request-heartbeat-timer request) timer)
          (ai/image-progress--fail request "stop")
          (should (null (ai/image-progress-request-heartbeat-timer request)))
          (when (boundp 'timer-list)
            (should-not (memq timer timer-list))))
      (when (timerp timer)
        (cancel-timer timer))
      (ai/image-progress-test--dispose request))))

(ert-deftest ai/image-progress-concurrent-requests-are-isolated ()
  (let ((first (ai/image-progress-test--request nil 2))
        (second (ai/image-progress-test--request nil 12)))
    (unwind-protect
        (let ((second-before
               (overlay-get (ai/image-progress-request-status-overlay second)
                            'after-string)))
          (ai/image-progress--fail first "first failed")
          (should (eq (ai/image-progress-request-state first) 'failed))
          (should (eq (ai/image-progress-request-state second) 'running))
          (should (eq second
                      (gethash (ai/image-progress-request-id second)
                               ai/image-progress--requests)))
          (should (equal second-before
                         (overlay-get (ai/image-progress-request-status-overlay second)
                                      'after-string))))
      (ai/image-progress-test--dispose first)
      (ai/image-progress-test--dispose second))))

(ert-deftest ai/image-progress-consumes-native-sse-events ()
  (let ((request (ai/image-progress-test--request nil 5)))
    (unwind-protect
        (progn
          (ai/image-progress--consume-sse
           request
           "data: {\"type\":\"image_generation.partial_image\",\"partial_image_index\":0,\"b64_json\":\"preview\"}\n\n")
          (should (string-match-p
                   "Received image preview 1"
                   (overlay-get (ai/image-progress-request-status-overlay request)
                                'after-string)))
          (ai/image-progress--consume-sse
           request
           "data: {\"type\":\"image_generation.completed\",\"b64_json\":\"ZmFrZQ==\",\"media_type\":\"image/png\",\"usage\":{\"cost\":0.02}}\n\ndata: [DONE]\n\n")
          (should (eq (ai/image-progress-request-state request) 'processing))
          (should (equal "ZmFrZQ=="
                         (alist-get 'b64_json
                                    (ai/image-progress-request-completed-event request)))))
      (ai/image-progress-test--dispose request))))

(ert-deftest ai/image-progress-streaming-capability-is-model-specific ()
  (should (ai/image-progress--streaming-p "openai/gpt-image-2"))
  (should-not (ai/image-progress--streaming-p "example/non-streaming-model")))

(provide 'ai-image-progress-test)
;;; ai-image-progress-test.el ends here
