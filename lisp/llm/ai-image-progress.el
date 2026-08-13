;;; ai-image-progress.el --- Progress UI for image generation -*- lexical-binding: t; -*-

(require 'ai-image)
(require 'cl-lib)
(require 'json)
(require 'plz)
(require 'subr-x)

(defgroup ai/image-progress nil
  "Progress feedback for asynchronous image generation."
  :group 'ai/image
  :prefix "ai/image-progress-")

(defcustom ai/image-progress-heartbeat-first-delay 10
  "Seconds before the first image-generation heartbeat."
  :type 'number
  :group 'ai/image-progress)

(defcustom ai/image-progress-heartbeat-interval 15
  "Seconds between image-generation heartbeats after the first one."
  :type 'number
  :group 'ai/image-progress)

(defcustom ai/image-progress-long-running-after 25
  "Seconds after which image generation is described as taking longer."
  :type 'number
  :group 'ai/image-progress)

(defcustom ai/image-progress-streaming-models
  '("openai/gpt-image-2" "openai/gpt-image-1" "openai/gpt-image-1-mini")
  "Image models known to expose native OpenRouter SSE progress events.
Models not listed here keep the same generated output but use heartbeat-only
progress while the buffered response is pending."
  :type '(repeat string)
  :group 'ai/image-progress)

(cl-defstruct (ai/image-progress-request
               (:constructor ai/image-progress-request-create))
  id
  state
  model
  output-file
  callback
  origin-buffer
  insertion-marker
  open-after
  status-buffer
  status-overlay
  started-at
  heartbeat-timer
  process
  sse-buffer
  completed-event
  stream-error
  callback-called)

(defvar ai/image-progress--next-id 0
  "Monotonic identifier for image-generation requests.")

(defvar ai/image-progress--requests (make-hash-table :test #'eql)
  "Active image-generation requests keyed by request id.")

(defun ai/image-progress--streaming-p (&optional model)
  "Return non-nil when MODEL exposes native image SSE events."
  (member (or model ai/image-model) ai/image-progress-streaming-models))

(defun ai/image-progress--elapsed (request)
  "Return elapsed whole seconds for REQUEST."
  (max 0 (truncate (- (float-time) (ai/image-progress-request-started-at request)))))

(defun ai/image-progress--status-target (origin-buffer)
  "Return the buffer that should display progress for ORIGIN-BUFFER."
  (cond
   ((buffer-live-p origin-buffer) origin-buffer)
   ((and (buffer-live-p (current-buffer))
         (or (derived-mode-p 'org-mode)
             (bound-and-true-p gptel-mode)))
    (current-buffer))))

(defun ai/image-progress--make-overlay (buffer)
  "Create a zero-width progress overlay at the end of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((overlay (make-overlay (point-max) (point-max) buffer nil nil)))
        (overlay-put overlay 'priority 1001)
        overlay))))

(defun ai/image-progress--render (request text &optional details)
  "Update REQUEST's single visible status element to TEXT.
DETAILS, when non-nil, is exposed as overlay help text."
  (when-let ((overlay (ai/image-progress-request-status-overlay request)))
    (when (overlay-buffer overlay)
      (overlay-put
       overlay 'after-string
       (propertize
        (format "\n[Image %d] %s\n" (ai/image-progress-request-id request) text)
        'face 'shadow))
      (overlay-put overlay 'help-echo details))))

(defun ai/image-progress--stop-heartbeat (request)
  "Stop REQUEST's heartbeat timer and clear its timer slot."
  (when-let ((timer (ai/image-progress-request-heartbeat-timer request)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (ai/image-progress-request-heartbeat-timer request) nil)))

(defun ai/image-progress--state (request state text &optional details)
  "Move REQUEST to STATE and render TEXT with optional DETAILS."
  (setf (ai/image-progress-request-state request) state)
  (unless (eq state 'running)
    (ai/image-progress--stop-heartbeat request))
  (ai/image-progress--render request text details)
  request)

(defun ai/image-progress--heartbeat (request)
  "Refresh REQUEST's running status without inventing provider progress."
  (when (eq (ai/image-progress-request-state request) 'running)
    (let ((elapsed (ai/image-progress--elapsed request)))
      (ai/image-progress--render
       request
       (if (>= elapsed ai/image-progress-long-running-after)
           (format "Image generation is taking a little longer... (%ds)" elapsed)
         (format "Still generating image... (%ds)" elapsed))))))

(defun ai/image-progress--start-heartbeat (request)
  "Start the conservative liveness heartbeat for REQUEST."
  (setf
   (ai/image-progress-request-heartbeat-timer request)
   (run-at-time ai/image-progress-heartbeat-first-delay
                ai/image-progress-heartbeat-interval
                #'ai/image-progress--heartbeat request)))

(defun ai/image-progress--json-read-string (string)
  "Parse JSON STRING as an alist with symbol keys."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (json-read-from-string string)))

(defun ai/image-progress--api-error-from-string (string)
  "Return the best API error text extractable from STRING."
  (condition-case nil
      (ai/image--api-error (ai/image-progress--json-read-string string))
    (error nil)))

(defun ai/image-progress--plz-error-text (error)
  "Return useful details from plz ERROR."
  (let* ((response (and (fboundp 'plz-error-response)
                        (plz-error-response error)))
         (body (and response
                    (fboundp 'plz-response-body)
                    (plz-response-body response)))
         (body-text (and (stringp body)
                         (ai/image-progress--api-error-from-string body)))
         (message-text (and (fboundp 'plz-error-message)
                            (plz-error-message error))))
    (or body-text
        (and (stringp message-text) (not (string-empty-p message-text)) message-text)
        (format "%S" error))))

(defun ai/image-progress--normalize-stream-payload (event)
  "Convert completed SSE EVENT to the buffered payload shape."
  `((data . (((b64_json . ,(alist-get 'b64_json event))
              (media_type . ,(alist-get 'media_type event)))))
    (usage . ,(alist-get 'usage event))))

(defun ai/image-progress--finish-callback (request result)
  "Invoke REQUEST's callback once with RESULT."
  (unless (ai/image-progress-request-callback-called request)
    (setf (ai/image-progress-request-callback-called request) t)
    (when-let ((callback (ai/image-progress-request-callback request)))
      (funcall callback result))))

(defun ai/image-progress--cleanup (request)
  "Release active resources owned by REQUEST."
  (ai/image-progress--stop-heartbeat request)
  (remhash (ai/image-progress-request-id request) ai/image-progress--requests)
  (when-let ((marker (ai/image-progress-request-insertion-marker request)))
    (set-marker marker nil)
    (setf (ai/image-progress-request-insertion-marker request) nil)))

(defun ai/image-progress--complete (request payload)
  "Process successful image PAYLOAD for REQUEST."
  (unless (memq (ai/image-progress-request-state request) '(completed cancelled))
    (condition-case err
        (progn
          (ai/image-progress--state request 'processing "Processing generated image...")
          (let ((encoded (ai/image--result payload))
                (output-file (ai/image-progress-request-output-file request)))
            (unless encoded
              (error "OpenRouter returned no image data"))
            (ai/image--write-result encoded output-file)
            (ai/image--insert-org-image
             (ai/image-progress-request-origin-buffer request)
             (ai/image-progress-request-insertion-marker request)
             output-file)
            (when (and (ai/image-progress-request-open-after request)
                       (not (and (buffer-live-p (ai/image-progress-request-origin-buffer request))
                                 (with-current-buffer
                                     (ai/image-progress-request-origin-buffer request)
                                   (derived-mode-p 'org-mode)))))
              (find-file-other-window output-file))
            (ai/image-progress--finish-callback
             request (ai/image--tool-result output-file payload))
            (ai/image-progress--state
             request 'completed
             (format "Image ready. (%ds)" (ai/image-progress--elapsed request)))
            (ai/image-progress--cleanup request)))
      (error
       (ai/image-progress--fail request (error-message-string err))))))

(defun ai/image-progress--fail (request details)
  "Mark REQUEST failed with DETAILS and finish its callback."
  (unless (memq (ai/image-progress-request-state request) '(completed cancelled))
    (ai/image-progress--state
     request 'failed
     (format "Image generation failed. (%ds)" (ai/image-progress--elapsed request))
     details)
    (ai/image-progress--finish-callback
     request (format "ERROR: Image generation failed: %s" details))
    (ai/image-progress--cleanup request)))

(defun ai/image-progress--handle-sse-event (request event)
  "Apply one decoded OpenRouter SSE EVENT to REQUEST."
  (pcase (alist-get 'type event)
    ("image_generation.partial_image"
     (let ((index (or (alist-get 'partial_image_index event) 0)))
       (ai/image-progress--render
        request
        (format "Received image preview %d; generation still in progress... (%ds)"
                (1+ index) (ai/image-progress--elapsed request)))))
    ("image_generation.completed"
     (setf (ai/image-progress-request-completed-event request) event)
     (ai/image-progress--state request 'processing "Processing generated image..."))
    ("error"
     (let* ((error-object (alist-get 'error event))
            (details (or (and (listp error-object) (alist-get 'message error-object))
                         (format "%S" error-object))))
       (setf (ai/image-progress-request-stream-error request) details)
       (ai/image-progress--state
        request 'failed
        (format "Image generation failed. (%ds)" (ai/image-progress--elapsed request))
        details)))))

(defun ai/image-progress--consume-sse (request chunk)
  "Consume SSE CHUNK for REQUEST, retaining an incomplete final line."
  (let* ((input (concat (or (ai/image-progress-request-sse-buffer request) "") chunk))
         (lines (split-string input "\n"))
         (complete-lines (butlast lines))
         (remainder (car (last lines))))
    (setf (ai/image-progress-request-sse-buffer request) remainder)
    (dolist (raw-line complete-lines)
      (let ((line (string-trim-right raw-line "\r")))
        (when (string-prefix-p "data:" line)
          (let ((data (string-trim-left (substring line 5))))
            (unless (or (string-empty-p data) (string= data "[DONE]"))
              (condition-case nil
                  (ai/image-progress--handle-sse-event
                   request (ai/image-progress--json-read-string data))
                (error nil)))))))))

(defun ai/image-progress--stream-filter (request process output)
  "Insert curl OUTPUT for PROCESS and inspect native events for REQUEST."
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert output)))))
  (ai/image-progress--consume-sse request output))

(defun ai/image-progress--buffered-success (request body)
  "Finish non-streaming REQUEST from buffered JSON BODY."
  (condition-case err
      (ai/image-progress--complete request (ai/image-progress--json-read-string body))
    (error
     (ai/image-progress--fail request (error-message-string err)))))

(defun ai/image-progress--stream-success (request body)
  "Finish streaming REQUEST after curl receives complete SSE BODY."
  ;; Re-consuming BODY is unnecessary in normal operation, but protects against
  ;; custom filter behavior changing in a future plz release.
  (unless (ai/image-progress-request-completed-event request)
    (ai/image-progress--consume-sse request (concat body "\n")))
  (cond
   ((ai/image-progress-request-stream-error request)
    (ai/image-progress--fail request (ai/image-progress-request-stream-error request)))
   ((ai/image-progress-request-completed-event request)
    (ai/image-progress--complete
     request
     (ai/image-progress--normalize-stream-payload
      (ai/image-progress-request-completed-event request))))
   (t
    (ai/image-progress--fail request "OpenRouter stream ended without a completed image event"))))

(defun ai/image-progress--request-error (request error)
  "Finish REQUEST from plz ERROR."
  (unless (eq (ai/image-progress-request-state request) 'cancelled)
    (ai/image-progress--fail request (ai/image-progress--plz-error-text error))))

(defun ai/image-progress--request-body (prompt reference-file streaming)
  "Return encoded image request JSON for PROMPT and REFERENCE-FILE.
When STREAMING is non-nil, request native SSE events."
  (json-encode
   (append (ai/image--request-body prompt reference-file)
           (when streaming '((stream . t))))))

(defun ai/image-progress--request
    (prompt output-file &optional reference-file callback origin-buffer insertion-marker open-after)
  "Generate PROMPT asynchronously with image-specific progress feedback.
This function overrides `ai/image--request' while preserving its arguments and
final output behavior."
  (let* ((key (ai/llm--require-api-key 'openrouter))
         (streaming (ai/image-progress--streaming-p ai/image-model))
         (status-buffer (ai/image-progress--status-target origin-buffer))
         (request
          (ai/image-progress-request-create
           :id (cl-incf ai/image-progress--next-id)
           :state 'queued
           :model ai/image-model
           :output-file output-file
           :callback callback
           :origin-buffer origin-buffer
           :insertion-marker insertion-marker
           :open-after open-after
           :status-buffer status-buffer
           :started-at (float-time)
           :sse-buffer "")))
    (setf (ai/image-progress-request-status-overlay request)
          (ai/image-progress--make-overlay status-buffer))
    (puthash (ai/image-progress-request-id request) request ai/image-progress--requests)
    (ai/image-progress--state request 'preparing "Preparing image request...")
    (condition-case err
        (let ((process
               (plz 'post ai/image-endpoint
                 :headers `(("Authorization" . ,(concat "Bearer " key))
                            ("Content-Type" . "application/json")
                            ("X-Title" . "Emacs gptel image tools"))
                 :body (encode-coding-string
                        (ai/image-progress--request-body prompt reference-file streaming)
                        'utf-8)
                 :body-type 'binary
                 :as 'string
                 :decode t
                 :noquery t
                 :filter (when streaming
                           (lambda (process output)
                             (ai/image-progress--stream-filter request process output)))
                 :then (if streaming
                           (lambda (body)
                             (ai/image-progress--stream-success request body))
                         (lambda (body)
                           (ai/image-progress--buffered-success request body)))
                 :else (lambda (error)
                         (ai/image-progress--request-error request error)))))
          (setf (ai/image-progress-request-process request) process)
          (ai/image-progress--state
           request 'running (format "Generating image with %s..." ai/image-model))
          (ai/image-progress--start-heartbeat request)
          request)
      (error
       (ai/image-progress--fail request (error-message-string err))
       request))))

(defun ai/image-progress--active-requests ()
  "Return active image requests sorted by id."
  (let (requests)
    (maphash (lambda (_id request) (push request requests)) ai/image-progress--requests)
    (sort requests
          (lambda (left right)
            (< (ai/image-progress-request-id left)
               (ai/image-progress-request-id right))))))

(defun ai/image-progress--cancel-request (request)
  "Cancel REQUEST and replace its running status immediately."
  (unless (memq (ai/image-progress-request-state request) '(completed failed cancelled))
    (ai/image-progress--state
     request 'cancelled
     (format "Image generation cancelled. (%ds)" (ai/image-progress--elapsed request)))
    (ai/image-progress--finish-callback request "ERROR: Image generation cancelled")
    (when-let ((process (ai/image-progress-request-process request)))
      (when (process-live-p process)
        (delete-process process)))
    (ai/image-progress--cleanup request)))

;;;###autoload
(defun ai/image-cancel (&optional request-id)
  "Cancel an active image generation identified by REQUEST-ID.
Interactively, cancel the sole active request or prompt when several are active."
  (interactive)
  (let* ((requests (ai/image-progress--active-requests))
         (request
          (cond
           (request-id (gethash request-id ai/image-progress--requests))
           ((null requests) (user-error "No active image generation requests"))
           ((null (cdr requests)) (car requests))
           (t
            (let* ((choices
                    (mapcar
                     (lambda (item)
                       (cons (format "Image %d — %s"
                                     (ai/image-progress-request-id item)
                                     (ai/image-progress-request-state item))
                             item))
                     requests))
                   (choice (completing-read "Cancel image request: " choices nil t)))
              (cdr (assoc choice choices)))))))
    (unless request
      (user-error "Unknown image request: %s" request-id))
    (ai/image-progress--cancel-request request)))

(advice-add 'ai/image--request :override #'ai/image-progress--request)

(provide 'ai-image-progress)
;;; ai-image-progress.el ends here
