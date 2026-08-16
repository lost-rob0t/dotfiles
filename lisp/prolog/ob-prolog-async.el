;;; ob-prolog-async.el --- Non-blocking Org Babel Prolog -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ob-prolog)
(require 'subr-x)

(cl-defstruct (ob-prolog-async--request
               (:constructor ob-prolog-async--request-create))
  id
  source-buffer
  source-overlay
  source-body
  params
  result-params
  temp-file
  output-buffer
  error-buffer)

(defvar ob-prolog-async--next-id 0
  "Monotonically increasing identifier for asynchronous Prolog requests.")

(defun ob-prolog-async--truthy-p (value)
  "Return non-nil when VALUE enables an Org Babel boolean header argument."
  (and value
       (not (member (downcase (format "%s" value))
                    '("" "0" "false" "nil" "no" "off")))))

(defun ob-prolog-async--enabled-p (params)
  "Return non-nil when PARAMS requests asynchronous execution."
  (when-let ((entry (assq :async params)))
    (let ((value (cdr entry)))
      (or (null value)
          (ob-prolog-async--truthy-p value)))))

(defun ob-prolog-async--session-none-p (params)
  "Return non-nil when PARAMS uses no persistent Prolog session."
  (let ((session (cdr (assq :session params))))
    (or (null session)
        (equal session "none"))))

(defun ob-prolog-async--source-overlay (request-id)
  "Create an overlay marking the source block for REQUEST-ID."
  (let* ((head (or (org-babel-where-is-src-block-head)
                   (point)))
         (end (min (point-max) (1+ head)))
         (overlay (make-overlay head end (current-buffer) nil t)))
    (overlay-put overlay 'ob-prolog-async-request-id request-id)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun ob-prolog-async--supersede-existing-request (head)
  "Delete an existing async request overlay at source block HEAD."
  (dolist (overlay (overlays-at head))
    (when (overlay-get overlay 'ob-prolog-async-request-id)
      (delete-overlay overlay))))

(defun ob-prolog-async--command (goal temp-file)
  "Build a direct process command for GOAL against TEMP-FILE."
  (append (split-string-and-unquote org-babel-prolog-command)
          (list "--quiet"
                "-l" temp-file
                "-g" (or goal "halt")
                "-t" "halt")))

(defun ob-prolog-async--read-buffer (buffer)
  "Return BUFFER contents without text properties."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun ob-prolog-async--format-result (output params)
  "Format Prolog OUTPUT using the same Babel result rules as PARAMS."
  (let ((result-params (cdr (assq :result-params params))))
    (unless (string-empty-p output)
      (org-babel-reassemble-table
       (org-babel-result-cond result-params
         output
         (let ((tmp (org-babel-temp-file "prolog-results-")))
           (unwind-protect
               (progn
                 (with-temp-file tmp
                   (insert output))
                 (org-babel-import-elisp-from-file tmp))
             (ignore-errors (delete-file tmp)))))
       (org-babel-pick-name (cdr (assq :colname-names params))
                            (cdr (assq :colnames params)))
       (org-babel-pick-name (cdr (assq :rowname-names params))
                            (cdr (assq :rownames params)))))))

(defun ob-prolog-async--current-info (request)
  "Return current Babel info when REQUEST still targets its original block."
  (let ((buffer (ob-prolog-async--request-source-buffer request))
        (overlay (ob-prolog-async--request-source-overlay request)))
    (when (and (buffer-live-p buffer)
               (overlay-buffer overlay))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (overlay-start overlay))
          (let ((info (ignore-errors (org-babel-get-src-block-info 'light))))
            (when (and info
                       (string= (car info) "prolog")
                       (equal (nth 1 info)
                              (ob-prolog-async--request-source-body request)))
              info)))))))

(defun ob-prolog-async--insert-result (request result)
  "Insert RESULT for REQUEST using its original Babel result policy."
  (when-let ((info (ob-prolog-async--current-info request)))
    (let ((buffer (ob-prolog-async--request-source-buffer request))
          (overlay (ob-prolog-async--request-source-overlay request))
          (result-params (ob-prolog-async--request-result-params request)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (overlay-start overlay))
          (unless (or (member "none" result-params)
                      (member "silent" result-params))
            (setf (nth 2 info) (ob-prolog-async--request-params request))
            (org-babel-insert-result result result-params info nil "prolog")))))))

(defun ob-prolog-async--cleanup (request)
  "Release temporary resources owned by REQUEST."
  (let ((overlay (ob-prolog-async--request-source-overlay request))
        (temp-file (ob-prolog-async--request-temp-file request))
        (output-buffer (ob-prolog-async--request-output-buffer request))
        (error-buffer (ob-prolog-async--request-error-buffer request)))
    (when (overlayp overlay)
      (delete-overlay overlay))
    (when (and temp-file (file-exists-p temp-file))
      (ignore-errors (delete-file temp-file)))
    (when (buffer-live-p output-buffer)
      (kill-buffer output-buffer))
    (when (buffer-live-p error-buffer)
      (kill-buffer error-buffer))))

(defun ob-prolog-async--finish (request process)
  "Finish REQUEST after PROCESS exits."
  (unwind-protect
      (let* ((exit-code (process-exit-status process))
             (stdout (ob-prolog-async--read-buffer
                      (ob-prolog-async--request-output-buffer request)))
             (stderr (ob-prolog-async--read-buffer
                      (ob-prolog-async--request-error-buffer request))))
        (if (zerop exit-code)
            (ob-prolog-async--insert-result
             request
             (ob-prolog-async--format-result stdout
                                             (ob-prolog-async--request-params request)))
          (let ((error-text
                 (string-trim
                  (string-join
                   (delq nil
                         (list (format "ob-prolog async exited with status %d" exit-code)
                               (unless (string-empty-p stderr) stderr)
                               (unless (string-empty-p stdout) stdout)))
                   "\n"))))
            (ob-prolog-async--insert-result request error-text)
            (display-warning 'ob-prolog-async error-text :error))))
    (ob-prolog-async--cleanup request)))

(defun ob-prolog-async--sentinel (process _event)
  "Schedule result handling when PROCESS reaches a terminal state."
  (when (memq (process-status process) '(exit signal))
    (let ((request (process-get process 'ob-prolog-async-request)))
      (when request
        (process-put process 'ob-prolog-async-request nil)
        ;; Run after the current Babel command returns to the event loop.
        (run-at-time 0 nil #'ob-prolog-async--finish request process)))))

(defun ob-prolog-async--validate-params (params)
  "Reject PARAMS whose semantics cannot be preserved asynchronously."
  (let ((result-params (cdr (assq :result-params params))))
    (unless (ob-prolog-async--session-none-p params)
      (user-error "ob-prolog :async does not support named :session blocks; use :session none"))
    (when (member "file" result-params)
      (user-error "ob-prolog :async does not yet support :results file"))
    (when (assq :post params)
      (user-error "ob-prolog :async does not yet support :post"))
    (when (equal (cdr (assq :cache params)) "yes")
      (user-error "ob-prolog :async does not yet support :cache yes"))))

(defun ob-prolog-async--execute (body params)
  "Start BODY with PARAMS in a non-blocking SWI-Prolog process."
  (ob-prolog-async--validate-params params)
  (let* ((request-id (cl-incf ob-prolog-async--next-id))
         (head (or (org-babel-where-is-src-block-head) (point)))
         (_ (ob-prolog-async--supersede-existing-request head))
         (result-params (cdr (assq :result-params params)))
         (goal (org-babel-prolog--parse-goal (cdr (assq :goal params))))
         (vars (org-babel-variable-assignments:prolog params))
         (full-body (org-babel-expand-body:generic body params vars))
         (temp-file (org-babel-temp-file "prolog-async-" ".pl"))
         (output-buffer (generate-new-buffer
                         (format " *ob-prolog-async-%d*" request-id)))
         (error-buffer (generate-new-buffer
                        (format " *ob-prolog-async-%d-stderr*" request-id)))
         (overlay (ob-prolog-async--source-overlay request-id))
         (source-info (ignore-errors (org-babel-get-src-block-info 'light)))
         (source-body (if source-info (nth 1 source-info) body))
         (request (ob-prolog-async--request-create
                   :id request-id
                   :source-buffer (current-buffer)
                   :source-overlay overlay
                   :source-body source-body
                   :params params
                   :result-params result-params
                   :temp-file temp-file
                   :output-buffer output-buffer
                   :error-buffer error-buffer))
         process)
    (with-temp-file temp-file
      (insert (org-babel-chomp full-body)))
    (condition-case err
        (setq process
              (make-process
               :name (format "ob-prolog-async-%d" request-id)
               :buffer output-buffer
               :command (ob-prolog-async--command goal temp-file)
               :connection-type 'pipe
               :noquery t
               :stderr error-buffer
               :sentinel #'ignore))
      (error
       (ob-prolog-async--cleanup request)
       (signal (car err) (cdr err))))
    (process-put process 'ob-prolog-async-request request)
    (set-process-sentinel process #'ob-prolog-async--sentinel)
    (when (memq (process-status process) '(exit signal))
      (ob-prolog-async--sentinel process "finished"))
    nil))

(defun ob-prolog-async--around-execute (original body params)
  "Run ORIGINAL synchronously unless PARAMS contains an enabled :async flag."
  (if (ob-prolog-async--enabled-p params)
      (ob-prolog-async--execute body params)
    (funcall original body params)))

(add-to-list 'org-babel-header-args:prolog '(:async . ((yes no))))
(advice-remove 'org-babel-execute:prolog #'ob-prolog-async--around-execute)
(advice-add 'org-babel-execute:prolog :around #'ob-prolog-async--around-execute)

(provide 'ob-prolog-async)
;;; ob-prolog-async.el ends here
