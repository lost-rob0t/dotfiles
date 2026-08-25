(in-package #:nyxt-user)

(defparameter *operation* "default"
  "Current capture/reporting operation name.")

(defparameter *document-path*
  (uiop:merge-pathnames* #P"nx-document/" (user-homedir-pathname))
  "Root directory for Nyxt operation artifacts.")

(defun safe-path-component (value)
  "Return VALUE as a filesystem-friendly component."
  (let ((string (princ-to-string value)))
    (with-output-to-string (stream)
      (loop for character across string
            do (write-char
                (if (or (alphanumericp character)
                        (find character "-_."))
                    character
                    #\_)
                stream)))))

(defun shortened-component (value &optional (limit 96))
  "Return a sanitized VALUE capped at LIMIT characters."
  (let ((safe (safe-path-component value)))
    (if (zerop (length safe))
        "nyxt"
        (subseq safe 0 (min limit (length safe))))))

(defun operation-directory ()
  "Return the current operation directory, creating it if needed."
  (let ((path
          (merge-pathnames
           (format nil "~a/" (shortened-component *operation* 64))
           *document-path*)))
    (ensure-directories-exist path)
    path))

(defun operation-image-directory ()
  "Return the current operation image directory, creating it if needed."
  (let ((path (merge-pathnames #P"images/" (operation-directory))))
    (ensure-directories-exist path)
    path))

(defun capture-label (&optional (buffer (current-buffer)))
  "Return a useful filename stem for BUFFER."
  (cond
    ((and buffer (web-buffer-p buffer) (url buffer))
     (shortened-component (render-url (url buffer))))
    (buffer
     (shortened-component (title buffer)))
    (t
     "nyxt")))

(defun capture-path (&optional (buffer (current-buffer)))
  "Return a new PNG pathname for BUFFER in the current operation."
  (merge-pathnames
   (format nil "~d-~a.png" (get-universal-time) (capture-label buffer))
   (operation-image-directory)))

(defun run-scrot (arguments path)
  "Run scrot with ARGUMENTS and write the capture to PATH."
  (handler-case
      (progn
        (uiop:run-program
         (append (list "scrot") arguments (list "-F" (namestring path)))
         :output *standard-output*
         :error-output *error-output*)
        path)
    (error (condition)
      (echo "scrot failed: ~a" condition)
      nil)))

(define-command scrot (&optional (buffer (current-buffer)))
  "Capture the focused Nyxt window into the current operation."
  (let ((path (capture-path buffer)))
    (when (run-scrot '("-u") path)
      (echo "Saved capture: ~a" (namestring path)))))

(define-command scrot-select (&optional (buffer (current-buffer)))
  "Interactively select a region and save it into the current operation."
  (let ((path (capture-path buffer)))
    (when (run-scrot '("-s") path)
      (echo "Saved selection: ~a" (namestring path)))))

(define-command set-operation ()
  "Set the operation used by screenshot/reporting commands."
  (let* ((input
           (prompt1
             :prompt "Operation"
             :sources 'prompter:raw-source))
         (name
           (string-trim
            '(#\Space #\Tab #\Newline #\Return)
            input)))
    (if (zerop (length name))
        (echo "Operation unchanged: ~a" *operation*)
        (progn
          (setf *operation* name)
          (operation-image-directory)
          (echo "Operation: ~a" *operation*)))))

(define-command show-operation ()
  "Display the current operation and capture directory."
  (echo "~a → ~a"
        *operation*
        (namestring (operation-image-directory))))

(define-command open-operation-directory ()
  "Open the current operation directory with the desktop file manager."
  (uiop:launch-program
   (list "xdg-open" (namestring (operation-directory)))))
