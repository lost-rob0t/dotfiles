(in-package :nyxt)

(defparameter *operation* "default"
  "current operation name.")
(defparameter *document-path* (uiop:merge-pathnames* #P"nx-document/" (user-homedir-pathname)))
(defparameter)

(defun get-op-dir ()
  (let ((op-path (uiop:merge-pathnames* *operation* *document-path*)))
    (ensure-directories-exist op-path)
    op-path))
(defun get-op-image-dir ()
  (let ((image-path (uiop:merge-pathnames* "images/" (get-op-dir))))
    (ensure-directories-exist image-path)
    image-path))
(defun init-dirs ()
  (get-op-dir)
  (get-op-image-dir))
(defun make-url-filename (url format)
  (let ((time (local-time:now)))
    (substitute #\- #\/ (format nil "~a-~a.~a" url time format))))



(defparameter *op-images-dir* (get-op-image-dir))

;; Nyxt doesnt seem to support screenshot taking yet
(defun screenshot-window (path)
  "Take a screenshot of the window and save it at PATH"
  (uiop:run-program (format nil  "scrot  -u -F ~a" (namestring path))))

(defun screenshot-selection (path)
  "Select and area and screenshot it at PATH"
  (uiop:run-program (format nil  "scrot  -s -F ~a" (namestring path))))




(defun screenshot-buffer (&optional (buffer (current-buffer)))
  "Take a screenshot of the  buffer"
  (when (web-buffer-p buffer)
    (let* (
           (b-url (url buffer))
           (image-path (uiop:merge-pathnames* (uiop:merge-pathnames* (get-op-image-dir) (make-url-filename b-url "png")) (get-op-dir))))

      (if (null b-url)
          (echo "Buffer has no url")
          (progn
            (echo "Taking screenshot: ~a" image-path)
            (screenshot-window image-path)
            (echo "Screenshot done"))))))

(defun screenshot-buffer-selection (&optional (buffer (current-buffer)))
  "Take a screenshot of the  buffer"
  (when (web-buffer-p buffer)
    (let* ((time (local-time:now))
           (b-url (url buffer))
           (image-path (merge-pathnames (format nil "~a-~a.png" b-url time) (get-op-image-dir))))
      (uiop:ensure-all-directories-exist (list *document-path* (get-op-dir) (get-op-image-dir)))
      (if (null b-url)
          (echo "Buffer has no url")
          (progn
            (screenshot-selection image-path))))))


(define-command scrot (&optional (buffer (current-buffer)))
  "Take a screenshot of the current buffer"
  (screenshot-buffer buffer))
(define-command scrot-select (&optional (buffer (current-buffer)))
  "Select an area of the screen to screenshot."
  (screenshot-buffer-selection))


(define-command set-operation ()
  (let ((name (first (prompt :prompt "Enter Name" :sources 'prompter:raw-source))))
    (echo name)
    (if (uiop:string-suffix-p  name "/")
        (setq *operation* name)
        (setq *operation* (concatenate 'string name "/")))))



    


(defun reporting-handler ()
  "Automaticly report as you go"
  (screenshot-buffer))


(define-mode reporting-mode ()
  ((old-url (quri:uri ""))))
(defmethod enable ((mode reporting-mode) &key)
  (when (web-buffer-p (buffer mode))
    (hooks:add-hook (request-resource-hook (buffer mode)) #'reporting-handler)))

(defmethod disable ((mode reporting-mode) &key)
  (hooks:remove-hook (request-resource-hook (buffer mode)) #'reporting-handler))

(defmethod on-signal-load-finished ((mode reporting-mode) url)
  (declare (ignore url))
  (reporting-handler)
  nil)




(define-command summerize (&optional (buffer (current-buffer)))
  "Open a new buffer displaying the whole history tree of a buffer."

  (with-current-html-buffer (output-buffer (format nil "*History-~a*" (id buffer)))
                           'nyxt/history-tree-mode:history-tree-mode
    (with-data-unsafe (history (history-path buffer))
      (let* (
             (mode (find-submode output-buffer 'nyxt/history-tree-mode:history-tree-mode))
             (tree `(:ul ,(htree:map-owned-tree
                           #'(lambda (node)
                               `(:li
                                 (:a :href ,(render-url (url (htree:data node)))
                                     ,(let ((title (title-or-fallback (htree:data node))))
                                        (if (eq node (htree:current-owner-node history))
                                            `(:b ,title)
                                            title)))))
                           history
                           :include-root t
                           :collect-function #'(lambda (a b) `(,@a ,(when b `(:ul ,@b))))))))
        (markup:markup
         (:body (:h1 "History")
                (:style (style output-buffer))
                (:style (style mode))
                (:div (markup:raw
                       (markup:markup*
                        tree)))))))))
