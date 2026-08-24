;;; qtile-ui.el --- shared Qtile dropdown frame lifecycle -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)

(defvar qtile-ui-frame-registry nil
  "Alist of stable Qtile popup IDs and their live Emacs frames.")

(defun qtile-ui--lookup (key alist)
  "Read KEY from JSON ALIST whether its keys are strings or symbols."
  (or (alist-get key alist nil nil #'equal)
      (alist-get (symbol-name key) alist nil nil #'equal)
      (alist-get (intern (symbol-name key)) alist nil nil #'eq)))

(defun qtile-ui-param (key params)
  "Return KEY from popup PARAMS, including its nested feature arguments."
  (qtile-ui--lookup key params))

(defun qtile-ui-args (params)
  "Return feature-specific arguments from popup PARAMS."
  (or (qtile-ui-param 'args params) nil))

(defun qtile-ui--live-frame (popup-id)
  (let ((frame (cdr (assoc popup-id qtile-ui-frame-registry))))
    (unless (frame-live-p frame)
      (setq frame
            (cl-find-if
             (lambda (candidate)
               (equal (frame-parameter candidate 'qtile-ui-popup-id)
                      popup-id))
             (frame-list))))
    (if (frame-live-p frame)
        (progn
          (unless (assoc popup-id qtile-ui-frame-registry)
            (push (cons popup-id frame) qtile-ui-frame-registry))
          frame)
      (setq qtile-ui-frame-registry
            (cl-remove-if (lambda (entry)
                            (equal (car entry) popup-id))
                          qtile-ui-frame-registry))
      nil)))

(defun qtile-ui--forget-frame (frame)
  (setq qtile-ui-frame-registry
        (cl-delete-if (lambda (entry) (eq (cdr entry) frame))
                      qtile-ui-frame-registry)))

(unless (memq #'qtile-ui--forget-frame delete-frame-functions)
  (add-hook 'delete-frame-functions #'qtile-ui--forget-frame))

(defun qtile-ui--frame-parameters (popup-id geometry minibuffer)
  (let ((title (format "qtile-%s" popup-id)))
    `((name . ,title)
      (title . ,title)
      (left . ,(qtile-ui-param 'left geometry))
      (top . ,(qtile-ui-param 'top geometry))
      (user-position . t)
      (width . 80)
      (height . 24)
      (minibuffer . ,(if minibuffer t nil))
      (mode-line-format . nil)
      (header-line-format . nil)
      (menu-bar-lines . 0)
      (tool-bar-lines . 0)
      (vertical-scroll-bars . nil)
      (horizontal-scroll-bars . nil)
      (internal-border-width . 8)
      (undecorated . t))))

(defun qtile-ui--set-pixel-size (frame geometry)
  (let ((width (qtile-ui-param 'width geometry))
        (height (qtile-ui-param 'height geometry)))
    (when (and (numberp width) (numberp height))
      (condition-case nil
          (set-frame-size frame width height t)
        (error nil)))))

(defun qtile-ui-prepare-buffer ()
  "Apply shared dropdown buffer defaults without changing feature content."
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local truncate-lines nil)
  (setq-local cursor-type t)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun qtile-ui-bind-dismiss ()
  "Bind the common keyboard dismissal keys in the current popup buffer."
  (local-set-key (kbd "q") #'qtile-ui-close-current)
  (local-set-key (kbd "ESC") #'qtile-ui-close-current)
  (local-set-key (kbd "<escape>") #'qtile-ui-close-current))

(defun qtile-ui-close (popup-id)
  "Close the popup identified by POPUP-ID, if it is still live."
  (interactive)
  (when-let ((frame (qtile-ui--live-frame popup-id)))
    (delete-frame frame t)))

(defun qtile-ui-close-current ()
  "Close the Qtile popup containing the current buffer."
  (interactive)
  (let ((popup-id (frame-parameter nil 'qtile-ui-popup-id)))
    (if popup-id
        (qtile-ui-close popup-id)
      (delete-frame (selected-frame) t))))

(defun qtile-ui--render-error (popup-id error)
  (let ((buffer (get-buffer-create (format "*Qtile %s*" popup-id))))
    (switch-to-buffer buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Qtile popup error\n\n" 'face 'error))
      (insert (error-message-string error)))
    (special-mode)
    (qtile-ui-prepare-buffer)
    (qtile-ui-bind-dismiss)
    buffer))

(defun qtile-ui-toggle (popup-id renderer params)
  "Toggle POPUP-ID and invoke RENDERER with structured PARAMS on open.

The caller supplies geometry computed from the triggering Qtile widget.  A
second invocation closes the registered frame, while a later invocation creates
the same stable popup identity again and reuses the feature's buffer.
"
  (interactive)
  (if-let ((frame (qtile-ui--live-frame popup-id)))
      (progn
        (select-frame-set-input-focus frame)
        (delete-frame frame t))
    (let* ((geometry (qtile-ui-param 'geometry params))
           (frame (make-frame
                   (qtile-ui--frame-parameters
                    popup-id geometry (eq (qtile-ui-param 'minibuffer params) t))))
           (function (if (symbolp renderer) renderer (intern renderer))))
      (set-frame-parameter frame 'qtile-ui-popup-id popup-id)
      (push (cons popup-id frame) qtile-ui-frame-registry)
      (qtile-ui--set-pixel-size frame geometry)
      (with-selected-frame frame
        (condition-case error
            (prog1 (funcall function params)
              (when (frame-live-p frame)
                (select-frame-set-input-focus frame)
                (qtile-ui-bind-dismiss)))
          (error
           (qtile-ui--render-error popup-id error)))))))

(provide 'qtile-ui)
;;; qtile-ui.el ends here
