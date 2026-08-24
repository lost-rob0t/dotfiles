;;; qtile-ui.el --- shared Qtile dropdown frame lifecycle -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar qtile-ui-frame-registry nil
  "Alist of stable Qtile popup IDs and their live Emacs frames.")

(defconst qtile-ui-error-buffer-name "*Qtile Errors*"
  "Buffer receiving errors raised while rendering Qtile popups.")

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

(defun qtile-ui--frame-parameters (popup-id geometry _minibuffer)
  (let* ((title (format "qtile-%s" popup-id))
         (background (face-attribute 'default :background nil nil))
         (foreground (face-attribute 'default :foreground nil nil))
         (parameters `((name . ,title)
                       (title . ,title)
                       (left . ,(qtile-ui-param 'left geometry))
                       (top . ,(qtile-ui-param 'top geometry))
                       (user-position . t)
                       (width . 80)
                       (height . 24)
                       ;; Give every popup its own minibuffer.  A nil value
                       ;; makes Emacs expose the daemon's full-size
                       ;; *Minibuf-0* frame to the window manager.
                       (minibuffer . t)
                       (fullscreen . nil)
                       (maximized . nil)
                       (menu-bar-lines . 0)
                       (tool-bar-lines . 0)
                       (vertical-scroll-bars . nil)
                       (horizontal-scroll-bars . nil)
                       (internal-border-width . 8)
                       (undecorated . t))))
    (when (qtile-ui--usable-color-p background)
      (push `(background-color . ,background) parameters))
    (when (qtile-ui--usable-color-p foreground)
      (push `(foreground-color . ,foreground) parameters))
    parameters))

(defun qtile-ui--usable-color-p (color)
  "Return non-nil when COLOR is a concrete face color, not an unspecified sentinel."
  (and (stringp color)
       (not (member color '("unspecified" "unspecified-bg" "unspecified-fg")))))

(defun qtile-ui--set-pixel-size (frame geometry)
  (let ((width (qtile-ui-param 'width geometry))
        (height (qtile-ui-param 'height geometry)))
    (when (and (numberp width) (numberp height))
      (condition-case nil
          (set-frame-size frame width height t)
        (error nil)))))

(defun qtile-ui--error-message (error)
  "Return a safe display string for ERROR."
  (condition-case nil
      (error-message-string error)
    (error (format "%s" error))))

(defun qtile-ui--log-error (popup-id error)
  "Append a timestamped popup ERROR to the shared Emacs error buffer."
  (with-current-buffer (get-buffer-create qtile-ui-error-buffer-name)
    (goto-char (point-max))
    (insert (format "%s [%s] %s\n"
                    (format-time-string "%Y-%m-%d %H:%M:%S")
                    popup-id
                    (qtile-ui--error-message error)))))

(defun qtile-ui-show-errors ()
  "Display the shared Qtile popup error buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create qtile-ui-error-buffer-name)))

(defun qtile-ui--header-line ()
  "Return the compact top line shared by Qtile popup buffers."
  (let* ((popup-id (frame-parameter nil 'qtile-ui-popup-id))
         (title (if popup-id (format "Qtile %s" popup-id) "Qtile")))
    (list (propertize (format " %s " title) 'face 'mode-line)
          (propertize "  q/Escape close " 'face 'shadow))))

(defun qtile-ui--apply-frame-theme (frame)
  "Apply the configured Emacs theme to a newly-created daemon frame."
  (when (and (boundp 'doom-theme) (symbolp doom-theme))
    (condition-case nil
        (load-theme doom-theme t)
      (error nil)))
  (with-selected-frame frame
    (dolist (spec '((:background . background-color)
                    (:foreground . foreground-color)))
      (let ((color (face-attribute 'default (car spec) frame t)))
        (when (stringp color)
          (set-frame-parameter frame (cdr spec) color))))))

(defun qtile-ui--make-frame (popup-id geometry minibuffer params)
  "Create POPUP-ID on the display supplied by Qtile when available."
  (let ((frame-parameters (qtile-ui--frame-parameters popup-id geometry minibuffer))
        (display (qtile-ui-param 'display params)))
    (if (and (stringp display)
             (not (string-empty-p display))
             (fboundp 'make-frame-on-display))
        (make-frame-on-display display frame-parameters)
      (make-frame frame-parameters))))

(defun qtile-ui-prepare-buffer ()
  "Apply shared dropdown buffer defaults without changing feature content."
  ;; mode-line-format is a buffer-local variable, not a frame parameter:
  ;; setting it on the frame did nothing, which is why Doom's modeline kept
  ;; rendering.  Doom additionally needs its own minor mode disabled.
  (setq-local mode-line-format nil)
  (setq-local header-line-format (qtile-ui--header-line))
  (when (fboundp 'hide-mode-line-mode)
    (hide-mode-line-mode 1))
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
    (let ((minibuffer (active-minibuffer-window)))
      (when (and minibuffer (eq (window-frame minibuffer) frame))
        (with-selected-frame frame
          (condition-case nil
              (abort-recursive-edit)
            (quit nil)))))
    (delete-frame frame t)))

(defun qtile-ui-close-current ()
  "Close the Qtile popup containing the current buffer."
  (interactive)
  (let ((popup-id (frame-parameter nil 'qtile-ui-popup-id)))
    (if popup-id
        (qtile-ui-close popup-id)
      (delete-frame (selected-frame) t))))

(defun qtile-ui--render-error (popup-id error)
  (qtile-ui--log-error popup-id error)
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
      (qtile-ui-close popup-id)
    (condition-case error
        (let* ((geometry (qtile-ui-param 'geometry params))
               (frame (qtile-ui--make-frame
                       popup-id geometry
                       (eq (qtile-ui-param 'minibuffer params) t)
                       params))
               (function (if (symbolp renderer) renderer (intern renderer))))
          (set-frame-parameter frame 'qtile-ui-popup-id popup-id)
          (push (cons popup-id frame) qtile-ui-frame-registry)
          (qtile-ui--apply-frame-theme frame)
          (qtile-ui--set-pixel-size frame geometry)
          (with-selected-frame frame
            (condition-case render-error
                (prog1 (funcall function params)
                  (when (frame-live-p frame)
                    (select-frame-set-input-focus frame)
                    (qtile-ui-bind-dismiss)))
              (error
               (qtile-ui--render-error popup-id render-error))
              (quit
               (qtile-ui-close popup-id)
               nil))))
      (error
       (qtile-ui--log-error popup-id error)
       nil))))

(provide 'qtile-ui)
;;; qtile-ui.el ends here
