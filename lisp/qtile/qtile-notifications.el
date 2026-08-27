;;; qtile-notifications.el --- Emacs notification dashboard for Qtile -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'json)
(require 'qtile-ui)
(require 'qtile-ui-org)

(defgroup qtile-notifications nil
  "Dunst history dashboard opened from a Qtile bar."
  :group 'external)

(defcustom qtile-notifications-history-command
  (expand-file-name "~/.config/qtile/scripts/dunst_history.py")
  "Executable Dunst history adapter."
  :type 'file)

(defcustom qtile-notifications-settings-command
  (expand-file-name "~/.config/qtile/ui_settings.py")
  "Executable Qtile UI settings adapter."
  :type 'file)

(defface qtile-notifications-low
  '((t (:inherit shadow)))
  "Face for low urgency notifications.")
(defface qtile-notifications-normal
  '((t (:inherit default)))
  "Face for normal urgency notifications.")
(defface qtile-notifications-critical
  '((t (:inherit error :weight bold)))
  "Face for critical urgency notifications.")
(defface qtile-notifications-app
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for notification application names.")
(defface qtile-notifications-summary
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for notification summaries.")

(defvar-local qtile-notifications-entries nil)
(defvar-local qtile-notifications-paused nil)
(defvar-local qtile-notifications-error nil)
(defvar-local qtile-notifications-backend "dmenu")
(defvar-local qtile-notifications-selected-id nil)
(defvar-local qtile-notifications--process nil)
(defvar-local qtile-notifications--confirm-clear nil)
(defvar-local qtile-notifications--params nil)
(defvar-local qtile-notifications--settings-error nil)

(defun qtile-notifications--get (key object)
  (or (alist-get key object nil nil #'equal)
      (alist-get (symbol-name key) object nil nil #'equal)
      (alist-get (intern (symbol-name key)) object nil nil #'eq)))

(defun qtile-notifications--urgency-face (urgency)
  (pcase urgency
    ("critical" 'qtile-notifications-critical)
    ("low" 'qtile-notifications-low)
    (_ 'qtile-notifications-normal)))

(defun qtile-notifications--entry-action (id)
  (lambda (_button)
    (setq qtile-notifications-selected-id id)
    (start-process "qtile-dunst-replay" nil "dunstctl" "history-pop" (number-to-string id))
    (qtile-notifications-render)))

(defun qtile-notifications--set-backend (backend)
  (setq qtile-notifications--settings-error nil)
  (let ((target (current-buffer)))
    (make-process
     :name "qtile-ui-settings"
     :command (list "python3" qtile-notifications-settings-command "--set" backend)
     :noquery t
     :sentinel
     (lambda (process _event)
       (when (and (memq (process-status process) '(exit signal))
                  (buffer-live-p target))
         (with-current-buffer target
           (if (= (process-exit-status process) 0)
               (setq qtile-notifications-backend backend)
             (setq qtile-notifications--settings-error
                   "could not persist notification_ui"))
           (qtile-notifications-render)))))))

(defun qtile-notifications-toggle-dnd ()
  "Toggle Dunst pause state and refresh the dashboard."
  (interactive)
  (start-process "qtile-dunst-dnd" nil "dunstctl" "set-paused" "toggle")
  (run-at-time 0.2 nil #'qtile-notifications-refresh))

(defun qtile-notifications-clear-history ()
  "Require a second explicit activation before clearing all Dunst history."
  (interactive)
  (if qtile-notifications--confirm-clear
      (progn
        (setq qtile-notifications--confirm-clear nil)
        (start-process "qtile-dunst-clear" nil "dunstctl" "history-clear")
        (run-at-time 0.2 nil #'qtile-notifications-refresh))
    (setq qtile-notifications--confirm-clear t)
    (qtile-notifications-render)))

(defun qtile-notifications--render-entry (entry)
  (let* ((id (qtile-notifications--get 'id entry))
         (urgency (qtile-notifications--get 'urgency entry))
         (app (or (qtile-notifications--get 'app entry) "notification"))
         (summary (or (qtile-notifications--get 'summary entry) "(no summary)"))
         (body (or (qtile-notifications--get 'body entry) ""))
         (timestamp (or (qtile-notifications--get 'timestamp entry) ""))
         (start (point)))
    (insert (if (equal urgency "critical") "󰀪 " "󰂚 "))
    (insert (propertize (format "%s  " timestamp) 'face 'qtile-ui-org-muted))
    (insert (propertize (format "%s\n" app) 'face 'qtile-notifications-app))
    (insert-button (format "  %s" summary)
                   'action (qtile-notifications--entry-action id)
                   'follow-link t
                   'face (if (equal id qtile-notifications-selected-id)
                             'qtile-ui-org-selected
                           'qtile-notifications-summary))
    (insert "\n")
    (unless (string-empty-p body)
      (insert (propertize (format "    %s\n" body)
                          'face (qtile-notifications--urgency-face urgency))))
    (add-text-properties start (point) `(qtile-notification-id ,id))))

(defun qtile-notifications-render ()
  "Render every retained normalized Dunst history record."
  (when (derived-mode-p 'qtile-notifications-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (qtile-ui-org-heading "NOTIFICATIONS")
      (insert (propertize (format "DND: %s  |  Backend: %s\n"
                                  (if qtile-notifications-paused "ON" "OFF")
                                  qtile-notifications-backend)
                          'face (if qtile-notifications-paused
                                    'qtile-ui-org-warning
                                  'qtile-ui-org-success)))
      (when qtile-notifications-error
        (insert (propertize (format "Unavailable: %s\n" qtile-notifications-error)
                            'face 'qtile-notifications-critical)))
      (when qtile-notifications--settings-error
        (insert (propertize
                 (format "Settings unavailable: %s\n"
                         qtile-notifications--settings-error)
                 'face 'qtile-notifications-critical)))
      (qtile-ui-org-button "[ Dmenu ]"
                           (lambda (_button) (qtile-notifications--set-backend "dmenu")))
      (insert " ")
      (qtile-ui-org-button "[ Emacs ]"
                           (lambda (_button) (qtile-notifications--set-backend "emacs")))
      (qtile-ui-org-separator)
      (dolist (entry qtile-notifications-entries)
        (qtile-notifications--render-entry entry)
        (insert "\n"))
      (qtile-ui-org-separator)
      (qtile-ui-org-button "[ Refresh ]" (lambda (_button) (qtile-notifications-refresh)))
      (insert " ")
      (qtile-ui-org-button "[ Toggle DND ]" (lambda (_button) (qtile-notifications-toggle-dnd)))
      (insert " ")
      (qtile-ui-org-button
       (if qtile-notifications--confirm-clear
           "[ Confirm Clear History ]"
         "[ Clear History ]")
       (lambda (_button) (qtile-notifications-clear-history)))
      (insert "\n")
      (qtile-ui-org-muted (format "History: %d notifications\n"
                                  (length qtile-notifications-entries)))
      (goto-char (point-min)))))

(defun qtile-notifications--process-sentinel (process _event target raw)
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p target))
    (with-current-buffer target
      (if (= (process-exit-status process) 0)
          (with-current-buffer raw
            (goto-char (point-min))
            (condition-case nil
                (let ((payload (json-read-from-string (buffer-string))))
                  (setq qtile-notifications-entries
                        (or (qtile-notifications--get 'entries payload) nil))
                  (setq qtile-notifications-paused
                        (eq (qtile-notifications--get 'paused payload) t))
                  (setq qtile-notifications-error
                        (qtile-notifications--get 'error payload)))
              (error
               (setq qtile-notifications-entries nil)
               (setq qtile-notifications-error "invalid adapter response"))))
        (setq qtile-notifications-error "Dunst history adapter failed"))
      (qtile-notifications-render))
    (when (buffer-live-p raw)
      (kill-buffer raw))))

(defun qtile-notifications-refresh ()
  "Request the complete normalized Dunst history without blocking Qtile."
  (interactive)
  (when (process-live-p qtile-notifications--process)
    (delete-process qtile-notifications--process))
  (let* ((target (current-buffer))
         (raw (generate-new-buffer " *qtile-dunst-history*"))
         (command (list "python3" qtile-notifications-history-command "--json")))
    (setq qtile-notifications--process
          (make-process
           :name "qtile-dunst-history"
           :buffer raw
           :command command
           :noquery t
           :sentinel (lambda (process event)
                       (qtile-notifications--process-sentinel
                        process event target raw))))))

(define-derived-mode qtile-notifications-mode special-mode "Qtile-Notifications"
  "Read-only modern Dunst history dashboard."
  (qtile-ui-prepare-buffer)
  (qtile-ui-bind-dismiss)
  (setq-local buffer-read-only t))

(defun qtile-notifications-open (params)
  "Open or refresh the shared Emacs notification dashboard."
  (interactive)
  (let* ((args (qtile-ui-args params))
         (buffer (get-buffer-create "*Qtile Notifications*")))
    (switch-to-buffer buffer)
    (unless (derived-mode-p 'qtile-notifications-mode)
      (qtile-notifications-mode))
    (setq qtile-notifications--params params)
    (setq qtile-notifications-error nil)
    (setq qtile-notifications--settings-error nil)
    (setq qtile-notifications-backend
          (or (qtile-notifications--get 'backend args) "dmenu"))
    (setq qtile-notifications-history-command
          (or (qtile-notifications--get 'history-command args)
              qtile-notifications-history-command))
    (qtile-notifications-refresh)
    buffer))

(provide 'qtile-notifications)
;;; qtile-notifications.el ends here
