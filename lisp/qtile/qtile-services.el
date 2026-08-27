;;; qtile-services.el --- small systemd user-service dashboard -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'qtile-ui)
(require 'qtile-ui-org)

(defvar-local qtile-services-lines nil)
(defvar-local qtile-services-selected nil)
(defvar-local qtile-services--process nil)
(defvar-local qtile-services--confirm-restart nil)

(defun qtile-services--unit (line)
  (car (split-string line nil t)))

(defun qtile-services--select (unit)
  (setq qtile-services-selected unit)
  (setq qtile-services--confirm-restart nil)
  (qtile-services-render))

(defun qtile-services-restart-selected ()
  "Restart the selected user service after a second explicit activation."
  (interactive)
  (cond
   ((not qtile-services-selected)
    (setq qtile-services--confirm-restart nil)
    (qtile-services-render))
   ((and qtile-services--confirm-restart)
    (setq qtile-services--confirm-restart nil)
    (start-process "qtile-service-restart" nil "systemctl" "--user"
                   "restart" qtile-services-selected)
    (run-at-time 0.4 nil #'qtile-services-refresh))
   (t
    (setq qtile-services--confirm-restart t)
    (qtile-services-render))))

(defun qtile-services-render ()
  "Render current user-service status and safe restart controls."
  (when (derived-mode-p 'qtile-services-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (qtile-ui-org-heading "SERVICES")
      (qtile-ui-org-muted "User services from systemd\n\n")
      (dolist (line qtile-services-lines)
        (let ((unit (qtile-services--unit line)))
          (insert-button
           (if (equal unit qtile-services-selected)
               (format "[x] %s" line)
             (format "[ ] %s" line))
           'action (lambda (_button) (qtile-services--select unit))
           'follow-link t
           'face (if (equal unit qtile-services-selected)
                     'qtile-ui-org-selected
                   'button))
          (insert "\n")))
      (qtile-ui-org-separator)
      (qtile-ui-org-button "[ Refresh ]" (lambda (_button) (qtile-services-refresh)))
      (insert " ")
      (qtile-ui-org-button
       (if qtile-services--confirm-restart
           "[ Confirm Restart Selected ]"
         "[ Restart Selected ]")
       (lambda (_button) (qtile-services-restart-selected)))
      (insert "\n")
      (qtile-ui-org-muted
       (if qtile-services-selected
           (format "Selected: %s\n" qtile-services-selected)
         "Select a service before restarting it.\n"))
      (goto-char (point-min)))))

(defun qtile-services--process-sentinel (process _event target raw)
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p target))
    (with-current-buffer target
      (when (= (process-exit-status process) 0)
        (with-current-buffer raw
          (setq qtile-services-lines
                (split-string (buffer-string) "\n" t)))
        (qtile-services-render)))
    (when (buffer-live-p raw)
      (kill-buffer raw))))

(defun qtile-services-refresh ()
  "Refresh user-service status asynchronously."
  (interactive)
  (when (process-live-p qtile-services--process)
    (delete-process qtile-services--process))
  (let* ((target (current-buffer))
         (raw (generate-new-buffer " *qtile-services*")))
    (setq qtile-services--process
          (make-process
           :name "qtile-services"
           :buffer raw
           :command '("systemctl" "--user" "list-units" "--type=service"
                      "--state=running" "--no-legend" "--plain")
           :noquery t
           :sentinel (lambda (process event)
                       (qtile-services--process-sentinel
                        process event target raw))))))

(define-derived-mode qtile-services-mode special-mode "Qtile-Services"
  "Systemd user-service status dashboard."
  (qtile-ui-prepare-buffer)
  (qtile-ui-bind-dismiss)
  (setq-local buffer-read-only t))

(defun qtile-services-open (_params)
  "Open or reuse the service dashboard from a Qtile popup."
  (interactive)
  (let ((buffer (get-buffer-create "*Qtile Services*")))
    (switch-to-buffer buffer)
    (unless (derived-mode-p 'qtile-services-mode)
      (qtile-services-mode))
    (qtile-services-refresh)
    buffer))

(provide 'qtile-services)
;;; qtile-services.el ends here
