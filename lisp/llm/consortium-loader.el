;;; consortium-loader.el --- Lazy Consortium Emacs integration -*- lexical-binding: t; -*-

(defgroup nsa/consortium nil
  "Lazy Emacs entrypoints for the Consortium ADARD harness."
  :group 'tools)

(defcustom nsa/consortium-root
  (expand-file-name "~/Documents/Projects/consortium")
  "Checkout containing Consortium's Emacs client and Nix flake."
  :type 'directory
  :group 'nsa/consortium)

(defvar nsa/consortium-current-run-id nil
  "Most recently admitted Consortium run in this Emacs session.")

(defvar nsa/consortium--event-hook-installed nil)

(defun nsa/consortium--command ()
  "Return the command used to start the Consortium daemon."
  (let ((binary (expand-file-name "result/bin/consortium"
                                 nsa/consortium-root)))
    (if (file-executable-p binary)
        (list binary "daemon")
      (let ((nix (or (executable-find "nix")
                     "/run/current-system/sw/bin/nix")))
        (unless (file-executable-p nix)
          (error "Consortium package is not built and Nix is unavailable"))
        (list nix "run" nsa/consortium-root "--" "daemon")))))

(defun nsa/consortium--event (event)
  "Display meaningful Consortium EVENT transitions in the minibuffer."
  (when (equal (alist-get 'run_id event) nsa/consortium-current-run-id)
    (let ((type (alist-get 'type event)))
      (when (member type '("phase.started" "phase.completed"
                           "run.completed" "run.failed" "run.cancelled"))
        (message "Consortium %s: %s"
                 (or (alist-get 'phase event) "run")
                 type)))))

(defun nsa/consortium--ensure ()
  "Load Consortium lazily and configure its daemon command."
  (let ((client-directory (expand-file-name "emacs" nsa/consortium-root)))
    (unless (file-readable-p (expand-file-name "consortium.el"
                                               client-directory))
      (error "Consortium Emacs client is not readable: %s" client-directory))
    (add-to-list 'load-path client-directory)
    (require 'consortium)
    (let ((command (nsa/consortium--command)))
      (setq consortium-program (car command)
            consortium-program-arguments (cdr command))))
  (unless nsa/consortium--event-hook-installed
    (add-hook 'consortium-event-hook #'nsa/consortium--event)
    (setq nsa/consortium--event-hook-installed t)))

(defun nsa/consortium-connect ()
  "Connect to the Consortium daemon without blocking Emacs."
  (interactive)
  (nsa/consortium--ensure)
  (consortium-connect)
  (message "Consortium daemon connected"))

(defun nsa/consortium-start (task)
  "Start a Consortium TASK asynchronously."
  (interactive (list (read-string "Consortium task: ")))
  (nsa/consortium--ensure)
  (consortium-start-task
   task
   nil
   (lambda (run-id error)
     (if error
         (message "Consortium start failed: %S" error)
       (setq nsa/consortium-current-run-id run-id)
       (message "Consortium run admitted: %s" run-id)))))

(defun nsa/consortium-status ()
  "Show status for the current or selected Consortium run."
  (interactive)
  (nsa/consortium--ensure)
  (let ((run-id (or nsa/consortium-current-run-id
                    (read-string "Consortium run ID: "))))
    (consortium-status
     run-id
     (lambda (status error)
       (if error
           (message "Consortium status failed: %S" error)
         (message "Consortium %s: %s (%s)"
                  run-id
                  (alist-get 'state status)
                  (or (alist-get 'phase status) "no phase")))))))

(defun nsa/consortium-cancel ()
  "Cancel the current or selected Consortium run."
  (interactive)
  (nsa/consortium--ensure)
  (let ((run-id (or nsa/consortium-current-run-id
                    (read-string "Cancel Consortium run ID: "))))
    (consortium-cancel
     run-id
     (lambda (result error)
       (if error
           (message "Consortium cancellation failed: %S" error)
         (message "Consortium %s: %s"
                  run-id (alist-get 'status result)))))))

(defun nsa/consortium-events ()
  "Replay events for the current or selected Consortium run."
  (interactive)
  (nsa/consortium--ensure)
  (let ((run-id (or nsa/consortium-current-run-id
                    (read-string "Consortium run ID: "))))
    (consortium-events
     run-id
     0
     (lambda (result error)
       (if error
           (message "Consortium events failed: %S" error)
         (message "Consortium replayed %d events for %s"
                  (length (alist-get 'events result)) run-id))))))

(defun nsa/consortium-disconnect ()
  "Disconnect from the Consortium daemon."
  (interactive)
  (when (featurep 'consortium)
    (consortium-disconnect))
  (message "Consortium daemon disconnected"))

(provide 'consortium-loader)
;;; consortium-loader.el ends here
