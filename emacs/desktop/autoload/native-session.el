;;; native-session.el --- Native Doom session parity -*- lexical-binding: t; -*-

(defconst star-native-session-root
  (file-name-as-directory
   (expand-file-name
    "star-emacs/sessions/"
    (or (getenv "XDG_CACHE_HOME")
        (expand-file-name "~/.cache/"))))
  "Directory containing native desktop session snapshots.")

(defun star-native--desktop-call (directory function &rest args)
  (require 'desktop)
  (make-directory directory t)
  (let ((desktop-dirname directory)
        (desktop-path (list directory))
        (desktop-base-file-name ".desktop")
        (desktop-base-lock-name ".desktop.lock")
        (desktop-restore-eager 12)
        (desktop-load-locked-desktop t))
    (apply function args)))

(defun star-native/quicksave-session ()
  "Save the current desktop session to the native quick-session slot."
  (interactive)
  (star-native--desktop-call
   (expand-file-name "quick/" star-native-session-root)
   #'desktop-save
   (expand-file-name "quick/" star-native-session-root)
   t)
  (message "Native session saved"))

(defun star-native/quickload-session ()
  "Restore the native quick-session slot."
  (interactive)
  (star-native--desktop-call
   (expand-file-name "quick/" star-native-session-root)
   #'desktop-read
   (expand-file-name "quick/" star-native-session-root)))

(defun star-native/save-session (directory)
  "Save the current desktop session to DIRECTORY."
  (interactive
   (list (read-directory-name "Save session in: " star-native-session-root)))
  (star-native--desktop-call directory #'desktop-save directory t))

(defun star-native/load-session (directory)
  "Restore the desktop session in DIRECTORY."
  (interactive
   (list (read-directory-name "Load session from: " star-native-session-root)))
  (star-native--desktop-call directory #'desktop-read directory))

(defun star-native/kill-all-buffers ()
  "Kill all live file and user buffers in the current Emacs session."
  (interactive)
  (save-some-buffers)
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (not (string-prefix-p " " (buffer-name buffer)))
               (not (string-prefix-p "*Messages*" (buffer-name buffer))))
      (ignore-errors (kill-buffer buffer)))))

(defun star-native/restart-server ()
  "Recreate the current Emacs server socket without restarting Emacs."
  (interactive)
  (require 'server)
  (when (server-running-p server-name)
    (server-force-delete))
  (server-start)
  (message "Emacs server %s restarted" server-name))

(defun star-native/restart ()
  "Start a fresh native-profile Emacs and terminate this instance."
  (interactive)
  (let ((emacs (or (executable-find "emacs")
                   (expand-file-name invocation-name invocation-directory))))
    (unless (and emacs (file-executable-p emacs))
      (user-error "Cannot find an Emacs executable to restart"))
    (start-process
     "star-native-restart"
     nil
     emacs
     "--with-profile"
     "desktop-native")
    (save-buffers-kill-emacs)))

(defun star-native-session-init ()
  "Restore Doom's SPC q tree for native desktop Emacs."
  (interactive)
  (when (and (boundp 'star-leader-map)
             (keymapp star-leader-map))
    (dolist (binding '(("q d" star-native/restart-server "restart Emacs server")
                       ("q f" delete-frame "delete frame")
                       ("q F" star-native/kill-all-buffers "clear current frame")
                       ("q K" save-buffers-kill-emacs "kill Emacs and daemon")
                       ("q q" save-buffers-kill-terminal "quit Emacs")
                       ("q Q" evil-quit-all-with-error-code "quit without saving")
                       ("q s" star-native/quicksave-session "quick save session")
                       ("q l" star-native/quickload-session "restore quick session")
                       ("q S" star-native/save-session "save session")
                       ("q L" star-native/load-session "load session")
                       ("q r" star-native/restart "restart Emacs")
                       ("q c" star-config-sync "sync native config")))
      (define-key star-leader-map (kbd (nth 0 binding)) (nth 1 binding))
      (when (fboundp 'star-native--label-key)
        (star-native--label-key (nth 0 binding) (nth 2 binding))))))

(add-hook 'after-init-hook #'star-native-session-init t)

(provide 'native-session)
;;; native-session.el ends here
