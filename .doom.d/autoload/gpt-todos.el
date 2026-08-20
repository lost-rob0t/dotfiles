;;; gpt-todos.el -*- lexical-binding: t; -*-

(defcustom gpt-todos-sync-script
  (expand-file-name "~/.local/share/gpt-todos/bin/sync")
  "Path to the gpt-todos sync script."
  :type 'file)

;;;###autoload
(defun gpt-todos-sync ()
  "Run gpt-todos sync asynchronously in a hidden buffer."
  (interactive)
  (let* ((buffer (get-buffer-create " *gpt-todos-sync*"))
         (proc (start-process "gpt-todos-sync" buffer
                              "/usr/bin/env" "bash" gpt-todos-sync-script)))
    (set-process-sentinel
     proc
     (lambda (p _event)
       (when (memq (process-status p) '(exit signal))
         (if (= 0 (process-exit-status p))
             (message "gpt-todos sync complete")
           (message "gpt-todos sync failed; see %s"
                    (buffer-name (process-buffer p)))))))
    (message "gpt-todos sync started")))
