;;; reader-check.el --- deterministic Emacs Lisp reader fixture gate -*- lexical-binding: t; -*-

(let ((path (getenv "ZARA_ELISP_FIXTURE")))
  (unless (and path (file-readable-p path))
    (message "Missing readable ZARA_ELISP_FIXTURE")
    (kill-emacs 3))
  (with-temp-buffer
    (insert-file-contents path)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (condition-case err
        (progn
          (while (< (point) (point-max))
            (skip-chars-forward " \t\r\n")
            (forward-comment (buffer-size))
            (skip-chars-forward " \t\r\n")
            (when (< (point) (point-max))
              (read (current-buffer))))
          (kill-emacs 0))
      (error
       (message "Emacs Lisp reader rejected fixture: %S" err)
       (kill-emacs 2)))))
