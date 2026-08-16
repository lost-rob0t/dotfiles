;;; nsa-reload-lisp.el --- Reload local Lisp utilities -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar nsa/lisp-directory (expand-file-name "~/.dotfiles/lisp/")
  "Root directory containing local Emacs Lisp utilities.")

(defun nsa/reload-lisp--root ()
  "Return the canonical local Lisp root directory."
  (file-name-as-directory
   (file-truename (expand-file-name nsa/lisp-directory))))

(defun nsa/reload-lisp--refresh-load-path (root)
  "Ensure ROOT and its immediate subdirectories are on `load-path'."
  (dolist (directory (cons root (directory-files root t "^[^.]" t)))
    (when (file-directory-p directory)
      (add-to-list 'load-path directory))))

(defun nsa/reload-lisp--loaded-files (root)
  "Return loaded Emacs Lisp files below ROOT in original load order."
  (let (files)
    (dolist (entry load-history)
      (let ((file (car-safe entry)))
        (when (and (stringp file)
                   (file-exists-p file)
                   (file-in-directory-p (file-truename file) root))
          (push file files))))
    (delete-dups files)))

;;;###autoload
(defun nsa/reload-lisp-directory (&optional all)
  "Reload local Emacs Lisp below `nsa/lisp-directory'.

By default reload only files that are already present in `load-history'.
With prefix argument ALL, load every .el file recursively.  Refresh
`load-path' first so newly added immediate subdirectories are visible."
  (interactive "P")
  (let* ((root (nsa/reload-lisp--root))
         (_ (nsa/reload-lisp--refresh-load-path root))
         (files (if all
                    (sort (directory-files-recursively root "\\.el\\'")
                          #'string<)
                  (nsa/reload-lisp--loaded-files root)))
         (loaded 0)
         failures)
    (dolist (file files)
      (condition-case err
          (progn
            (load file nil 'nomessage)
            (setq loaded (1+ loaded)))
        (error
         (push (cons file (error-message-string err)) failures))))
    (setq failures (nreverse failures))
    (dolist (failure failures)
      (display-warning
       'nsa/reload-lisp
       (format "Failed to reload %s: %s" (car failure) (cdr failure))
       :warning))
    (message "Reloaded %d local Lisp file%s%s"
             loaded
             (if (= loaded 1) "" "s")
             (if failures
                 (format "; %d failed" (length failures))
               ""))
    (list :loaded loaded :failed failures)))

(provide 'nsa-reload-lisp)
;;; nsa-reload-lisp.el ends here
