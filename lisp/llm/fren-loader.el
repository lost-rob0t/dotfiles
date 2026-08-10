;;; fren-loader.el --- Load the local fren runtime -*- lexical-binding: t; -*-

(defconst fren-loader-root
  (expand-file-name "~/Documents/mara/")
  "Local fren runtime checkout.")

(defconst fren-loader-emacs-directory
  (expand-file-name "emacs/" fren-loader-root)
  "Directory containing the local fren Emacs runtime.")

(defun fren-loader-load ()
  "Load the local fren runtime through its Mara entry point.

The runtime directory is added to `load-path' before loading `mara.el' so
Mara's internal `require' forms resolve their dependencies themselves.  Do
not load every file in the runtime directory individually."
  (interactive)
  (let ((entry (expand-file-name "mara.el" fren-loader-emacs-directory)))
    (cond
     ((not (file-directory-p fren-loader-emacs-directory))
      (message "fren-loader: runtime directory is absent: %s"
               fren-loader-emacs-directory)
      nil)
     ((not (file-readable-p entry))
      (message "fren-loader: Mara entry point is unreadable: %s" entry)
      nil)
     (t
      (add-to-list 'load-path fren-loader-emacs-directory)
      ;; Load only the entry feature.  `mara.el' owns the dependency graph.
      (require 'mara)
      (when (called-interactively-p 'interactive)
        (message "fren-loader: loaded Mara from %s" entry))
      t))))

(fren-loader-load)

(provide 'fren-loader)
;;; fren-loader.el ends here
