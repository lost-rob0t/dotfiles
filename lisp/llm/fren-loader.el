;;; fren-loader.el --- Load the local fren runtime -*- lexical-binding: t; -*-

(defconst fren-loader-root
  (expand-file-name "~/Documents/mara/")
  "Local fren runtime checkout.")

(defun fren-loader-load ()
  "Load the local fren runtime when its checkout is available."
  (interactive)
  (let* ((emacs-directory (expand-file-name "emacs/" fren-loader-root))
         (entry (expand-file-name "mara.el" emacs-directory)))
    (when (file-readable-p entry)
      (add-to-list 'load-path emacs-directory)
      (require 'mara nil t))))

(fren-loader-load)

(provide 'fren-loader)
;;; fren-loader.el ends here
