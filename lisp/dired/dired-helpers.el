;;; dired-helpers.el --- Dired stuff -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: July 07, 2023
;; Modified: July 07, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lost-rob0t/dired-helpers
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Dired stuff
;;
;;; Code:

(defun nsaspy/merge-dirs (local-dir remote-dir)
  "Merge the contents of LOCAL-DIR with REMOTE-DIR."
  (dolist (entry (directory-files local-dir t))
    (let ((file-name (file-name-nondirectory entry)))
      (unless (string-match-p "^\\.\\.?$" file-name)
        (let ((remote-file (concat remote-dir "/" file-name)))
          (if (file-directory-p entry)
              ;; If the entry is a directory, recursively merge its contents
              (unless (file-exists-p remote-file)
                (make-directory remote-file t))
              (nsaspy/merge-dirs entry remote-file)
            ;; If the entry is a file, copy it to the remote directory
            (if (file-exists-p remote-file)
                (message "Skipping existing remote file: %s" remote-file)
              (copy-file entry remote-file t t))))))))

(provide 'dired-helpers)
;;; dired-helpers.el ends here
