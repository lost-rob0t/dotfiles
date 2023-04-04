;; ppackage.el --- Simple utility to create local lisp packages for emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: March 15, 2023
;; Modified: March 15, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/create-package
;; Package-Requires: ((emacs "24.3")(f "v0.20.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Simple utility to create local lisp packages for emacs
;;
;;; Code:

(require 'f)

(defcustom ppackage-path "~/lisp"
  "The Main path where your projects will live"
  :type 'string)
(defcustom ppackage-template "~/lisp/template/"
  "Path to a template directory to use when creating a new package."
  :type 'string)
(defun ppackage-list-packages ()
  "List Personal packages in ppackage-path"
  (f-entries ppackage-path))

(defun ppackage-init-package (name)
  "Create a new personal elisp package DIR with NAME.
Package will be placed inside ppackage-path."
  (if (f-dir? name)
      (message (format "personal package %s already exists!" name)))
  (progn
    (f-mkdir-full-path name)
    (f-copy-contents ppackage-template name))
  )

(defun ppackage-new ()
  "Create a new package interactively."
  (interactive)
  (let* ((name (read-string "Enter a package name: "))
         (path (f-full (f-join ppackage-path name)))
         (default-file (f-full (f-join path (format "%s.el" name))))
         )
    (ppackage-init-package path)
    (find-file default-file)))

(defun ppackage-edit-package ()
  "Edit a package."
  (interactive)
  (let* ((packages (ppackage-list-packages))
         (package (completing-read "Select package: " packages))
         (filename (completing-read "Select file: " (f-entries package nil t))))
    (find-file filename)
    ))

(provide 'ppackage)
;;; ppackage.el ends here
