;;; templates.el --- Create a cookie cutter template -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <nsaspy@airmail.cc>
;; Maintainer:  <nsaspy@airmail.cc>
;; Created: October 04, 2023
;; Modified: October 04, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lost-rob0t/templates
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Create a cookie cutter template
;;
;;; Code:
(require 'f)
(require 'f)

(defcustom templates-dir (f-expand "~/Templates/emacs/")
  "Dir to store templates in.")

(defun template-get (name)
  (f-full (f-join (f-expand templates-dir) name)))

(defun template-new (&optional name)
  "Create a new template."
  (interactive)
  (unless name
    (setq name (read-string "Enter template name: ")))
  (let ((dir (f-join (f-expand templates-dir) name)))
    (f-mkdir dir)
    (find-file dir)))

(defun template-here (&optional name dest)
  "Create a new dir based on template NAME at DEST."
  (interactive)
  (unless name
    (setq name (read-string "Enter template Name: ")))
  (unless dest
    (setq dest (read-directory-name "Enter Dest: ")))
  (f-mkdir dest)
  (f-copy-contents (template-get name) dest))


(provide 'templates)
;;; templates.el ends here
