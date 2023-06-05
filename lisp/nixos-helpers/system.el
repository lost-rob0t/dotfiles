;;; system.el --- Manage emacs for multiple systems -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <https://github.com/unseen>
;; Maintainer:  <unseen@hunter-02>
;; Created: June 04, 2023
;; Modified: June 04, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/system
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Manage emacs for multiple systems
;;
;;; Code:

(defun system-load-platform-name (path)
  "load the paatform name from file"
  (with-temp-buffer
    (insert-file-contents path)
    (string-replace "\n" "" (buffer-string))))

(defcustom system-name (system-load-platform-name (expand-file-name "~/.platform"))
  "The current system name.
This is used to only load elisp for that system.")

(defmacro with-system (system &rest body)
  `(when (string= ,system system-name)
     ,@body))

(provide 'system)
;;; system.el ends here
