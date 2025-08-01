;;; system.el --- Manage emacs for multiple systems -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author:  <https://github.com/unseen>
;; Maintainer:  <unseen@hunter-02>
;; Created: June 04, 2023
;; Modified: August 01, 2025
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
;;  this is a stub and im not sure why i added this
;;  I think it was going to be usedf to change speficic settings for certain hostnames eg for desktop or optmize for android.
;;; Code:

(defun system-load-platform-name (path)
  "load the paatform name from file"
  (with-temp-buffer
    (insert-file-contents path)
    (string-replace "\n" "" (buffer-string))))

(defmacro with-system (system &rest body)
  `(when (string= ,system system-name)
     ,@body))

(provide 'system)
;;; system.el ends here
