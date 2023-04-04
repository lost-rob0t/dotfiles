;;; hashcat-mode.el --- Manage Hashcat cracking jobs from emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: April 04, 2023
;; Modified: April 04, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/hashcat-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Manage Hashcat cracking jobs from emacs
;;
;;; Code:

(defvar hashcat-mode-executable (executable-find "hashcat"))
(defvar hashcat-mode--queue ()
  "Queue holding hashcat jobs")

(defun hashcat-mode-create-job (mode hash &optional options)
  "Create a new hashcat job")
;; TODO WORK ON THIS

(provide 'hashcat-mode)
;;; hashcat-mode.el ends here
