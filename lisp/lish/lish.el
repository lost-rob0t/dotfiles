;;; lish.el --- Mode for using lish shell (common lisp) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <https://github.com/unseen>
;; Maintainer:  <unseen@hunter-02>
;; Created: June 03, 2023
;; Modified: June 03, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/lish
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Mode for using lish shell (common lisp)
;;
;;; Code:

;;
(defun lish-vterm ()
  "Start a new instance of the Lish shell using vterm."
  (interactive)
  (require 'vterm)
  (let ((buffer-name "*Lish*"))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (vterm-mode)
        (vterm-send-string "lish")
        (vterm-send-return)))
    (switch-to-buffer buffer-name)))

(add-to-list 'auto-mode-alist '("\\.lish\\'" . lish-vterm))

(add-to-list 'auto-mode-alist '("\\.lish\\'" . lish-mode))
(provide 'lish)
;;; lish.el ends here
