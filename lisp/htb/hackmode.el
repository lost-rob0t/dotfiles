;;; htb.el --- Hack The box utils -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <https://github.com/unseen>
;; Maintainer:  <unseen@hunter-02>
;; Created: May 06, 2023
;; Modified: May 06, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/htb
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Hack The box utils
;;
;;; Code:

(defvar hackmode-wordlist-dir "~/wordlists/")
(defun hackmode-add-host ()
  "Add a Host to /etc/hosts"
  (interactive)
  (let ((hostname (read-string "Enter Host name: "))
        (ip (read-string "Enter IP: ")))
    (append-to-file (format "%s\t%s\n" ip hostname) nil "/sudo::/etc/hosts")))


(defun hackmode-kill-wordlist ()
  "Copy the path of a wordlist to the kill ring"
  (interactive)
  (kill-new (f-expand (read-file-name "Select Wordlist: " (f-expand hackmode-wordlist-dir)))))

(provide 'htb)
;;; htb.el ends here
