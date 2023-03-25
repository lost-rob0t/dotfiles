;;; pwnagotchi.el --- Scripts to manage your pwnagotchi -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <nsaspy@airmail.cc>
;; Maintainer:  <nsaspy@airmail.cc>
;; Created: March 25, 2023
;; Modified: March 25, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/pwnagotchi
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Scripts to manage your pwnagotchi
;;
;;; Code:
(require 'f)
(defcustom pwnagotchi-dir "~/pwnagotchi"
  "Directory for where to store your pwnagotchi pcaps"
  :type 'string)
(defcustom pwnagotchi-daily-dir "~/pwnagotchi/daily"
  "Directory for where to store your pwnagotchi pcaps"
  :type 'string)


(defcustom pwnagotchi-backup-brain t
  "If not nil backup the brain")

(defcustom pwnagotchi-list ()
  "List of pwnagotchi ip addresses with ssh root acess.")

(defun pwnagotchi-make-daily-dir ()
  "Make a dir based on todays date."
  (let* ((date (format-time-string "%Y-%m-%d"))
         (dir (f-full (f-join pwnagotchi-dir pwnagotchi-daily-dir date))))
    (if (not (file-exists-p dir))
        (f-mkdir dir)))
  )

(defun pwnagotchi-remote-backup (ip)
  "Create a tar.gz archive of REMOTE-DIR on the remote host with file name TAR-FILE."
  (let ((date (format-time-string "%Y-%m-%d"))
        (default-directory (format "/ssh:%s:/root/" ip)))
    (shell-command-to-string (format "tar -czf backup-%s.tar.gz handshakes brain.nn " date))))
(defun pwnagotchi-clear (ip)
  "Clear out the handshakes"
  (let (
        (default-directory (format "/ssh:%s:/root/" ip)))
    (shell-command-to-string "rm handshakes -rf && mkdir handshakes")))


(defun pwnagotchi-get-daily-dir ()
  "get a dir based on todays date."
  (let* ((date (format-time-string "%Y-%m-%d")))
         (f-full (f-join pwnagotchi-dir pwnagotchi-daily-dir date))))
(defun pwnagotchi-get-handshakes (ip)
  "Returns the path to the handshakes in TRAMP format"
  (format "/ssh:%s:/root/handshakes" ip))
(defun pwnagotchi-get-brain (ip)
  "Returns the path to the handshakes in TRAMP format"
  (format "/ssh:%s:/root/brain.nn" ip))
(defun pwnagotchi-backup ()
  "Backup the pwnagotchi files to pwnagotchi-dir."
  (interactive)
  (pwnagotchi-make-daily-dir)
  (dolist (ip pwnagotchi-list)
    (message ip)
    (pwnagotchi-remote-backup ip)
    (f-copy-contents (pwnagotchi-get-handshakes ip) (pwnagotchi-get-daily-dir))
    (pwnagotchi-clear ip)
))
(defun pwnagotchi-convert-hashcat-all ()
  "Convert all pcaps in pwnagotchi dirs to hashcat format."
  )
(provide 'pwnagotchi)
;;; pwnagotchi.el ends here
