;;; pwnagotchi2.el --- Interact with your pwnagotchi and more! -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: April 09, 2024
;; Modified: April 09, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lost-rob0t/pwnagotchi
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Interact with your pwnagotchi and more!
;;
;;
;;; Code:

(defcustom pwnagotchi-dir "~/pwnagotchi"
  "Directory for where to store your pwnagotchi pcaps"
  :type 'string)
(defcustom pwnagotchi-handshakes-dir "~/pwnagotchi/hs"
  "Directory for where to store your pwnagotchi pcaps"
  :type 'string)

(defcustom pwnagotchi-hashes-dir "~/pwnagotchi/wordlists"
  "Directory for where to store wordlists"
  :type 'string)

(defcustom pwnagotchi-drive-dir "~/pwnagotchi/mount"
  "Directorty for where the cd card of the pwnagotchi will be mounted")

(defcustom pwnagotchi-backup-brain t
  "When set to non nil, backup the brain.")

(defcustom pwnagotchi-tramp-list ()
  "List of remote pwnagotchi ip addresses with ssh access." :type 'list)

(defcustom pwnagotchi-wigle-db ""
  "Path to the widle db file."
  :type 'string)

(defvar pwnagotchi-hcxpcapng-path (executable-find "hcxpcapngtool")
  "The path to the hcxpcapng tool. Used to convert pcap files.")

(defvar pwnagotchi-hashcat-path (executable-find "hashcat")
  "The path to Hashcat.
Used to crack handshakes.")




(provide 'pwnagotchi2)
;;; pwnagotchi2.el ends here
