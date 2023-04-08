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
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Scripts to manage your pwnagotchi
;;
;;; Code:
(require 'f)
(require 's)

(defcustom pwnagotchi-dir "~/pwnagotchi"
  "Directory for where to store your pwnagotchi pcaps"
  :type 'string)
(defcustom pwnagotchi-handshakes-dir "~/pwnagotchi/hs"
  "Directory for where to store your pwnagotchi pcaps"
  :type 'string)
(defcustom pwnagotchi-hashes-dir "~/pwnagotchi/hashes"
  "Directory for where to store your pwnagotchi pcaps"
  :type 'string)
(defcustom pwnagotchi-hashes-dir "~/pwnagotchi/wordlists"
  "Directory for where to store wordlists"
  :type 'string)


(defcustom pwnagotchi-backup-brain t
  "If not nil backup the brain")

(defcustom pwnagotchi-list ()
  "List of pwnagotchi ip addresses with ssh root access.")

(defvar pwnagotchi-hcxpcapng-path (executable-find "hcxpcapngtool")
  "The path to the hcxpcapng tool.
Used to convert pcap files")

(defvar pwnagotchi-hashcat-path (executable-find "hashcat")
  "The path to Hashcat.
Used to crack handshakes.")


(define-error 'pwnagotchi-hcxpcapngtool-parse-error "hcxpcapngtool could not parse %s")
(defun pwnagotchi-remote-backup (ip)
  "Create a tar.gz archive of REMOTE-DIR on the remote host with file name TAR-FILE."
  (let ((date (format-time-string "%Y-%m-%d"))
        (default-directory (format "/ssh:%s:/root/" ip)))
    (shell-command-to-string (format "tar -czf backup-%s.tar.gz handshakes brain.nn " date))))


(defun pwnagotchi-get-handshakes (ip)
  "Returns the path to the handshakes in TRAMP format"
  (format "/ssh:%s:/root/handshakes" ip))


(defun pwnagotchi-get-brain (ip)
  "Returns the path to the handshakes in TRAMP format"
  (format "/ssh:%s:/root/brain.nn" ip))


(defun pwnagotchi-backup ()
  "Backup the pwnagotchi files to pwnagotchi-dir."
  (interactive)
  (dolist (ip pwnagotchi-list)
    (message ip)
    (pwnagotchi-remote-backup ip)
    (f-copy-contents (pwnagotchi-get-handshakes ip) pwnagotchi-handshakes-dir)))
(defun pwnagotchi-backup-host ()
  "Backup the pwnagotchi files to pwnagotchi-dir."
  (interactive)
  (let ((ip (read-string "Enter ip: ")))
    (pwnagotchiremote-backup ip)
    (f-copy-contents (pwnagotchi-get-handshakes ip) pwnagotchi-handshakes-dir)))



(defun pwnagotchi-collect-ssid (names prefixes)
  "Sort a list of SSID's by prefix."
  (let ((prefix-alist '()) (misc-alist '()))
    (dolist (name names)
      (let ((matched-prefix nil))
        (dolist (prefix prefixes)
          (when (string-prefix-p prefix name)
            (setq matched-prefix prefix)
            (setq prefix-alist (cons (cons prefix (cons name (cdr (assoc prefix prefix-alist)))) (assq-delete-all prefix prefix-alist)))))
        (unless matched-prefix
          (setq misc-alist (cons name misc-alist)))))
    (cons (cons "misc" misc-alist) (mapcar (lambda (prefix) (cons prefix (cdr (assoc prefix prefix-alist)))) prefixes))))




(defun pwnagotchi-format-mac (string)
  "Join every two characters in STRING with SEPARATOR and return the result."

  (setq substrings '())
  (dotimes (i (/ (length string) 2))
    (let ((start (* i 2)))
      (setq substrings (append substrings (list (substring string start (+ start 2)))))))
  (mapconcat 'identity substrings ":"))


(defun pwnagotchi-convert-hcxformat (file)
  "attempt to convert a pcap file with a handshake to the hashcat format 22000."
  (when pwnagotchi-hcxpcapng-path
    (let ((hcx-buffer (get-buffer-create "*hcxpcapng*")))
      (unwind-protect
          (with-current-buffer hcx-buffer
            (let ((process
                   (start-process-shell-command "hcxpcapng" hcx-buffer
                                                (format "%s %s -o %s" pwnagotchi-hcxpcapng-path file (f-join pwnagotchi-handshakes-dir file)))))
              (if (= (process-exit-status process) 1)
                (signal 'pwnagotchi-hcxpcapngtool-parse-error file)
                )))))))

(defun pwnagotchi-batch-hcxformat (files output)
  "attempt to convert a pcap file with a handshake to the hashcat format 22000."
  (when pwnagotchi-hcxpcapng-path
    (let ((hcx-buffer (get-buffer-create "*hcxpcapng*"))
          (input (mapconcat #'identity files " ")))
      (unwind-protect
          (with-current-buffer hcx-buffer
            (let ((process
                   (start-process-shell-command "hcxpcapng" hcx-buffer
                                                (format "%s %s -o %s" pwnagotchi-hcxpcapng-path input output))))
              (if (= (process-exit-status process) 1)
                (signal 'pwnagotchi-hcxpcapngtool-parse-error output)
                )))))))




(defun pwnagotchi-convert-handshake ()
  "Interactivly convert a file to the hashcat 22000 format."
  (interactive)
  (let* ((handshakes (f-glob "*.pcap" pwnagotchi-handshakes-dir))
         (chosen-hs (completing-read "Enter A SSID or mac: " handshakes))
         )
    (pwnagotchi-convert-hcxformat chosen-hs)))

(defun pwnagotchi-convert-ssid-regex ()
  "Interactivly convert a base ssid name to the hashcat 22000 format.
the output file is the Basename of the glob pattern.
The glob pattern would be Wifi*"
  (interactive)
  (let* ((output-filename (read-string "Enter Output filename: "))
         (hs-regex (read-regexp "Enter Regex: "))
         (handshakes (f-glob "*.pcap" pwnagotchi-handshakes-dir))
         (matched-hs (mapcar (lambda (str)
                               (when str
                                 (string-match hs-regex str)))
                                'handshakes)))
    (pwnagotchi-batch-hcxformat matched-hs (f-join pwnagotchi-hashes-dir (format "%s.2200" output-filename))))
)
(defun pwnagotchi-convert-all ()
  "Convert all handshakes to a single file."
  (interactive)
  (let ((handshakes (f-glob "*.pcap" pwnagotchi-handshakes-dir)))
    (pwnagotchi-batch-hcxformat handshakes (f-join pwnagotchi-hashes-dir "all.22000"))))
(defun pwnagotchi-convert-glob ()
  "Convert all handshakes that match glob to a single file."
  (interactive)
  (let* ((glob (read-string "Enter Glob: "))(filename (read-file-name "Enter Output Filename: " pwnagotchi-hashes-dir))(handshakes (f-glob glob pwnagotchi-handshakes-dir)))
    (message "Parsed: %d handshakes" (length handshakes))
    (pwnagotchi-batch-hcxformat handshakes (f-join pwnagotchi-hashes-dir (format "%s.22000" filename))))
)


(provide 'pwnagotchi)
;;; pwnagotchi.el ends here
