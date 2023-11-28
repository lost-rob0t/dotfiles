;;; saved.el --- Saved Lisp code from scratch destruction -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: April 04, 2023
;; Modified: April 04, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/saved
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Saved Lisp code from scratch destruction
;;
;;; Code:

(require 'async)

(defun alert-libnotify-notify (info)
  "Send INFO using notifications-notify.
Handles :ICON, :CATEGORY, :SEVERITY, :PERSISTENT, :NEVER-PERSIST, :TITLE
and :MESSAGE keywords from the INFO plist.  :CATEGORY can be
passed as a single symbol, a string or a list of symbols or
strings."
  (if (fboundp #'notifications-notify)
      (let ((category (plist-get info :category))
            (urgency (cdr (assq (plist-get info :severity) alert-libnotify-priorities))))
        (notifications-notify
         :title (alert-encode-string (plist-get info :title))
         :body (alert-encode-string (plist-get info :message))
         :app-icon (or (plist-get info :icon) alert-default-icon)
         :category (cond ((symbolp category)
                          (symbol-name category))
                         ((stringp category) category)
                         ((listp category)
                          (mapconcat (if (symbolp (car category))
                                         #'symbol-name
                                       #'identity)
                                     category ",")))
         :timeout (* 1000               ; notify-send takes msecs
                     (if (and (plist-get info :persistent)
                              (not (plist-get info :never-persist)))
                         0              ; 0 indicates persistence
                       alert-fade-time))
         :urgency (if urgency (symbol-name urgency) "normal")))
    (alert-message-notify info)))


(defun nsaspy/kill-ssh-key ()
  "Interactivly Copy a public ssh key"
  (interactive)
  (let ((key (read-file-name "Select Key: " (f-expand "~/.ssh/"))))
    (kill-new (with-temp-buffer
                (insert-file-contents key)
                (goto-char (point-min))
                (buffer-string)))))
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))



(defun get-interface-ip (interface)
  "Get the IP address of a network interface."
  (let ((output (shell-command-to-string (concat "ip addr show dev " interface " | grep 'inet '"))))
    (when (string-match "\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)" output)
      (match-string 1 output))))

(defun get-iterfaces ()
  "Get a list of network interface names."
  (let ((output (shell-command-to-string "ip addr show | awk '/^[0-9]+:/ {gsub(/:/,\"\"); print $2}'")))
    (split-string output "\n" t)))

(defun nsaspy/kill-ip ()
  "Select and copy a network interface ip address."
  (interactive)
  (let* ((interface (completing-read "Interface: " (get-iterfaces)))
         (ip
          (get-interface-ip interface)))
    (kill-new ip)
    (message "%s" ip)
    ip))

(defcustom nsaspy/docker-images
  '(("ibmcom/couchdb3:latest" . "-d -e COUCHDB_USER=admin -e COUCHDB_PASSWORD=password -v $PWD/db:/opt/couchdb/data -p 0.0.0.0:5984:5984")
    ("postgres:latest" . "-d -e POSTGRES_USER=myuser -e POSTGRES_PASSWORD=mypassword -p 5432:5432")
    ("mongo:latest" . "-d -e MONGO_INITDB_ROOT_USERNAME=admin -e MONGO_INITDB_ROOT_PASSWORD=password -p 27017:27017")
    ("redis:latest" . "-d -p 6379:6379")
    ("nginx:latest" . "-d -p 80:80")
    ("wordpress:latest" . "-d -e WORDPRESS_DB_HOST=db -e WORDPRESS_DB_USER=myuser -e WORDPRESS_DB_PASSWORD=mypassword -p 8080:80")
    ("rabbitmq:latest" . "-d -p 5672:5672 -p 15672:15672")
    ("elasticsearch:latest" . "-d -p 9200:9200 -p 9300:9300")
    ("memcached:latest" . "-d -p 11211:11211")
    ("influxdb:latest" . "-d -p 8086:8086"))
  "List of docker images for completing read.")


(defun nsaspy/kill-docker-cmd ()
  "Copy a commonly used docker command to the kill ring."
  (interactive)
  (let* ((image (completing-read "Select Image: " (mapcar #'car nsaspy/docker-images) nil nil))
         (options (read-string "Options: " (cdr (assoc image nsaspy/docker-images))))

         (docker (executable-find "docker")))


    (kill-new (format "sudo %s %s %s" docker options image))))



(defun nsaspy/emacs-script ()
  "Run a emacs script in batch mode"
  (interactive)
  (let* ((file (read-file-name "Script to run: " default-directory))
         (buffer (get-buffer-create "*emacs-script*"))
         (args (read-string "Arguments: " nil t)))
    (async-shell-command (format "%s --script %s %s" (executable-find "emacs") file args) buffer buffer)))


(defun nsaspy/wget-region
    (start end)
  "Download a region of URLs using wget."
  (interactive "r")
  (let
      ((url-list
        (split-string
         (buffer-substring start end)
         "\n" t)))
    (dolist
        (url url-list)
      (when
          (string-match-p "^https?://" url)
        (shell-command
         (concat "wget " url))))))

(defun nsaspy/wget-mirror (url directory)
  "Mirror a website using wget."
  (interactive "sEnter the URL to mirror: \nDEnter the target directory: ")
  (async-shell-command (format "wget --mirror --convert-links --adjust-extension --page-requisites --no-parent -P %s %s" directory url)))

(defun nsa/list-drives ()
  "Create a list of  disks on a Linux system."
  (let ((output (shell-command-to-string "lsblk --nodeps --output NAME -n")))
    (split-string output)))

(defun nsa/select-disk ()
  "Select a Linux Drive."
  (completing-read "Drive: " (nsa/list-drives)))


(defun nsa/are-you-fucking-sure (&optional str)
  "Ask for confirmation three times., Optionaly include a STR message."
  (interactive)
  (unless (y-or-n-p (format  "%s Are you fucking sure? (1/3) " str))
    (message "Cancelled.")
    (setq this-command 'ignore)
    nil)
  (unless (y-or-n-p (format " %s Are you really fucking sure? (2/3) " str))
    (message "Cancelled.")
    (setq this-command 'ignore)
    nil)
  (unless (y-or-n-p (format "%s Are you absolutely fucking sure? (3/3) " str))
    (message "Cancelled.")
    (setq this-command 'ignore)
    nil)
  t)


(defun nsa/dd-drive (&optional source-file)
  "Copy data from SOURCE-FILE (or file under point in Dired) to a selected drive using dd."
  (interactive)
  (unless source-file
    (setq source-file (dired-get-filename)))
  (let* ((drive-list (nsa/list-drives))
         (selected-drive (nsa/select-disk))
         (dd-command (format "sudo dd if=%s of=/dev/%s bs=4M status=progress" source-file selected-drive)))
    (nsa/are-you-fucking-sure (format  "You have selected the drive %s" selected-drive))
    (async-shell-command dd-command "*dd*")
    (message "Copying %s to drive %s. Command: %s" source-file selected-drive dd-command)))

(defun nsa/async-shell-command-alert (&optional cmd buffer error-buffer)
  "Send an alert when a command finishes."
  (interactive "MEnter Shell Command: ")
  (async-shell-command  (format "%s && dunstify \"%s\" \"Finished!\" " cmd (car  (split-string cmd))) buffer error-buffer))





(provide 'saved)
;;; saved.el ends here
