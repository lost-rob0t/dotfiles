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

(defcustom nsa/alert-min-interval 15
  "Minimum seconds between desktop notifications emitted by Emacs."
  :type 'number
  :group 'alert)

(defcustom nsa/alert-max-pending 15
  "Maximum number of pending desktop notifications."
  :type 'integer
  :group 'alert)

(defvar nsa/alert--queue nil)
(defvar nsa/alert--queued-keys (make-hash-table :test #'equal))
(defvar nsa/alert--timer nil)
(defvar nsa/alert--last-sent 0.0)
(defvar nsa/alert--suppressed 0)

(defun nsa/alert--key (info)
  "Return a stable dedupe key for alert INFO."
  (list (plist-get info :title)
        (plist-get info :message)
        (plist-get info :category)
        (plist-get info :severity)))

(defun nsa/alert--send-now (info)
  "Deliver alert INFO immediately, bypassing the throttle queue."
  (if (fboundp #'notifications-notify)
      (let ((category (plist-get info :category))
            (urgency (cdr (assq (plist-get info :severity)
                                alert-libnotify-priorities))))
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
         :timeout (* 1000
                     (if (and (plist-get info :persistent)
                              (not (plist-get info :never-persist)))
                         0
                       alert-fade-time))
         :urgency (if urgency (symbol-name urgency) "normal")))
    (alert-message-notify info)))

(defun nsa/alert--schedule-drain ()
  "Schedule the next queued alert while enforcing the global rate limit."
  (unless (timerp nsa/alert--timer)
    (let* ((elapsed (- (float-time) nsa/alert--last-sent))
           (delay (max 0.0 (- nsa/alert-min-interval elapsed))))
      (setq nsa/alert--timer
            (run-at-time delay nil #'nsa/alert--drain)))))

(defun nsa/alert--drain ()
  "Emit one queued alert and schedule the next one."
  (setq nsa/alert--timer nil)
  (when-let ((entry (pop nsa/alert--queue)))
    (let ((key (car entry))
          (info (cdr entry)))
      (remhash key nsa/alert--queued-keys)
      (when (> nsa/alert--suppressed 0)
        (setq info (copy-sequence info))
        (setq info
              (plist-put
               info :message
               (format "%s\n\n[%d duplicate/overflow alerts suppressed]"
                       (or (plist-get info :message) "")
                       nsa/alert--suppressed)))
        (setq nsa/alert--suppressed 0))
      (setq nsa/alert--last-sent (float-time))
      (condition-case err
          (nsa/alert--send-now info)
        (error
         (message "Emacs alert delivery failed: %S" err))))
    (when nsa/alert--queue
      (setq nsa/alert--timer
            (run-at-time nsa/alert-min-interval nil #'nsa/alert--drain)))))

(defun alert-libnotify-notify (info)
  "Queue INFO for throttled desktop delivery.

At most `nsa/alert-max-pending' alerts are queued and no more than one
desktop notification is emitted every `nsa/alert-min-interval' seconds.
Identical queued alerts and overflow are suppressed and counted."
  (let ((key (nsa/alert--key info)))
    (cond
     ((gethash key nsa/alert--queued-keys)
      (setq nsa/alert--suppressed (1+ nsa/alert--suppressed)))
     ((>= (length nsa/alert--queue) nsa/alert-max-pending)
      (setq nsa/alert--suppressed (1+ nsa/alert--suppressed)))
     (t
      (puthash key t nsa/alert--queued-keys)
      (setq nsa/alert--queue
            (nconc nsa/alert--queue
                   (list (cons key (copy-sequence info)))))
      (nsa/alert--schedule-drain)))))


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
         (dd-command (format "sudo dd if=%s of=/dev/%s bs=4M status=progress && sync" source-file selected-drive)))
    (nsa/are-you-fucking-sure (format "You have selected the drive %s" selected-drive))
    (async-shell-command dd-command "*dd*")
    (message "Copying %s to drive %s. Command: %s" source-file selected-drive dd-command)))

(defun nsa/async-shell-command-alert (&optional cmd buffer error-buffer)
  "Send an alert when a command finishes."
  (interactive "MEnter Shell Command: ")
  (async-shell-command  (format "%s && dunstify \"%s\" \"Finished!\" " cmd (car  (split-string cmd))) buffer error-buffer))

(defun nsa/auth-source-get (&rest keys)
  "Fetch KEYS from auth-source"
  (funcall (plist-get (car (apply #'auth-source-search keys)) :secret)))


(provide 'saved)
;;; saved.el ends here
