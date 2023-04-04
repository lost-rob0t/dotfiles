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
         :timeout (* 1000 ; notify-send takes msecs
                     (if (and (plist-get info :persistent)
                              (not (plist-get info :never-persist)))
                         0 ; 0 indicates persistence
                       alert-fade-time))
         :urgency (if urgency (symbol-name urgency) "normal")))
    (alert-message-notify info)))



(provide 'saved)
;;; saved.el ends here
