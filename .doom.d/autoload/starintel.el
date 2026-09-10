;;; starintel.el --- StarIntel staging client for doom -*- lexical-binding: t; -*-

;; Operator config for the StarIntel Emacs client.  Lives in the dotfiles;
;; the bearer credential never does: it is resolved per-request from
;; `auth-source' (see ~/.authinfo, mode 600) with an env fallback, and is
;; never written to buffers, logs, or Customize.

(require 'client nil t)                 ; vendored: lisp/starintel/client.el

(defgroup nsa/starintel nil
  "StarIntel staging client configuration."
  :group 'starintel)

(defcustom nsa/starintel-server-url
  (or (getenv "STARINTEL_SERVER_URL") "http://ingest.star.intel")
  "Base URL of the staging ingest StarIntel server."
  :type 'string
  :group 'nsa/starintel)

(defcustom nsa/starintel-auth-host nil
  "auth-source host entry for the StarIntel API key.
When nil it is derived from `nsa/starintel-server-url'."
  :type '(choice (const :tag "Derive from server URL" nil) string)
  :group 'nsa/starintel)

(defun nsa/starintel-url-host-port ()
  "Return (HOST . PORT) for `nsa/starintel-server-url'."
  (let* ((url (if (string-match-p "://" nsa/starintel-server-url)
                  nsa/starintel-server-url
                (concat "http://" nsa/starintel-server-url)))
         (parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (port (url-port parsed)))
    (cons (or host "127.0.0.1") (or port 80))))

(defun nsa/starintel-auth-token ()
  "Return the StarIntel bearer token without exposing it.
Resolution order: $STARINTEL_API_KEY, then auth-source (entry for
the server host).  Returns nil when neither is available."
  (let* ((host-port (nsa/starintel-url-host-port))
         (host (or nsa/starintel-auth-host (car host-port)))
         (port (number-to-string (cdr host-port))))
    (or (getenv "STARINTEL_API_KEY")
        (let* ((entry (car (auth-source-search
                            :max 1 :user "starintel"
                            :host host
                            :port port
                            :require '(:secret))))
               (secret (plist-get entry :secret)))
          (cond
           ((functionp secret) (funcall secret))
           ((stringp secret) secret)
           (t nil))))))

(defun nsa/starintel-connect ()
  "Connect Emacs to the staging StarIntel server via auth-source."
  (interactive)
  (require 'client)
  (setq starintel-api-base-url nsa/starintel-server-url
        starintel-api-token-function #'nsa/starintel-auth-token)
  (starintel-connect))

(with-eval-after-load 'client
  (setq starintel-api-base-url (or (getenv "STARINTEL_SERVER_URL")
                                   starintel-api-base-url
                                   nsa/starintel-server-url)
        starintel-api-token-function #'nsa/starintel-auth-token))

(global-set-key (kbd "C-c S c") #'nsa/starintel-connect)
(global-set-key (kbd "C-c S s") #'starintel-search)
(global-set-key (kbd "C-c S d") #'starintel-document)
(global-set-key (kbd "C-c S S") #'starintel-status)

(provide 'starintel)
;;; starintel.el ends here
