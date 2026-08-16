;;; starintel.el --- Shared StarIntel API client -*- lexical-binding: t; -*-

;; Reuses the StarIntel HTTP boundary instead of embedding Hackmode or provider
;; implementations in Emacs.

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)

(defgroup starintel nil
  "Shared StarIntel client for Emacs."
  :group 'tools
  :prefix "starintel-")

(defcustom starintel-scheme "http"
  "Scheme used to contact StarIntel Server."
  :type '(choice (const "http") (const "https"))
  :group 'starintel)

(defcustom starintel-host "127.0.0.1"
  "StarIntel Server host."
  :type 'string
  :group 'starintel)

(defcustom starintel-port 5000
  "StarIntel Server port."
  :type 'integer
  :group 'starintel)

(defcustom starintel-auth-token-function nil
  "Optional function returning a bearer token for StarIntel requests.

The token is read immediately before a request and is never included in
messages or error objects.  Keep secrets in auth-source or another secret store
and point this option at a function that retrieves them."
  :type '(choice (const :tag "No bearer token" nil) function)
  :group 'starintel)

(defvar starintel--server-info nil
  "Most recently returned StarIntel server metadata.")

(defvar starintel--last-error nil
  "Most recent structured StarIntel client error.")

(defun starintel-base-url ()
  "Return the configured StarIntel Server base URL."
  (format "%s://%s:%d" starintel-scheme starintel-host starintel-port))

(defun starintel--path-url (path)
  "Return the absolute StarIntel URL for PATH."
  (concat (string-remove-suffix "/" (starintel-base-url))
          "/"
          (string-remove-prefix "/" path)))

(defun starintel--query-string (params)
  "Encode alist PARAMS as a query string."
  (when params
    (concat
     "?"
     (mapconcat
      (lambda (pair)
        (format "%s=%s"
                (url-hexify-string
                 (if (symbolp (car pair))
                     (symbol-name (car pair))
                   (format "%s" (car pair))))
                (url-hexify-string (format "%s" (cdr pair)))))
      params
      "&"))))

(defun starintel--request-url (path params)
  "Return request URL for PATH and query PARAMS."
  (concat (starintel--path-url path)
          (or (starintel--query-string params) "")))

(defun starintel--bearer-token ()
  "Return the configured bearer token, or nil."
  (when starintel-auth-token-function
    (let ((token (funcall starintel-auth-token-function)))
      (when (and (stringp token) (not (string-empty-p token)))
        token))))

(defun starintel--request-headers ()
  "Return HTTP headers for a StarIntel request."
  (let ((headers '(("Accept" . "application/json")
                   ("Content-Type" . "application/json")))
        (token (starintel--bearer-token)))
    (if token
        (cons (cons "Authorization" (concat "Bearer " token)) headers)
      headers)))

(defun starintel--response-body ()
  "Return the HTTP response body from the current URL buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\r?\n\r?\n" nil t)
        (buffer-substring-no-properties (point) (point-max))
      "")))

(defun starintel--decode-json (body)
  "Decode JSON BODY into alists/lists, or nil for an empty body."
  (unless (string-empty-p (string-trim body))
    (json-parse-string body
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object :false)))

(defun starintel--error (type &rest properties)
  "Construct a StarIntel client error of TYPE with PROPERTIES."
  (append (list :type type) properties))

(defun starintel--finish-request (transport-status success error)
  "Finish the current URL request.

TRANSPORT-STATUS is the plist supplied by `url-retrieve'.  SUCCESS receives the
decoded response for 2xx requests.  ERROR receives a structured plist and never
a credential value."
  (let ((buffer (current-buffer)))
    (unwind-protect
        (let ((transport-error (plist-get transport-status :error)))
          (cond
           (transport-error
            (let ((failure (starintel--error :transport
                                             :detail (format "%s" transport-error))))
              (setq starintel--last-error failure)
              (when error (funcall error failure))))
           (t
            (let* ((status (or url-http-response-status 0))
                   (body (starintel--response-body))
                   (decoded
                    (condition-case parse-error
                        (starintel--decode-json body)
                      (error
                       (starintel--error
                        :decode
                        :status status
                        :detail (error-message-string parse-error))))))
              (cond
               ((and (listp decoded) (eq (plist-get decoded :type) :decode))
                (setq starintel--last-error decoded)
                (when error (funcall error decoded)))
               ((and (>= status 200) (< status 300))
                (setq starintel--last-error nil)
                (when success (funcall success decoded)))
               (t
                (let ((failure (starintel--error :http
                                                 :status status
                                                 :response decoded)))
                  (setq starintel--last-error failure)
                  (when error (funcall error failure)))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(cl-defun starintel-request (method path
                                    &key params data success error)
  "Asynchronously send METHOD PATH to StarIntel Server.

PARAMS is an alist of query parameters.  DATA is JSON-encoded when non-nil.
SUCCESS receives decoded JSON.  ERROR receives a structured error plist.
Return the request buffer created by `url-retrieve'."
  (let ((url-request-method (upcase (string-remove-prefix ":" (format "%s" method))))
        (url-request-extra-headers (starintel--request-headers))
        (url-request-data (when data
                            (encode-coding-string (json-encode data) 'utf-8))))
    (url-retrieve (starintel--request-url path params)
                  (lambda (status)
                    (starintel--finish-request status success error))
                  nil
                  t
                  t)))

(defun starintel--interactive-error (prefix)
  "Return an error callback that reports PREFIX without secret data."
  (lambda (failure)
    (message "%s: %s" prefix
             (or (plist-get failure :status)
                 (plist-get failure :type)))))

(defun starintel-get-server-info (&optional callback)
  "Asynchronously fetch StarIntel server metadata.

When called interactively, display the returned server/version.  CALLBACK, when
non-nil, receives the decoded metadata."
  (interactive)
  (starintel-request
   :get "/"
   :success
   (lambda (data)
     (setq starintel--server-info data)
     (when (called-interactively-p 'interactive)
       (message "StarIntel %s (document spec %s)"
                (alist-get 'version data)
                (alist-get 'doc_spec_version data)))
     (when callback (funcall callback data)))
   :error (starintel--interactive-error "StarIntel server info failed")))

(defun starintel-health-check (&optional callback)
  "Asynchronously query StarIntel `/health'."
  (interactive)
  (starintel-request
   :get "/health"
   :success
   (lambda (data)
     (when (called-interactively-p 'interactive)
       (message "StarIntel health: %s" (alist-get 'msg data)))
     (when callback (funcall callback data)))
   :error (starintel--interactive-error "StarIntel health check failed")))

(defun starintel-get-document (id success &optional error)
  "Asynchronously fetch StarIntel document ID.

SUCCESS receives the decoded document.  ERROR receives a structured failure."
  (starintel-request :get (format "/document/%s" (url-hexify-string id))
                     :success success
                     :error error))

(defun starintel-search (query success &optional limit bookmark error)
  "Asynchronously search StarIntel for QUERY.

SUCCESS receives decoded search results.  LIMIT defaults to 50.  BOOKMARK is
passed through when non-nil."
  (let ((params `((q . ,query) (limit . ,(or limit 50)))))
    (when bookmark
      (setq params (append params `((bookmark . ,bookmark)))))
    (starintel-request :get "/search"
                       :params params
                       :success success
                       :error error)))

(defun starintel-ingest-document (dtype document &optional success error)
  "Asynchronously ingest DOCUMENT as DTYPE through the server ingest boundary."
  (starintel-request
   :post
   (format "/new/document/%s" (url-hexify-string (format "%s" dtype)))
   :data document
   :success success
   :error error))

(defun starintel-ingest-documents (documents &optional success error)
  "Asynchronously submit DOCUMENTS through StarIntel bulk ingest."
  (starintel-request :post "/documents/bulk"
                     :data documents
                     :success success
                     :error error))

(defun starintel-create-target (actor target &optional success error)
  "Asynchronously create TARGET for ACTOR through the StarIntel target API."
  (starintel-request
   :post
   (format "/new/target/%s" (url-hexify-string (format "%s" actor)))
   :data target
   :success success
   :error error))

(defun starintel-get-targets (actor success &optional error)
  "Asynchronously fetch targets registered for ACTOR."
  (starintel-request
   :get
   (format "/targets/%s" (url-hexify-string (format "%s" actor)))
   :success success
   :error error))

(provide 'starintel)
;;; starintel.el ends here
