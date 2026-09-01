(in-package #:nyxt-user)

(defparameter *starintel-default-base-url* "http://127.0.0.1:5000"
  "Default StarIntel HTTP API base URL when no persistent config or override exists.")

(defparameter *starintel-empty-document-json*
  "{\"_id\":\"\",\"dataset\":\"\",\"dtype\":\"\",\"schema_version\":\"0.9.0\",\"version\":1,\"date_added\":\"\",\"date_updated\":\"\",\"title\":\"\",\"summary\":\"\",\"description\":\"\",\"status\":\"recorded\",\"language\":\"en\",\"tags\":[],\"labels\":[],\"aliases\":[],\"keywords\":[],\"identifiers\":[],\"sources\":[],\"evidence\":[],\"temporal\":{},\"provenance\":{},\"assessment\":{},\"verification\":{\"status\":\"unverified\",\"verified\":false},\"handling\":{\"visibility\":\"public\",\"sensitive\":false,\"pii\":false},\"lineage\":{},\"quality\":{},\"workflow\":{},\"geospatial\":{},\"attachments\":[],\"related_ids\":[],\"notes\":[],\"data\":{},\"extensions\":{}}"
  "Canonical empty StarIntel v0.9 document envelope.")

(defun starintel-config-root ()
  (merge-pathnames
   "starintel/"
   (uiop:ensure-directory-pathname
    (or (uiop:getenv "XDG_CONFIG_HOME")
        (merge-pathnames ".config/" (user-homedir-pathname))))))

(defun starintel-config-file (name)
  (merge-pathnames name (starintel-config-root)))

(defun starintel-nonblank (value)
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
      (unless (zerop (length trimmed))
        trimmed))))

(defun starintel-read-config-file (name)
  (let ((path (starintel-config-file name)))
    (when (probe-file path)
      (starintel-nonblank (uiop:read-file-string path)))))

(defun starintel-base-url ()
  (string-right-trim
   "/"
   (or (starintel-nonblank (uiop:getenv "STARINTEL_URL"))
       (starintel-read-config-file "url")
       *starintel-default-base-url*)))

(defun starintel-file-mode (path)
  (starintel-nonblank
   (ignore-errors
     (uiop:run-program
      (list "stat" "-L" "-c" "%a" (namestring path))
      :output :string
      :error-output nil
      :ignore-error-status t))))

(defun starintel-api-key-file ()
  (let ((path (starintel-config-file "api-key")))
    (when (probe-file path)
      (unless (string= (or (starintel-file-mode path) "") "600")
        (error "Refusing StarIntel API key file unless its mode is 0600: ~a"
               (namestring path)))
      (starintel-read-config-file "api-key"))))

(defun starintel-api-key ()
  (or (starintel-nonblank (uiop:getenv "STARINTEL_API_KEY"))
      (starintel-api-key-file)))

(defun starintel-headers (&key json)
  (let ((headers (when json
                   (list (cons "Content-Type" "application/json")))))
    (when-let ((key (starintel-api-key)))
      (push (cons "Authorization" (format nil "Bearer ~a" key)) headers))
    headers))

(defun starintel-get (path)
  (handler-case
      (dex:get (format nil "~a~a" (starintel-base-url) path)
               :headers (starintel-headers))
    (error ()
      (error "StarIntel GET request failed."))))

(defun starintel-post (path object)
  (handler-case
      (dex:post (format nil "~a~a" (starintel-base-url) path)
                :headers (starintel-headers :json t)
                :content (njson:encode object))
    (error ()
      (error "StarIntel POST request failed."))))

(defun starintel-param (url-designator key &optional default)
  (or (assoc-value (quri:uri-query-params (url url-designator))
                   key
                   :test #'string=)
      default))

(defun starintel-route-name (url-designator)
  (let* ((uri (url url-designator))
         (host (quri:uri-host uri))
         (path (string-trim "/" (or (quri:uri-path uri) ""))))
    (string-downcase (if (and host (plusp (length host))) host path))))

(defun starintel-json-get (object key &optional default)
  (handler-case
      (let ((value (njson:jget key object)))
        (if (eq value :null) default value))
    (error () default)))

(defun starintel-result-doc (row)
  (or (starintel-json-get row "doc") row))

(defun starintel-result-id (row)
  (let ((doc (starintel-result-doc row)))
    (or (starintel-json-get doc "_id")
        (starintel-json-get doc "id")
        (starintel-json-get row "id")
        "")))

(defun starintel-result-type (row)
  (let ((doc (starintel-result-doc row)))
    (or (starintel-json-get doc "dtype")
        (starintel-json-get doc "type")
        "")))

(defun starintel-result-dataset (row)
  (or (starintel-json-get (starintel-result-doc row) "dataset") ""))

(defun starintel-result-summary (row)
  (let ((doc (starintel-result-doc row)))
    (or (starintel-json-get doc "text")
        (starintel-json-get doc "name")
        (starintel-json-get doc "domain")
        (starintel-json-get doc "url")
        (starintel-json-get doc "handle")
        (starintel-json-get doc "email")
        "")))

(defparameter *starintel-page-css*
  "body { background:#170c32; color:#f3f4f5; font-family:system-ui,sans-serif; margin:0; padding:24px; }
   a { color:#2de2e6; text-decoration:none; }
   a:visited { color:#92406e; }
   a:hover, a:active { color:#f6019d; }
   h1,h2 { color:#f6019d; }
   .bar, .card { background:#202146; border:1px solid #92406e; border-radius:8px; padding:14px; margin-bottom:16px; }
   form { display:flex; gap:8px; flex-wrap:wrap; align-items:end; }
   label { color:#2de2e6; font-size:.85rem; display:flex; flex-direction:column; gap:4px; }
   input { background:#170c32; color:#f3f4f5; border:1px solid #92406e; border-radius:6px; padding:8px; }
   input:focus { outline:1px solid #2de2e6; border-color:#2de2e6; }
   button { background:#f6019d; color:#170c32; border:1px solid #2de2e6; border-radius:6px; padding:8px 14px; font-weight:700; }
   table { width:100%; border-collapse:collapse; background:#202146; }
   th { color:#2de2e6; text-align:left; border-bottom:1px solid #92406e; padding:8px; }
   td { border-bottom:1px solid #3a285d; padding:8px; vertical-align:top; }
   tr:hover { background:#2a285b; }
   code, pre { font-family:ui-monospace,monospace; }
   pre { white-space:pre-wrap; overflow-wrap:anywhere; background:#0f0822; border:1px solid #92406e; padding:14px; border-radius:8px; }
   .muted { color:#bca8cf; }
   .pager { display:flex; gap:14px; margin-top:16px; }"
  "CSS for native StarIntel Nyxt pages.")

(defun starintel-page (title body-writer)
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title title)
      (:style (:raw *starintel-page-css*)))
     (:body
      (:div.bar
       (:strong "STARINTEL")
       " · "
       (:a :href "starintel:search" "search")
       " · API "
       (:span.muted (starintel-base-url)))
      (funcall body-writer)))))

(defun starintel-search-page (url)
  (let* ((query (starintel-param url "q" ""))
         (limit-raw (starintel-param url "limit" "50"))
         (sort (starintel-param url "sort" ""))
         (bookmark (starintel-param url "bookmark" ""))
         (limit (or (parse-integer limit-raw :junk-allowed t) 50))
         (limit (max 1 (min limit 200)))
         (body (unless (str:blankp query)
                 (starintel-get
                  (format nil "/search?q=~a&limit=~d~@[&sort=~a~]~@[&bookmark=~a~]"
                          (quri:url-encode query)
                          limit
                          (unless (str:blankp sort) (quri:url-encode sort))
                          (unless (str:blankp bookmark) (quri:url-encode bookmark))))))
         (json (when body (njson:decode body)))
         (rows (or (and json (starintel-json-get json "rows")) #()))
         (next-bookmark (and json (starintel-json-get json "bookmark")))
         (total (and json (or (starintel-json-get json "total_rows")
                              (starintel-json-get json "total")))))
    (starintel-page
     "StarIntel Search"
     (lambda ()
       (spinneret:with-html
         (:h1 "Search")
         (:div.card
          (:form :method "get" :action "starintel:search"
           (:label "Query" (:input :name "q" :value query :size "60" :autofocus t))
           (:label "Limit" (:input :name "limit" :type "number" :min "1" :max "200" :value (princ-to-string limit)))
           (:label "Sort" (:input :name "sort" :value sort :placeholder "optional CouchDB sort"))
           (:button :type "submit" "Search")))
         (when body
           (:p.muted
            (format nil "~d result~:p~@[ · ~a total~]" (length rows) total))
           (:table
            (:thead (:tr (:th "Type") (:th "ID") (:th "Dataset") (:th "Summary")))
            (:tbody
             (loop for row across (coerce rows 'vector)
                   for id = (starintel-result-id row)
                   do (:tr
                       (:td (starintel-result-type row))
                       (:td
                        (if (str:blankp id)
                            "—"
                            (:a :href (format nil "starintel:document?id=~a" (quri:url-encode id)) id)))
                       (:td (starintel-result-dataset row))
                       (:td (starintel-result-summary row))))))
           (when next-bookmark
             (:div.pager
              (:a :href (format nil "starintel:search?q=~a&limit=~d~@[&sort=~a~]&bookmark=~a"
                                (quri:url-encode query)
                                limit
                                (unless (str:blankp sort) (quri:url-encode sort))
                                (quri:url-encode next-bookmark))
                  "Next page →")))
           (:details
            (:summary "Raw response")
            (:pre body))))))))

(defun starintel-document-page (url)
  (let* ((id (starintel-param url "id" ""))
         (body (unless (str:blankp id)
                 (starintel-get (format nil "/document/~a" (quri:url-encode id))))))
    (starintel-page
     (if (str:blankp id) "StarIntel Document" (format nil "StarIntel · ~a" id))
     (lambda ()
       (spinneret:with-html
         (:h1 "Document")
         (if (str:blankp id)
             (:p "Missing document id.")
             (progn
               (:p (:strong id))
               (:pre body))))))))

(defun starintel-scheme-handler (url)
  (handler-case
      (case (intern (string-upcase (starintel-route-name url)) :keyword)
        (:SEARCH (starintel-search-page url))
        (:DOCUMENT (starintel-document-page url))
        (otherwise (starintel-search-page "starintel:search")))
    (error ()
      (starintel-page
       "StarIntel Error"
       (lambda ()
         (spinneret:with-html
           (:h1 "StarIntel request failed")
           (:p "The request failed. Check the API URL, API key, and Nyxt logs.")))))))

(define-internal-scheme "starintel" #'starintel-scheme-handler)

(defun starintel-open-url (url &key new-buffer)
  (ffi-buffer-load (if new-buffer (make-buffer-focus) (current-buffer)) url))

(define-command-global starintel-search ()
  "Open the full StarIntel search interface in Nyxt."
  (starintel-open-url "starintel:search" :new-buffer t))

(define-command-global starintel-search-selection ()
  "Search StarIntel for the current page selection."
  (let ((selection (ffi-buffer-copy (current-buffer))))
    (if (str:blankp selection)
        (echo "Select text first.")
        (starintel-open-url
         (format nil "starintel:search?q=~a" (quri:url-encode selection))
         :new-buffer t))))

(defun starintel-canonical-dtype (dtype)
  (let ((token (substitute #\- #\_ (string-downcase dtype))))
    (cond
      ((member token '("organization" "organisation") :test #'string=) "org")
      ((string= token "investigation-target") "target")
      ((string= token "social-media-posts") "social-media-post")
      (t token))))

(defun starintel-now ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun starintel-new-id (dtype)
  (format nil "starintel:~a:nyxt-~d-~8,'0x"
          dtype
          (get-universal-time)
          (random #x100000000)))

(defun starintel-new-document (dtype dataset data)
  (unless (hash-table-p data)
    (error "StarIntel data JSON must be an object."))
  (let* ((dtype (starintel-canonical-dtype dtype))
         (now (starintel-now))
         (document (njson:decode *starintel-empty-document-json*)))
    (setf (gethash "_id" document) (starintel-new-id dtype)
          (gethash "dataset" document) dataset
          (gethash "dtype" document) dtype
          (gethash "date_added" document) now
          (gethash "date_updated" document) now
          (gethash "data" document) data)
    document))

(defun starintel-merge-json-object (target source)
  (unless (hash-table-p source)
    (error "Extra JSON must be an object."))
  (maphash (lambda (key value)
             (setf (gethash key target) value))
           source)
  target)

(define-command-global starintel-create-document-from-selection ()
  "Create a canonical StarIntel v0.9 document using selected text as a data field."
  (let ((selection (ffi-buffer-copy (current-buffer))))
    (if (str:blankp selection)
        (echo "Select the field value first.")
        (let* ((dtype (starintel-canonical-dtype
                       (prompt1 :prompt "Document type" :sources 'prompter:raw-source)))
               (dataset (prompt1 :prompt "Dataset"
                                 :input "manual"
                                 :sources 'prompter:raw-source))
               (field (prompt1 :prompt "Data field name" :sources 'prompter:raw-source))
               (extra-raw (prompt1 :prompt "Extra data JSON object"
                                   :input "{}"
                                   :sources 'prompter:raw-source))
               (data (njson:decode extra-raw)))
          (when (or (str:blankp dtype)
                    (str:blankp dataset)
                    (str:blankp field))
            (error "Document type, dataset, and data field name are required."))
          (unless (hash-table-p data)
            (error "Extra data JSON must be an object."))
          (setf (gethash field data) selection)
          (let ((document (starintel-new-document dtype dataset data)))
            (starintel-post (format nil "/new/document/~a" (quri:url-encode dtype)) document)
            (echo "Created StarIntel ~a document from selected data field ~a." dtype field))))))

(define-command-global starintel-create-document ()
  "Create a StarIntel document from a complete canonical JSON object."
  (let* ((dtype (starintel-canonical-dtype
                 (prompt1 :prompt "Document type" :sources 'prompter:raw-source)))
         (raw (prompt1 :prompt "Canonical document JSON"
                       :input (format nil "{\"dtype\":\"~a\"}" dtype)
                       :sources 'prompter:raw-source))
         (document (njson:decode raw)))
    (unless (hash-table-p document)
      (error "Document JSON must be an object."))
    (setf (gethash "dtype" document) dtype)
    (starintel-post (format nil "/new/document/~a" (quri:url-encode dtype)) document)
    (echo "Created StarIntel ~a document." dtype)))

;; Extend only VI normal mode: these bindings never steal printable keys while
;; Nyxt is in VI insert mode or a prompt buffer is accepting text.
(define-configuration nyxt/mode/vi:vi-normal-mode
  ((keyscheme-map
    (define-keyscheme-map
     "vi-starintel" (list :import %slot-value%)
     nyxt/keyscheme:vi-normal
     (list
      "g s" 'starintel-search
      "g S" 'starintel-search-selection
      "g d" 'starintel-create-document-from-selection
      "g D" 'starintel-create-document)))))
