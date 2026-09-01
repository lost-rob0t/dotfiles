(in-package #:nyxt-user)

(defparameter *star-lang-root*
  (merge-pathnames ".local/share/star-lang/" (user-homedir-pathname))
  "Pinned StarLang source tree installed by Home Manager.")

(defparameter *star-runtime-directory-file-name* "runtime-directory.json"
  "JSON runtime-directory snapshot consumed by the default Nyxt StarLang port.")

(defvar *star-lang-runtime-loaded-p* nil)

(defun star-config-root ()
  (merge-pathnames
   "star-lang/"
   (uiop:ensure-directory-pathname
    (or (uiop:getenv "XDG_CONFIG_HOME")
        (merge-pathnames ".config/" (user-homedir-pathname))))))

(defun star-runtime-directory-file ()
  (merge-pathnames *star-runtime-directory-file-name* (star-config-root)))

(defun star-lang-asd-path (relative)
  (merge-pathnames relative *star-lang-root*))

(defun star-lang-ensure-runtime ()
  "Load the pinned final StarLang actor/runtime systems on first use."
  (unless *star-lang-runtime-loaded-p*
    (unless (probe-file *star-lang-root*)
      (error "Pinned StarLang source is missing at ~a. Run Home Manager first."
             (namestring *star-lang-root*)))
    (dolist (relative '("star-actor-protocol/star-actor-protocol.asd"
                        "star-mailbox/star-mailbox.asd"
                        "starlang-runtime/starlang-runtime.asd"))
      (let ((path (star-lang-asd-path relative)))
        (unless (probe-file path)
          (error "Pinned StarLang system definition is missing: ~a"
                 (namestring path)))
        (asdf:load-asd path)))
    (asdf:load-system :starlang-runtime)
    (setf *star-lang-runtime-loaded-p* t))
  t)

(defun star-json-get (object key &optional default)
  (handler-case
      (let ((value (njson:jget key object)))
        (if (eq value :null) default value))
    (error () default)))

(defun star-json-required-string (object key)
  (let ((value (star-json-get object key)))
    (unless (and (stringp value) (plusp (length value)))
      (error "StarLang runtime-directory entry requires non-empty string ~a." key))
    value))

(defun star-json-optional-string (object key)
  (let ((value (star-json-get object key)))
    (when value
      (unless (stringp value)
        (error "StarLang runtime-directory field ~a must be a string." key))
      value)))

(defun star-json-runtime (object)
  (let ((value (star-json-required-string object "runtime")))
    (intern (string-upcase value) :keyword)))

(defun star-json-alive (object)
  (let ((value (star-json-get object "alive" :unknown)))
    (cond
      ((or (eq value t) (null value)) value)
      ((or (eq value :unknown)
           (and (stringp value) (string-equal value "unknown")))
       :unknown)
      (t
       (error "StarLang runtime-directory alive must be true, false, or unknown.")))))

(defun star-json-capabilities (object)
  (let ((value (star-json-get object "capabilities")))
    (cond
      ((null value) nil)
      ((vectorp value)
       (let ((items (coerce value 'list)))
         (unless (every #'stringp items)
           (error "StarLang runtime-directory capabilities must be strings."))
         items))
      (t
       (error "StarLang runtime-directory capabilities must be a JSON array.")))))

(defun star-json-entry->plist (object)
  (unless (hash-table-p object)
    (error "StarLang runtime-directory entries must be JSON objects."))
  (let ((entry
          (list :name (star-json-required-string object "name")
                :runtime (star-json-runtime object)
                :alive (star-json-alive object))))
    (dolist (field '(("service-uri" :service-uri)
                     ("domain" :domain)
                     ("address" :address)
                     ("endpoint" :endpoint)
                     ("ref" :ref)))
      (let ((value (star-json-optional-string object (first field))))
        (when value
          (setf entry (append entry (list (second field) value))))))
    (let ((capabilities (star-json-capabilities object)))
      (when capabilities
        (setf entry (append entry (list :capabilities capabilities)))))
    entry))

(defun star-file-runtime-directory-snapshot (context)
  "Read the default local runtime-directory snapshot for StarLang resolution."
  (declare (ignore context))
  (let ((path (star-runtime-directory-file)))
    (unless (probe-file path)
      (error "No StarLang runtime directory at ~a."
             (namestring path)))
    (let ((value (njson:decode (uiop:read-file-string path))))
      (unless (vectorp value)
        (error "StarLang runtime directory must be a JSON array."))
      (map 'list #'star-json-entry->plist value))))

(defparameter *star-runtime-directory-snapshot-function*
  #'star-file-runtime-directory-snapshot
  "Snapshot provider injected into StarLang's runtime-directory port.
Override this with another function to bind Nyxt to a live runtime directory.")

(defun star-runtime-directory-port ()
  (star-lang-ensure-runtime)
  (starlangruntime:make-runtime-directory-port
   :snapshot *star-runtime-directory-snapshot-function*))

(defun star-uri-string (url-designator)
  "Return URL-DESIGNATOR as the exact URI string StarLang should parse."
  (cond
    ((stringp url-designator) url-designator)
    ((quri:uri-p url-designator) (quri:render-uri url-designator))
    (t (quri:render-uri (url url-designator)))))

(defun star-resolve-uri (url-designator)
  "Resolve URL-DESIGNATOR with StarLang's authoritative service resolver."
  (let ((uri (star-uri-string url-designator)))
    (star-lang-ensure-runtime)
    ;; Parse/canonicalize before directory lookup so Nyxt does not duplicate
    ;; StarLang's service-URI grammar or validation rules.
    (let* ((parsed (staractorprotocol:ensure-star-service-uri uri))
           (canonical (staractorprotocol:star-service-uri-string parsed)))
      (starlangruntime:resolve-star-service-uri
       (star-runtime-directory-port)
       :nyxt
       canonical))))

(defparameter *star-page-css*
  "body { background:#170c32; color:#f3f4f5; font-family:system-ui,sans-serif; margin:0; padding:24px; }
   h1 { color:#f6019d; }
   .card { background:#202146; border:1px solid #92406e; border-radius:8px; padding:16px; margin:14px 0; }
   dt { color:#2de2e6; font-weight:700; margin-top:10px; }
   dd { margin-left:0; overflow-wrap:anywhere; }
   code { font-family:ui-monospace,monospace; }
   .ok { color:#2de2e6; }
   .warn { color:#fba922; }
   .muted { color:#bca8cf; }"
  "CSS for StarLang service-resolution pages.")

(defun star-page (title body-writer)
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title title)
      (:style (:raw *star-page-css*)))
     (:body
      (:h1 title)
      (funcall body-writer)))))

(defun star-alive-label (value)
  (cond
    ((eq value t) "alive")
    ((null value) "unavailable")
    (t "unknown")))

(defun star-runtime-label (value)
  (if (keywordp value)
      (string-downcase (symbol-name value))
      (princ-to-string value)))

(defun star-capabilities-label (value)
  (if value
      (format nil "~{~a~^, ~}" value)
      "none declared"))

(defun star-resolved-page (uri entry)
  (star-page
   "StarLang service"
   (lambda ()
     (spinneret:with-html
       (:div.card
        (:div.ok "resolved by starlang-runtime")
        (:dl
         (:dt "URI") (:dd (:code uri))
         (:dt "Actor") (:dd (getf entry :name))
         (:dt "Domain") (:dd (or (getf entry :domain) "—"))
         (:dt "Address") (:dd (or (getf entry :address) "—"))
         (:dt "Runtime") (:dd (star-runtime-label (getf entry :runtime)))
         (:dt "Liveness") (:dd (star-alive-label (getf entry :alive)))
         (:dt "Capabilities")
         (:dd (star-capabilities-label (getf entry :capabilities)))
         (when (getf entry :endpoint)
           (:dt "Endpoint") (:dd (:code (getf entry :endpoint))))))
       (:p.muted
        "Resolution uses StarLang's service URI parser and runtime-directory resolver; Nyxt does not implement a second URI grammar.")))))

(defun star-error-page (url-designator condition)
  (star-page
   "StarLang resolution failed"
   (lambda ()
     (spinneret:with-html
       (:div.card
        (:div.warn "unresolved")
        (:dl
         (:dt "URI") (:dd (:code (ignore-errors (star-uri-string url-designator))))
         (:dt "Reason") (:dd (princ-to-string condition))))
       (:p.muted
        (format nil "Default directory source: ~a"
                (namestring (star-runtime-directory-file))))))))

(defun star-scheme-handler (url-designator)
  (handler-case
      (let* ((uri (star-uri-string url-designator))
             (entry (star-resolve-uri uri)))
        (star-resolved-page uri entry))
    (error (condition)
      (star-error-page url-designator condition))))

(define-internal-scheme "star" #'star-scheme-handler)

(define-command-global star-open ()
  "Open and resolve a StarLang star:// service URI in a new Nyxt buffer."
  (let ((uri (prompt1 :prompt "Star service URI"
                      :input "star://"
                      :sources 'prompter:raw-source)))
    (ffi-buffer-load (make-buffer-focus) uri)))
