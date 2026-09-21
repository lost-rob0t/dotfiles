;;; kb-roam.el --- audited Org-roam ingestion and publishing -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-id)
(require 'ox-html)
(require 'seq)
(require 'subr-x)

(defconst kb-roam-metadata-schema "org-roam-meta/v1")
(defconst kb-roam-generic-tags
  '("ai" "misc" "note" "notes" "general" "stuff" "thing" "things" "todo" "todos" "kb"))
(defconst kb-roam-reserved-tags '("private" "noexport"))

(defun kb-roam--env (name fallback)
  (let ((value (getenv name)))
    (if (and value (not (string-empty-p value))) value fallback)))

(defun kb-roam-root ()
  (file-name-as-directory
   (expand-file-name
    (kb-roam--env "KB_ROAM_ROOT"
                  (kb-roam--env "GPT_TODOS_NOTES_DIR" "~/Documents/Notes/org")))))

(defun kb-roam-publish-root ()
  (file-name-as-directory
   (expand-file-name
    (kb-roam--env "KB_ROAM_PUBLISH_ROOT" "~/.local/state/kb-roam/site"))))

(defun kb-roam--slug (value)
  (let* ((down (downcase (string-trim value)))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" down)))
    (string-trim slug "-" "-")))

(defun kb-roam--normalize-tag (tag)
  (let* ((down (downcase (string-trim tag)))
         (norm (replace-regexp-in-string "[^[:alnum:]_-]+" "_" down)))
    (string-trim norm "_" "_")))

(defun kb-roam--split-tags (value)
  (delete-dups
   (seq-filter
    (lambda (x) (not (string-empty-p x)))
    (mapcar #'kb-roam--normalize-tag
            (split-string (or value "") "[,[:space:]]+" t)))))

(defun kb-roam--topical-tags (tags)
  (seq-remove
   (lambda (tag)
     (or (member tag kb-roam-generic-tags)
         (member tag kb-roam-reserved-tags)
         (string-prefix-p "ai_" tag)))
   tags))

(defun kb-roam--validate-tags (tags)
  (let ((topic (kb-roam--topical-tags tags)))
    (when (< (length topic) 2)
      (error "Need at least two specific topical tags; got %S" tags))
    (when (> (length topic) 8)
      (error "Too many topical tags; maximum is 8"))
    tags))

(defun kb-roam--ai-tag ()
  (concat "ai_" (substring (secure-hash 'sha256 (org-id-new)) 0 16)))

(defun kb-roam--keyword (name)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote name))
             nil t)
        (string-trim (match-string-no-properties 1))))))

(defun kb-roam--filetags ()
  (kb-roam--split-tags
   (replace-regexp-in-string ":" " " (or (kb-roam--keyword "FILETAGS") ""))))

(defun kb-roam--file-visibility ()
  (let ((explicit (downcase (or (kb-roam--keyword "ROAM_VISIBILITY") "")))
        (legacy (downcase (or (kb-roam--keyword "PUBLISH") "")))
        (private-keyword (kb-roam--bool-keyword "PRIVATE"))
        (private-tag (member "private" (kb-roam--filetags))))
    (cond
     ((or private-keyword private-tag) "private")
     ((member explicit '("public" "private" "unlisted")) explicit)
     ((member legacy '("t" "true" "yes" "public")) "public")
     (t "private"))))

(defun kb-roam--bool-keyword (name)
  (member (downcase (or (kb-roam--keyword name) ""))
          '("t" "true" "yes" "1")))

(defun kb-roam--effective-heading-visibility ()
  (if (or (member "private" (org-get-tags nil t))
          (member "noexport" (org-get-tags nil t))
          (member (downcase (or (org-entry-get nil "PRIVATE" 'inherit) ""))
                  '("t" "true" "yes" "1"))
          (string= "private"
                   (downcase (or (org-entry-get nil "VISIBILITY" 'inherit) ""))))
      "private"
    "public"))

(defun kb-roam--note-path (title kind)
  (let* ((now (current-time))
         (year (format-time-string "%Y" now))
         (month (format-time-string "%m" now))
         (stamp (format-time-string "%Y%m%dT%H%M%S" now))
         (base (if (string= kind "session") "ai/sessions" "kb"))
         (dir (expand-file-name (format "%s/%s/%s" base year month) (kb-roam-root))))
    (make-directory dir t)
    (expand-file-name (format "%s-%s.org" stamp (kb-roam--slug title)) dir)))

(defun kb-roam-write-note (title body tags kind source visibility ai-generated &optional session-id)
  (setq tags (kb-roam--validate-tags tags))
  (let* ((ai-tag (when ai-generated (kb-roam--ai-tag)))
         (all-tags (append (when ai-generated (list "ai" ai-tag)) tags))
         (path (kb-roam--note-path title kind))
         (id (org-id-new))
         (now (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (with-temp-file path
      (insert ":PROPERTIES:\n:ID: " id "\n:END:\n")
      (insert "#+TITLE: " title "\n")
      (insert "#+CREATED: " now "\n#+LAST_MODIFIED: " now "\n")
      (insert "#+ROAM_SCHEMA: " kb-roam-metadata-schema "\n")
      (insert "#+ROAM_KIND: " kind "\n#+ROAM_VISIBILITY: " visibility "\n")
      (insert "#+CENSOR_PROFILE: strict\n")
      (when (and source (not (string-empty-p source))) (insert "#+SOURCE: " source "\n"))
      (when ai-generated
        (insert "#+AI_GENERATED: t\n#+AI_TAG: " ai-tag "\n")
        (when session-id (insert "#+AI_SESSION_ID: " session-id "\n")))
      (insert "#+FILETAGS: :" (string-join all-tags ":") ":\n\n")
      (insert body)
      (unless (string-suffix-p "\n" body) (insert "\n")))
    (when (fboundp 'org-roam-db-sync) (ignore-errors (org-roam-db-sync)))
    path))

(defun kb-roam-cli-ingest ()
  (let* ((title (kb-roam--env "KB_ROAM_TITLE" ""))
         (tags (kb-roam--split-tags (kb-roam--env "KB_ROAM_TAGS" "")))
         (body-file (kb-roam--env "KB_ROAM_BODY_FILE" ""))
         (kind (kb-roam--env "KB_ROAM_KIND" "note"))
         (source (kb-roam--env "KB_ROAM_SOURCE" ""))
         (visibility (kb-roam--env "KB_ROAM_VISIBILITY" "private"))
         (ai-generated (string= (downcase (kb-roam--env "KB_ROAM_AI" "false")) "true")))
    (when (string-empty-p title) (error "KB_ROAM_TITLE is required"))
    (unless (member visibility '("private" "public" "unlisted")) (error "Invalid visibility %s" visibility))
    (unless (file-readable-p body-file) (error "Unreadable KB_ROAM_BODY_FILE"))
    (kb-roam-write-note title
                        (with-temp-buffer (insert-file-contents body-file) (buffer-string))
                        tags kind source visibility ai-generated)))

(defun kb-roam-cli-session-export ()
  (let* ((session-id (kb-roam--env "KB_ROAM_SESSION_ID" ""))
         (json-file (kb-roam--env "KB_ROAM_SESSION_JSON" "")))
    (when (string-empty-p session-id) (error "KB_ROAM_SESSION_ID is required"))
    (unless (file-readable-p json-file) (error "Unreadable session JSON"))
    (let* ((raw (with-temp-buffer (insert-file-contents json-file) (buffer-string)))
           (title (format "OpenCode session %s" session-id))
           (body (concat "* Sanitized OpenCode session export\n\n#+begin_src json :exports code\n"
                         raw (unless (string-suffix-p "\n" raw) "\n") "#+end_src\n")))
      (kb-roam-write-note title body '("opencode" "session") "session"
                          (concat "opencode:" session-id) "private" t session-id))))

(defun kb-roam--org-files ()
  (directory-files-recursively (kb-roam-root) "\\.org\\'"))

(defun kb-roam--manifest-entry (file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let* ((rel (file-relative-name file (kb-roam-root)))
           (title (or (kb-roam--keyword "TITLE") (file-name-base file)))
           (schema (or (kb-roam--keyword "ROAM_SCHEMA") ""))
           (kind (or (kb-roam--keyword "ROAM_KIND")
                     (if (string-prefix-p "daily/" rel) "daily" "note")))
           (visibility (kb-roam--file-visibility))
           (ai-generated (kb-roam--bool-keyword "AI_GENERATED"))
           (ai-tag (or (kb-roam--keyword "AI_TAG") ""))
           (censor (or (kb-roam--keyword "CENSOR_PROFILE") "strict"))
           (tags (kb-roam--filetags))
           headings links)
      (org-map-entries
       (lambda ()
         (push `((id . ,(or (org-entry-get nil "ID") ""))
                 (level . ,(org-outline-level))
                 (title . ,(org-get-heading t t t t))
                 (visibility . ,(kb-roam--effective-heading-visibility)))
               headings))
       nil 'file)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let ((raw (org-element-property :raw-link link)))
            (when raw (push raw links)))))
      `((id . ,(secure-hash 'sha1 rel))
        (path . ,rel) (title . ,title) (schema . ,schema) (kind . ,kind)
        (visibility . ,visibility)
        (ai_generated . ,(if ai-generated t :json-false))
        (ai_tag . ,ai-tag) (censor_profile . ,censor)
        (tags . ,(vconcat tags))
        (headings . ,(vconcat (nreverse headings)))
        (links . ,(vconcat (delete-dups (nreverse links))))))))

(defun kb-roam-manifest ()
  `((schema . "kb-roam-manifest/v1")
    (files . ,(vconcat (mapcar #'kb-roam--manifest-entry (kb-roam--org-files))))))

(defun kb-roam-cli-manifest ()
  (let ((out (kb-roam--env "KB_ROAM_MANIFEST" "")))
    (when (string-empty-p out) (error "KB_ROAM_MANIFEST is required"))
    (make-directory (file-name-directory out) t)
    (with-temp-file out (insert (json-encode (kb-roam-manifest)) "\n"))
    out))

(defun kb-roam--mark-private-headlines ()
  (org-map-entries
   (lambda ()
     (when (string= "private" (kb-roam--effective-heading-visibility))
       (org-toggle-tag "private" 'on)))
   nil 'file))

(defun kb-roam--export-file (file)
  (let* ((rel (file-relative-name file (kb-roam-root)))
         (html-rel (concat (file-name-sans-extension rel) ".html"))
         (out (expand-file-name html-rel (kb-roam-publish-root)))
         (base (kb-roam--env "ROAM_PUBLISH_BASE_URL" "/")))
    (make-directory (file-name-directory out) t)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (kb-roam--mark-private-headlines)
      (let ((org-export-exclude-tags (delete-dups (append '("private" "noexport") org-export-exclude-tags)))
            (org-html-head (format "<link rel=\"stylesheet\" href=\"%sassets/roam.css\">" base))
            (org-html-preamble (format "<header class=\"site-head\"><a href=\"%s\">Roam</a><nav><a href=\"%sdaily/\">Dailies</a></nav></header>" base base))
            (org-html-postamble "<footer class=\"site-foot\">Published from Org-roam · private content removed before export</footer>")
            (org-export-with-broken-links 'mark))
        (org-export-to-file 'html out nil nil nil nil '(:with-toc t :section-numbers nil))))
    html-rel))

(defun kb-roam--escape (value) (org-html-encode-plain-text (or value "")))

(defun kb-roam--card (entry)
  (let* ((path (alist-get 'path entry))
         (href (concat "/" (file-name-sans-extension path) ".html"))
         (title (alist-get 'title entry))
         (kind (alist-get 'kind entry))
         (tags (append (alist-get 'tags entry) nil))
         (ai (eq (alist-get 'ai_generated entry) t)))
    (format "<article class=\"card\"><div class=\"eyebrow\">%s%s</div><h2><a href=\"%s\">%s</a></h2><div class=\"tags\">%s</div></article>"
            (kb-roam--escape kind) (if ai " · AI" "") href (kb-roam--escape title)
            (mapconcat (lambda (tag) (format "<span>#%s</span>" (kb-roam--escape tag))) tags " "))))

(defun kb-roam--write-index (entries out title intro)
  (make-directory (file-name-directory out) t)
  (with-temp-file out
    (insert "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">")
    (insert "<title>" (kb-roam--escape title) "</title><link rel=\"stylesheet\" href=\"/assets/roam.css\"></head><body>")
    (insert "<header class=\"site-head\"><a href=\"/\">Roam</a><nav><a href=\"/daily/\">Dailies</a></nav></header>")
    (insert "<main class=\"shell\"><section class=\"hero\"><p class=\"eyebrow\">published graph</p><h1>"
            (kb-roam--escape title) "</h1><p>" (kb-roam--escape intro) "</p></section><section class=\"grid\">")
    (dolist (entry entries) (insert (kb-roam--card entry)))
    (insert "</section></main><footer class=\"site-foot\">Org-roam publishing suite</footer></body></html>")))

(defun kb-roam-cli-publish ()
  (let* ((files (append (alist-get 'files (kb-roam-manifest)) nil))
         (public (seq-filter (lambda (entry) (string= "public" (alist-get 'visibility entry))) files))
         (daily (seq-filter (lambda (entry) (string= "daily" (alist-get 'kind entry))) public))
         (asset-dir (kb-roam--env "KB_ROAM_ASSET_DIR" ""))
         (asset-out (expand-file-name "assets" (kb-roam-publish-root))))
    (make-directory (kb-roam-publish-root) t)
    (dolist (entry public)
      (kb-roam--export-file (expand-file-name (alist-get 'path entry) (kb-roam-root))))
    (when (and (not (string-empty-p asset-dir)) (file-readable-p (expand-file-name "roam.css" asset-dir)))
      (make-directory asset-out t)
      (copy-file (expand-file-name "roam.css" asset-dir) (expand-file-name "roam.css" asset-out) t))
    (kb-roam--write-index public (expand-file-name "index.html" (kb-roam-publish-root))
                          "Roam" "Public notes from the graph. Everything else stays private by default.")
    (kb-roam--write-index daily (expand-file-name "daily/index.html" (kb-roam-publish-root))
                          "Dailies" "A public slice of the daily graph.")
    (kb-roam-publish-root)))

(provide 'kb-roam)
;;; kb-roam.el ends here
