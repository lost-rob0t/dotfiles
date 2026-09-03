;;; prompt-lib.el --- Org-first versioned prompt library client -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'ai-prompts)

(defgroup ai/prompt-lib nil
  "Client for the external prompts-lib repository."
  :group 'ai/prompts
  :prefix "ai/prompt-lib-")

(defcustom ai/prompt-lib-directory
  (file-name-as-directory
   (expand-file-name
    (or (getenv "PROMPTS_LIB_DIR") "~/Documents/Projects/prompts-lib")))
  "Local checkout of the prompts-lib repository."
  :type 'directory
  :group 'ai/prompt-lib)

(defcustom ai/prompt-lib-default-format 'org
  "Default file format for newly created prompts."
  :type '(choice (const :tag "Org" org)
                 (const :tag "Markdown" markdown)
                 (const :tag "Lisp data" lisp))
  :group 'ai/prompt-lib)

(defcustom ai/prompt-lib-formats '(org markdown lisp legacy)
  "Prompt formats discovered by the library client."
  :type '(set (const org) (const markdown) (const lisp) (const legacy))
  :group 'ai/prompt-lib)

(defvar ai/prompt-lib--records-cache nil)
(defvar ai/prompt-lib--active-directory nil)

(defun ai/prompt-lib--prompts-directory ()
  "Return the external prompt directory."
  (expand-file-name "prompts/" ai/prompt-lib-directory))

(defun ai/prompt-lib--library-root ()
  "Return the active prompt directory."
  (or ai/prompt-lib--active-directory ai/prompt-template-directory))

(defun ai/prompt-lib--format-extension (format)
  "Return filename extension for FORMAT."
  (pcase format
    ('org "org")
    ('markdown "md")
    ('lisp "el")
    ('legacy "prompt")
    (_ (user-error "Unsupported prompt format: %S" format))))

(defun ai/prompt-lib--path-format (path)
  "Return prompt format for PATH or nil."
  (pcase (downcase (or (file-name-extension path) ""))
    ("org" 'org)
    ((or "md" "markdown") 'markdown)
    ("el" 'lisp)
    ("prompt" 'legacy)
    (_ nil)))

(defun ai/prompt-lib--prompt-files ()
  "Return prompt files below the active library root."
  (let ((root (ai/prompt-lib--library-root))
        files)
    (when (file-directory-p root)
      (dolist (path (directory-files-recursively root "." nil))
        (let ((format (ai/prompt-lib--path-format path)))
          (when (and format (memq format ai/prompt-lib-formats))
            (push path files)))))
    (nreverse files)))

(defun ai/prompt-lib--title-from-path (path)
  "Build a readable title from PATH."
  (capitalize
   (replace-regexp-in-string "[-_.]+" " " (file-name-base path))))

(defun ai/prompt-lib--split-list (value separator)
  "Split VALUE by SEPARATOR and trim empty items."
  (when value
    (cl-remove-if #'string-empty-p
                  (mapcar #'string-trim (split-string value separator)))))

(defun ai/prompt-lib--org-keyword (name)
  "Read Org keyword NAME from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote name)) nil t)
      (string-trim (match-string-no-properties 1)))))

(defun ai/prompt-lib--org-body ()
  "Return the body of the top-level Org `Prompt' heading."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Prompt[ \t]*$" nil t)
      (user-error "Org prompt has no top-level `* Prompt' heading: %s"
                  (or buffer-file-name (buffer-name))))
    (forward-line 1)
    (let ((start (point)))
      (if (re-search-forward "^\\* [^*]" nil t)
          (string-trim-right
           (buffer-substring-no-properties start (match-beginning 0)))
        (string-trim-right
         (buffer-substring-no-properties start (point-max)))))))

(defun ai/prompt-lib--parse-org (path)
  "Parse Org prompt PATH into a normalized record."
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((id (or (ai/prompt-lib--org-keyword "prompt_id")
                   (file-name-base path)))
           (title (or (ai/prompt-lib--org-keyword "title")
                      (ai/prompt-lib--title-from-path path)))
           (description (or (ai/prompt-lib--org-keyword "description") ""))
           (filetags (ai/prompt-lib--org-keyword "filetags"))
           (aliases (ai/prompt-lib--org-keyword "prompt_aliases")))
      (list :id id
            :title title
            :description description
            :tags (ai/prompt-lib--split-list
                   (string-trim (or filetags "") ":" ":") ":")
            :aliases (ai/prompt-lib--split-list aliases "|")
            :body (ai/prompt-lib--org-body)
            :path path
            :format 'org))))

(defun ai/prompt-lib--markdown-frontmatter ()
  "Return flat Markdown frontmatter as an alist.
The supported syntax is deliberately limited to `key: value' lines between
opening and closing `---' delimiters."
  (save-excursion
    (goto-char (point-min))
    (let (metadata)
      (when (looking-at-p "---[ \t]*$")
        (forward-line 1)
        (while (and (not (eobp)) (not (looking-at-p "---[ \t]*$")))
          (when (looking-at "\\([^:#\n]+\\):[ \t]*\\(.*\\)$")
            (push (cons (downcase (string-trim (match-string-no-properties 1)))
                        (string-trim (match-string-no-properties 2)))
                  metadata))
          (forward-line 1)))
      metadata)))

(defun ai/prompt-lib--markdown-body ()
  "Return body below a Markdown `# Prompt' heading."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^# Prompt[ \t]*$" nil t)
      (user-error "Markdown prompt has no `# Prompt' heading: %s"
                  (or buffer-file-name (buffer-name))))
    (forward-line 1)
    (let ((start (point)))
      (if (re-search-forward "^# [^#]" nil t)
          (string-trim-right
           (buffer-substring-no-properties start (match-beginning 0)))
        (string-trim-right
         (buffer-substring-no-properties start (point-max)))))))

(defun ai/prompt-lib--parse-markdown (path)
  "Parse Markdown prompt PATH into a normalized record."
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((metadata (ai/prompt-lib--markdown-frontmatter))
           (get (lambda (key) (cdr (assoc key metadata))))
           (id (or (funcall get "prompt_id") (file-name-base path)))
           (title (or (funcall get "title") (ai/prompt-lib--title-from-path path))))
      (list :id id
            :title title
            :description (or (funcall get "description") "")
            :tags (ai/prompt-lib--split-list (funcall get "tags") ",")
            :aliases (ai/prompt-lib--split-list (funcall get "aliases") "|")
            :body (ai/prompt-lib--markdown-body)
            :path path
            :format 'markdown))))

(defun ai/prompt-lib--parse-lisp (path)
  "Parse declarative Lisp prompt PATH without evaluating it."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((data (condition-case error
                    (read (current-buffer))
                  (error (user-error "Invalid Lisp prompt %s: %s"
                                     path (error-message-string error))))))
      (unless (and (listp data) (plist-get data :prompt))
        (user-error "Lisp prompt must be a plist containing :prompt: %s" path))
      (list :id (or (plist-get data :id) (file-name-base path))
            :title (or (plist-get data :title) (ai/prompt-lib--title-from-path path))
            :description (or (plist-get data :description) "")
            :tags (mapcar (lambda (tag) (format "%s" tag)) (plist-get data :tags))
            :aliases (mapcar (lambda (alias) (format "%s" alias))
                             (plist-get data :aliases))
            :body (format "%s" (plist-get data :prompt))
            :path path
            :format 'lisp))))

(defun ai/prompt-lib--parse-legacy (path)
  "Parse legacy raw `.prompt' PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (list :id (file-name-base path)
          :title (ai/prompt-lib--title-from-path path)
          :description "Legacy raw prompt"
          :tags '("legacy")
          :aliases nil
          :body (buffer-string)
          :path path
          :format 'legacy)))

(defun ai/prompt-lib--parse-file (path)
  "Parse prompt PATH according to its extension."
  (pcase (ai/prompt-lib--path-format path)
    ('org (ai/prompt-lib--parse-org path))
    ('markdown (ai/prompt-lib--parse-markdown path))
    ('lisp (ai/prompt-lib--parse-lisp path))
    ('legacy (ai/prompt-lib--parse-legacy path))
    (_ nil)))

(defun ai/prompt-lib-records (&optional refresh)
  "Return normalized prompt records, optionally forcing REFRESH."
  (when refresh
    (setq ai/prompt-lib--records-cache nil))
  (or ai/prompt-lib--records-cache
      (setq ai/prompt-lib--records-cache
            (delq nil (mapcar #'ai/prompt-lib--parse-file
                              (ai/prompt-lib--prompt-files))))))

(defun ai/prompt-lib-activate ()
  "Activate external prompts-lib when its prompt directory is available."
  (interactive)
  (let ((external (ai/prompt-lib--prompts-directory)))
    (setq ai/prompt-lib--active-directory
          (if (file-directory-p external)
              external
            ai/prompt-template-directory))
    (when (file-directory-p external)
      (setq ai/prompt-template-directory external))
    (ai/prompt-lib-records 'refresh)
    (when (called-interactively-p 'interactive)
      (message "Prompt library: %s (%d prompts)"
               ai/prompt-lib--active-directory
               (length (ai/prompt-lib-records))))))

(defun ai/prompt-lib-refresh ()
  "Refresh prompt records from disk."
  (interactive)
  (ai/prompt-lib-activate))

(defun ai/prompt-lib--completion-candidates ()
  "Return completion candidates paired with prompt records."
  (mapcar
   (lambda (record)
     (let* ((aliases (string-join (plist-get record :aliases) ", "))
            (description (plist-get record :description))
            (label (format "%s  [%s]  %s%s"
                           (plist-get record :title)
                           (plist-get record :id)
                           (string-join (plist-get record :tags) ",")
                           (if (string-empty-p aliases) ""
                             (format "  {%s}" aliases)))))
       (cons (propertize label 'ai/prompt-description description) record)))
   (ai/prompt-lib-records)))

(defun ai/prompt-lib-read-record (&optional prompt)
  "Read and return a prompt record using PROMPT."
  (let ((candidates (ai/prompt-lib--completion-candidates)))
    (unless candidates
      (user-error "No prompts found in %s" (ai/prompt-lib--library-root)))
    (cdr (assoc (completing-read (or prompt "Prompt: ") candidates nil t)
                candidates))))

(defun ai/prompt-lib--record-by-name (name)
  "Find a prompt record by ID, file base name, title, or alias NAME."
  (cl-find-if
   (lambda (record)
     (or (equal name (plist-get record :id))
         (equal name (file-name-base (plist-get record :path)))
         (equal name (plist-get record :title))
         (member name (plist-get record :aliases))))
   (ai/prompt-lib-records)))

(defun ai/prompt-lib-template-names ()
  "Return prompt IDs for compatibility with `ai-prompts'."
  (mapcar (lambda (record) (plist-get record :id)) (ai/prompt-lib-records)))

(defun ai/prompt-lib-template-read (name)
  "Return raw prompt body for NAME."
  (let ((record (ai/prompt-lib--record-by-name name)))
    (unless record
      (user-error "Unknown prompt: %s" name))
    (plist-get record :body)))

(defun ai/prompt-lib-template-path (name)
  "Return prompt path for NAME."
  (let ((record (ai/prompt-lib--record-by-name name)))
    (unless record
      (user-error "Unknown prompt: %s" name))
    (plist-get record :path)))

(defun ai/prompt-lib-render-record (record)
  "Fill placeholders and return rendered prompt RECORD."
  (let ((template (plist-get record :body)))
    (ai/prompt-template-render-string
     template (ai/prompt-template--ask-values template))))

(defun ai/prompt-lib--preview-string (rendered format)
  "Show RENDERED prompt using FORMAT-appropriate major mode."
  (with-current-buffer (get-buffer-create "*Prompt Library Preview*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert rendered)
      (goto-char (point-min))
      (pcase format
        ('org (org-mode))
        ('markdown (if (fboundp 'markdown-mode) (markdown-mode) (text-mode)))
        ('lisp (emacs-lisp-mode))
        (_ (text-mode))))
    (pop-to-buffer (current-buffer))))

(defun ai/prompt-lib-render (&optional record)
  "Render RECORD or read one interactively and preview it."
  (interactive)
  (let* ((record (or record (ai/prompt-lib-read-record)))
         (rendered (ai/prompt-lib-render-record record)))
    (ai/prompt-lib--preview-string rendered (plist-get record :format))
    rendered))

(defun ai/prompt-lib-copy (&optional record)
  "Render RECORD and copy it to the kill ring."
  (interactive)
  (let ((rendered (ai/prompt-lib-render-record
                   (or record (ai/prompt-lib-read-record "Copy prompt: ")))))
    (kill-new rendered)
    (message "Prompt copied (%d chars)" (length rendered))))

(defun ai/prompt-lib-insert (&optional record)
  "Render RECORD and insert it at point."
  (interactive)
  (insert (ai/prompt-lib-render-record
           (or record (ai/prompt-lib-read-record "Insert prompt: ")))))

(defun ai/prompt-lib-send (&optional record)
  "Render RECORD and submit it through gptel."
  (interactive)
  (require 'gptel)
  (gptel-send
   (ai/prompt-lib-render-record
    (or record (ai/prompt-lib-read-record "Send prompt: ")))))

(defun ai/prompt-lib-edit (&optional record)
  "Open prompt RECORD for editing in its native major mode."
  (interactive)
  (find-file
   (plist-get (or record (ai/prompt-lib-read-record "Edit prompt: ")) :path)))

(defun ai/prompt-lib-open-repository ()
  "Open the prompt library repository in Dired."
  (interactive)
  (unless (file-directory-p ai/prompt-lib-directory)
    (user-error "Prompt library checkout does not exist: %s" ai/prompt-lib-directory))
  (dired ai/prompt-lib-directory))

(defun ai/prompt-lib--slug (name)
  "Return filesystem-safe slug derived from NAME."
  (let ((slug (downcase (string-trim name))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]._-]+" "-" slug))
    (replace-regexp-in-string "^-+\\|-+$" "" slug)))

(defun ai/prompt-lib--new-org (path id title)
  "Create Org prompt PATH with ID and TITLE."
  (write-region
   (format "#+title: %s\n#+prompt_id: %s\n#+description: \n#+filetags: :prompt:\n#+prompt_aliases: \n\n* Prompt\n\n"
           title id)
   nil path nil 'silent))

(defun ai/prompt-lib--new-markdown (path id title)
  "Create Markdown prompt PATH with ID and TITLE."
  (write-region
   (format "---\nprompt_id: %s\ntitle: %s\ndescription: \ntags: prompt\naliases: \n---\n\n# Prompt\n\n"
           id title)
   nil path nil 'silent))

(defun ai/prompt-lib--new-lisp (path id title)
  "Create declarative Lisp prompt PATH with ID and TITLE."
  (write-region
   (format "(:id %S\n :title %S\n :description \"\"\n :tags (\"prompt\")\n :aliases ()\n :prompt \"\")\n"
           id title)
   nil path nil 'silent))

(defun ai/prompt-lib-new (name &optional format)
  "Create prompt NAME using FORMAT.
Org is the default.  With a prefix argument, choose Markdown or Lisp instead."
  (interactive
   (list (read-string "New prompt name: ")
         (when current-prefix-arg
           (intern (completing-read "Format: " '("org" "markdown" "lisp") nil t)))))
  (let* ((format (or format ai/prompt-lib-default-format))
         (root (ai/prompt-lib--library-root))
         (slug (ai/prompt-lib--slug name))
         (id slug)
         (path (expand-file-name
                (format "%s.%s" slug (ai/prompt-lib--format-extension format)) root)))
    (unless (file-directory-p root)
      (make-directory root t))
    (when (file-exists-p path)
      (user-error "Prompt already exists: %s" path))
    (pcase format
      ('org (ai/prompt-lib--new-org path id name))
      ('markdown (ai/prompt-lib--new-markdown path id name))
      ('lisp (ai/prompt-lib--new-lisp path id name)))
    (ai/prompt-lib-records 'refresh)
    (find-file path)))

(defun ai/prompt-lib--record-by-id (id)
  "Return prompt record matching ID."
  (cl-find id (ai/prompt-lib-records)
           :key (lambda (record) (plist-get record :id)) :test #'equal))

(defun ai/prompt-lib--entry-record ()
  "Return prompt record at point in the browser."
  (or (ai/prompt-lib--record-by-id (tabulated-list-get-id))
      (user-error "No prompt on this line")))

(defun ai/prompt-lib-browser-preview ()
  "Preview prompt at point."
  (interactive)
  (ai/prompt-lib-render (ai/prompt-lib--entry-record)))

(defun ai/prompt-lib-browser-copy ()
  "Copy prompt at point."
  (interactive)
  (ai/prompt-lib-copy (ai/prompt-lib--entry-record)))

(defun ai/prompt-lib-browser-insert ()
  "Insert prompt at point in the previously selected window."
  (interactive)
  (let ((record (ai/prompt-lib--entry-record)))
    (quit-window)
    (ai/prompt-lib-insert record)))

(defun ai/prompt-lib-browser-edit ()
  "Edit prompt at point."
  (interactive)
  (ai/prompt-lib-edit (ai/prompt-lib--entry-record)))

(defun ai/prompt-lib-browser-refresh ()
  "Refresh records shown in the prompt browser."
  (interactive)
  (ai/prompt-lib-refresh)
  (ai/prompt-lib--browser-entries)
  (tabulated-list-print t))

(defun ai/prompt-lib--browser-entries ()
  "Populate `tabulated-list-entries' from prompt records."
  (setq tabulated-list-entries
        (mapcar
         (lambda (record)
           (list (plist-get record :id)
                 (vector (plist-get record :id)
                         (plist-get record :title)
                         (string-join (plist-get record :tags) ", ")
                         (symbol-name (plist-get record :format)))))
         (ai/prompt-lib-records))))

(defvar ai/prompt-lib-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ai/prompt-lib-browser-preview)
    (define-key map (kbd "c") #'ai/prompt-lib-browser-copy)
    (define-key map (kbd "i") #'ai/prompt-lib-browser-insert)
    (define-key map (kbd "e") #'ai/prompt-lib-browser-edit)
    (define-key map (kbd "g") #'ai/prompt-lib-browser-refresh)
    map)
  "Keymap for `ai/prompt-lib-browser-mode'.")

(define-derived-mode ai/prompt-lib-browser-mode tabulated-list-mode "Prompt-Lib"
  "Browse reusable prompts."
  (setq tabulated-list-format
        [("ID" 34 t)
         ("Title" 30 t)
         ("Tags" 36 t)
         ("Format" 10 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Title" nil))
  (ai/prompt-lib--browser-entries)
  (tabulated-list-init-header))

(defun ai/prompt-lib-browse ()
  "Open the reusable prompt library browser."
  (interactive)
  (ai/prompt-lib-refresh)
  (let ((buffer (get-buffer-create "*Prompt Library*")))
    (with-current-buffer buffer
      (ai/prompt-lib-browser-mode)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(declare-function ai/image-generate "ai-image")
(declare-function ai/image-generate-template "ai-image")
(declare-function ai/image-edit "ai-image")

(transient-define-prefix ai/prompt-lib-menu ()
  "Prompt library and image-generation commands."
  [["Prompt library"
    ("b" "Browse" ai/prompt-lib-browse)
    ("c" "Copy" ai/prompt-lib-copy)
    ("i" "Insert" ai/prompt-lib-insert)
    ("s" "Send to gptel" ai/prompt-lib-send)
    ("p" "Preview" ai/prompt-lib-render)
    ("e" "Edit" ai/prompt-lib-edit)
    ("n" "New Org prompt" ai/prompt-lib-new)
    ("g" "Refresh" ai/prompt-lib-refresh)
    ("o" "Open repo" ai/prompt-lib-open-repository)]
   ["Image tools"
    ("G" "Generate from prompt" ai/image-generate)
    ("T" "Generate from template" ai/image-generate-template)
    ("X" "Edit image" ai/image-edit)]])

;; Existing image/chat integrations call these ai-prompts functions.  Route
;; their lookup through the normalized multi-format library without changing
;; the existing placeholder renderer.
(defalias 'ai/prompt-template-names #'ai/prompt-lib-template-names)
(defalias 'ai/prompt-template--read #'ai/prompt-lib-template-read)
(defalias 'ai/prompt-template-path #'ai/prompt-lib-template-path)
(defalias 'ai/prompt-menu #'ai/prompt-lib-menu)

(ai/prompt-lib-activate)

(provide 'prompt-lib)
;;; prompt-lib.el ends here
