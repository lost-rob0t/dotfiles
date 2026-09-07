;;; prompt-lib.el --- Org-first prompt library client -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'ai-prompts)

(defgroup ai/prompt-lib nil
  "Client for lost-rob0t/prompts-lib."
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
  "Default format for newly created prompts."
  :type '(choice (const org) (const markdown) (const lisp))
  :group 'ai/prompt-lib)

(defvar ai/prompt-lib--records-cache nil)
(defvar ai/prompt-lib--active-directory nil)

(defun ai/prompt-lib--external-directory ()
  "Return the canonical external prompt directory."
  (expand-file-name "prompts/" ai/prompt-lib-directory))

(defun ai/prompt-lib--root ()
  "Return the active prompt directory."
  (or ai/prompt-lib--active-directory ai/prompt-template-directory))

(defun ai/prompt-lib--format (path)
  "Return the prompt format represented by PATH."
  (pcase (downcase (or (file-name-extension path) ""))
    ("org" 'org)
    ((or "md" "markdown") 'markdown)
    ("el" 'lisp)
    ("prompt" 'legacy)
    (_ nil)))

(defun ai/prompt-lib--files ()
  "Return supported prompt files beneath the active root."
  (let (files)
    (when (file-directory-p (ai/prompt-lib--root))
      (dolist (path (directory-files-recursively (ai/prompt-lib--root) "." nil))
        (when (ai/prompt-lib--format path)
          (push path files))))
    (nreverse files)))

(defun ai/prompt-lib--title-from-path (path)
  "Return a readable title for PATH."
  (capitalize (replace-regexp-in-string "[-_.]+" " " (file-name-base path))))

(defun ai/prompt-lib--split (value separator)
  "Split VALUE on SEPARATOR, trimming empty entries."
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
  "Return the top-level `Prompt' subtree body from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Prompt[ \t]*$" nil t)
      (user-error "Prompt file has no top-level * Prompt heading: %s"
                  (or buffer-file-name (buffer-name))))
    (forward-line 1)
    (let ((start (point)))
      (if (re-search-forward "^\\* [^*]" nil t)
          (string-trim-right
           (buffer-substring-no-properties start (match-beginning 0)))
        (string-trim-right
         (buffer-substring-no-properties start (point-max)))))))

(defun ai/prompt-lib--parse-org (path)
  "Parse canonical Org prompt PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((filetags (ai/prompt-lib--org-keyword "filetags")))
      (list :id (or (ai/prompt-lib--org-keyword "prompt_id")
                    (file-name-base path))
            :title (or (ai/prompt-lib--org-keyword "title")
                       (ai/prompt-lib--title-from-path path))
            :description (or (ai/prompt-lib--org-keyword "description") "")
            :tags (ai/prompt-lib--split
                   (string-trim (or filetags "") ":" ":") ":")
            :aliases (ai/prompt-lib--split
                      (ai/prompt-lib--org-keyword "prompt_aliases") "|")
            :body (ai/prompt-lib--org-body)
            :path path
            :format 'org))))

(defun ai/prompt-lib--markdown-metadata ()
  "Return supported flat Markdown frontmatter as an alist."
  (save-excursion
    (goto-char (point-min))
    (let (result)
      (when (looking-at-p "---[ \t]*$")
        (forward-line 1)
        (while (and (not (eobp)) (not (looking-at-p "---[ \t]*$")))
          (when (looking-at "\\([^:#\n]+\\):[ \t]*\\(.*\\)$")
            (push (cons (downcase (string-trim (match-string-no-properties 1)))
                        (string-trim (match-string-no-properties 2)))
                  result))
          (forward-line 1)))
      result)))

(defun ai/prompt-lib--markdown-body ()
  "Return body below a Markdown `# Prompt' heading."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^# Prompt[ \t]*$" nil t)
      (user-error "Markdown prompt has no # Prompt heading"))
    (forward-line 1)
    (let ((start (point)))
      (if (re-search-forward "^# [^#]" nil t)
          (string-trim-right
           (buffer-substring-no-properties start (match-beginning 0)))
        (string-trim-right
         (buffer-substring-no-properties start (point-max)))))))

(defun ai/prompt-lib--parse-markdown (path)
  "Parse Markdown prompt PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((metadata (ai/prompt-lib--markdown-metadata))
           (get (lambda (key) (cdr (assoc key metadata)))))
      (list :id (or (funcall get "prompt_id") (file-name-base path))
            :title (or (funcall get "title") (ai/prompt-lib--title-from-path path))
            :description (or (funcall get "description") "")
            :tags (ai/prompt-lib--split (funcall get "tags") ",")
            :aliases (ai/prompt-lib--split (funcall get "aliases") "|")
            :body (ai/prompt-lib--markdown-body)
            :path path
            :format 'markdown))))

(defun ai/prompt-lib--parse-lisp (path)
  "Parse declarative Lisp prompt PATH without evaluating it."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      (unless (and (listp data) (plist-get data :prompt))
        (user-error "Lisp prompt must be a plist containing :prompt: %s" path))
      (list :id (or (plist-get data :id) (file-name-base path))
            :title (or (plist-get data :title) (ai/prompt-lib--title-from-path path))
            :description (or (plist-get data :description) "")
            :tags (mapcar #'format "%s" (plist-get data :tags))
            :aliases (mapcar #'format "%s" (plist-get data :aliases))
            :body (format "%s" (plist-get data :prompt))
            :path path
            :format 'lisp))))

(defun ai/prompt-lib--parse-legacy (path)
  "Parse legacy raw prompt PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (list :id (file-name-base path)
          :title (ai/prompt-lib--title-from-path path)
          :description "Legacy prompt"
          :tags '("legacy")
          :aliases nil
          :body (buffer-string)
          :path path
          :format 'legacy)))

(defun ai/prompt-lib--parse (path)
  "Parse PATH into a normalized prompt record."
  (pcase (ai/prompt-lib--format path)
    ('org (ai/prompt-lib--parse-org path))
    ('markdown (ai/prompt-lib--parse-markdown path))
    ('lisp (ai/prompt-lib--parse-lisp path))
    ('legacy (ai/prompt-lib--parse-legacy path))))

(defun ai/prompt-lib-records (&optional refresh)
  "Return normalized records; REFRESH invalidates the cache."
  (when refresh
    (setq ai/prompt-lib--records-cache nil))
  (or ai/prompt-lib--records-cache
      (setq ai/prompt-lib--records-cache
            (delq nil (mapcar #'ai/prompt-lib--parse (ai/prompt-lib--files))))))

(defun ai/prompt-lib-activate ()
  "Activate external prompts-lib when present, otherwise use legacy prompts."
  (interactive)
  (let ((external (ai/prompt-lib--external-directory)))
    (setq ai/prompt-lib--active-directory
          (if (file-directory-p external) external ai/prompt-template-directory))
    (ai/prompt-lib-records 'refresh)
    (when (called-interactively-p 'interactive)
      (message "Prompt library: %s (%d prompts)"
               ai/prompt-lib--active-directory
               (length (ai/prompt-lib-records))))))

(defun ai/prompt-lib-refresh ()
  "Refresh prompt records from disk."
  (interactive)
  (ai/prompt-lib-activate))

(defun ai/prompt-lib--by-name (name)
  "Find prompt NAME by id, title, basename, or alias."
  (cl-find-if
   (lambda (record)
     (or (equal name (plist-get record :id))
         (equal name (plist-get record :title))
         (equal name (file-name-base (plist-get record :path)))
         (member name (plist-get record :aliases))))
   (ai/prompt-lib-records)))

(defun ai/prompt-lib-read-record (&optional label)
  "Select a prompt record using completion with LABEL."
  (let* ((records (ai/prompt-lib-records))
         (candidates
          (mapcar (lambda (record)
                    (cons (format "%s  [%s]  %s"
                                  (plist-get record :title)
                                  (plist-get record :id)
                                  (string-join (plist-get record :tags) ","))
                          record))
                  records)))
    (unless candidates
      (user-error "No prompts found in %s" (ai/prompt-lib--root)))
    (cdr (assoc (completing-read (or label "Prompt: ") candidates nil t)
                candidates))))

(defun ai/prompt-lib-template-names ()
  "Return prompt IDs for ai-prompts compatibility."
  (mapcar (lambda (record) (plist-get record :id)) (ai/prompt-lib-records)))

(defun ai/prompt-lib-template-read (name)
  "Return prompt body for NAME."
  (let ((record (ai/prompt-lib--by-name name)))
    (unless record (user-error "Unknown prompt: %s" name))
    (plist-get record :body)))

(defun ai/prompt-lib-template-path (name)
  "Return prompt path for NAME."
  (let ((record (ai/prompt-lib--by-name name)))
    (unless record (user-error "Unknown prompt: %s" name))
    (plist-get record :path)))

(defun ai/prompt-lib-render-record (record)
  "Render RECORD, prompting for placeholders."
  (let ((body (plist-get record :body)))
    (ai/prompt-template-render-string body (ai/prompt-template--ask-values body))))

(defun ai/prompt-lib--system-copy (text)
  "Put TEXT in the kill ring and GUI/system clipboard when available."
  (kill-new text)
  (condition-case nil
      (when (fboundp 'gui-set-selection)
        (gui-set-selection 'CLIPBOARD text))
    (error nil))
  text)

(defun ai/prompt-lib-copy (&optional record)
  "Render RECORD and copy it to the system clipboard."
  (interactive)
  (let ((rendered (ai/prompt-lib-render-record
                   (or record (ai/prompt-lib-read-record "Copy prompt: ")))))
    (ai/prompt-lib--system-copy rendered)
    (message "Prompt copied (%d chars)" (length rendered))
    rendered))

(defun ai/prompt-lib-preview (&optional record)
  "Render RECORD and show a preview buffer."
  (interactive)
  (let ((rendered (ai/prompt-lib-render-record
                   (or record (ai/prompt-lib-read-record "Preview prompt: ")))))
    (with-current-buffer (get-buffer-create "*Prompt Library Preview*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert rendered)
        (goto-char (point-min))
        (text-mode))
      (pop-to-buffer (current-buffer)))
    rendered))

(defun ai/prompt-lib-insert (&optional record)
  "Render RECORD and insert it at point."
  (interactive)
  (insert (ai/prompt-lib-render-record
           (or record (ai/prompt-lib-read-record "Insert prompt: ")))))

(defun ai/prompt-lib-send (&optional record)
  "Render RECORD and send it through gptel."
  (interactive)
  (require 'gptel)
  (gptel-send
   (ai/prompt-lib-render-record
    (or record (ai/prompt-lib-read-record "Send prompt: ")))))

(defun ai/prompt-lib-edit (&optional record)
  "Open RECORD for editing."
  (interactive)
  (find-file (plist-get (or record (ai/prompt-lib-read-record "Edit prompt: "))
                        :path)))

(defun ai/prompt-lib-open-repository ()
  "Open the local prompts-lib checkout."
  (interactive)
  (unless (file-directory-p ai/prompt-lib-directory)
    (user-error "prompts-lib checkout missing: %s" ai/prompt-lib-directory))
  (dired ai/prompt-lib-directory))

(defun ai/prompt-lib--slug (name)
  "Return a safe slug for NAME."
  (let ((slug (downcase (string-trim name))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]._-]+" "-" slug))
    (replace-regexp-in-string "^-+\\|-+$" "" slug)))

(defun ai/prompt-lib-new (name)
  "Create a canonical Org prompt called NAME."
  (interactive "sNew prompt name: ")
  (let* ((root (ai/prompt-lib--root))
         (slug (ai/prompt-lib--slug name))
         (path (expand-file-name (concat slug ".org") root)))
    (make-directory root t)
    (when (file-exists-p path)
      (user-error "Prompt already exists: %s" path))
    (write-region
     (format "#+title: %s\n#+prompt_id: %s\n#+description: \n#+filetags: :prompt:\n#+prompt_aliases: \n\n* Prompt\n\n"
             name slug)
     nil path nil 'silent)
    (ai/prompt-lib-records 'refresh)
    (find-file path)))

(defun ai/prompt-lib--record-at-point ()
  "Return browser record at point."
  (or (ai/prompt-lib--by-name (tabulated-list-get-id))
      (user-error "No prompt on this row")))

(defun ai/prompt-lib-browser-preview ()
  (interactive)
  (ai/prompt-lib-preview (ai/prompt-lib--record-at-point)))

(defun ai/prompt-lib-browser-copy ()
  (interactive)
  (ai/prompt-lib-copy (ai/prompt-lib--record-at-point)))

(defun ai/prompt-lib-browser-edit ()
  (interactive)
  (ai/prompt-lib-edit (ai/prompt-lib--record-at-point)))

(defun ai/prompt-lib-browser-refresh ()
  (interactive)
  (ai/prompt-lib-refresh)
  (ai/prompt-lib--browser-entries)
  (tabulated-list-print t))

(defun ai/prompt-lib--browser-entries ()
  "Populate browser rows."
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
    (define-key map (kbd "e") #'ai/prompt-lib-browser-edit)
    (define-key map (kbd "g") #'ai/prompt-lib-browser-refresh)
    map))

(define-derived-mode ai/prompt-lib-browser-mode tabulated-list-mode "Prompt-Lib"
  "Browse reusable prompts."
  (setq tabulated-list-format
        [("ID" 38 t) ("Title" 34 t) ("Tags" 42 t) ("Format" 10 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Title" nil))
  (ai/prompt-lib--browser-entries)
  (tabulated-list-init-header))

(defun ai/prompt-lib-browse ()
  "Open the prompt library browser."
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
    ("p" "Preview" ai/prompt-lib-preview)
    ("e" "Edit" ai/prompt-lib-edit)
    ("n" "New Org prompt" ai/prompt-lib-new)
    ("g" "Refresh" ai/prompt-lib-refresh)
    ("o" "Open repo" ai/prompt-lib-open-repository)]
   ["Image tools"
    ("G" "Generate from prompt" ai/image-generate)
    ("T" "Generate from template" ai/image-generate-template)
    ("X" "Edit image" ai/image-edit)]])

(defalias 'ai/prompt-template-names #'ai/prompt-lib-template-names)
(defalias 'ai/prompt-template--read #'ai/prompt-lib-template-read)
(defalias 'ai/prompt-template-path #'ai/prompt-lib-template-path)
(defalias 'ai/prompt-menu #'ai/prompt-lib-menu)

(ai/prompt-lib-activate)

(provide 'prompt-lib)
;;; prompt-lib.el ends here
