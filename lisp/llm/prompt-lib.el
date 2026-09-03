;;; prompt-lib.el --- Versioned prompt library client -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
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

(defcustom ai/prompt-lib-catalog-name "catalog.json"
  "Catalog filename relative to `ai/prompt-lib-directory'."
  :type 'string
  :group 'ai/prompt-lib)

(defvar ai/prompt-lib--records-cache nil)
(defvar ai/prompt-lib--active-directory nil)

(defun ai/prompt-lib--catalog-path ()
  "Return the configured prompt catalog path."
  (expand-file-name ai/prompt-lib-catalog-name ai/prompt-lib-directory))

(defun ai/prompt-lib--prompts-directory ()
  "Return the external prompt body directory."
  (expand-file-name "prompts/" ai/prompt-lib-directory))

(defun ai/prompt-lib--object-get (object key)
  "Read KEY from JSON alist OBJECT regardless of string/symbol keys."
  (cdr (or (assq key object)
           (assoc (symbol-name key) object))))

(defun ai/prompt-lib--normalize-tags (tags)
  "Return TAGS as a list of non-empty strings."
  (cl-remove-if #'string-empty-p
                (mapcar (lambda (tag) (format "%s" tag)) (or tags nil))))

(defun ai/prompt-lib--catalog-records ()
  "Read and validate records from the external catalog."
  (let ((catalog (ai/prompt-lib--catalog-path))
        records)
    (when (file-readable-p catalog)
      (with-temp-buffer
        (insert-file-contents catalog)
        (let* ((data (json-parse-buffer :object-type 'alist :array-type 'list
                                        :null-object nil :false-object nil))
               (schema (ai/prompt-lib--object-get data 'schema)))
          (unless (equal schema "prompts-lib.catalog.v1")
            (user-error "Unsupported prompts-lib catalog schema: %S" schema))
          (dolist (entry (ai/prompt-lib--object-get data 'prompts))
            (let* ((id (format "%s" (ai/prompt-lib--object-get entry 'id)))
                   (relative (format "%s" (ai/prompt-lib--object-get entry 'path)))
                   (path (expand-file-name relative ai/prompt-lib-directory)))
              (unless (file-in-directory-p path ai/prompt-lib-directory)
                (user-error "Prompt escapes library root: %s" relative))
              (push (list :id id
                          :title (or (ai/prompt-lib--object-get entry 'title) id)
                          :description (or (ai/prompt-lib--object-get entry 'description) "")
                          :tags (ai/prompt-lib--normalize-tags
                                 (ai/prompt-lib--object-get entry 'tags))
                          :aliases (ai/prompt-lib--normalize-tags
                                    (ai/prompt-lib--object-get entry 'aliases))
                          :path path
                          :source 'catalog)
                    records))))))
    (nreverse records)))

(defun ai/prompt-lib--prompt-files (directory)
  "Return prompt files recursively below DIRECTORY."
  (when (file-directory-p directory)
    (directory-files-recursively
     directory (concat (regexp-quote ai/prompt-template-extension) "\\'") nil)))

(defun ai/prompt-lib--title-from-path (path)
  "Build a readable title from prompt PATH."
  (capitalize
   (replace-regexp-in-string "[-_.]+" " " (file-name-base path))))

(defun ai/prompt-lib--uncataloged-records (records)
  "Return prompt records not already represented by RECORDS."
  (let* ((known (mapcar (lambda (record) (file-truename (plist-get record :path)))
                        records))
         (directory ai/prompt-template-directory)
         extras)
    (dolist (path (ai/prompt-lib--prompt-files directory))
      (unless (member (file-truename path) known)
        (push (list :id (file-name-base path)
                    :title (ai/prompt-lib--title-from-path path)
                    :description "Uncataloged prompt template"
                    :tags '("uncataloged")
                    :aliases nil
                    :path path
                    :source 'scan)
              extras)))
    (nreverse extras)))

(defun ai/prompt-lib-records (&optional refresh)
  "Return prompt records, optionally forcing REFRESH."
  (when refresh
    (setq ai/prompt-lib--records-cache nil))
  (or ai/prompt-lib--records-cache
      (let* ((catalog (ai/prompt-lib--catalog-records))
             (records (append catalog (ai/prompt-lib--uncataloged-records catalog))))
        (setq ai/prompt-lib--records-cache records))))

(defun ai/prompt-lib-activate ()
  "Use the external prompts-lib checkout when it is available."
  (interactive)
  (let ((directory (ai/prompt-lib--prompts-directory)))
    (when (file-directory-p directory)
      (setq ai/prompt-template-directory directory
            ai/prompt-lib--active-directory directory))
    (ai/prompt-lib-records 'refresh)
    (when (called-interactively-p 'interactive)
      (message "Prompt library: %s (%d prompts)"
               ai/prompt-template-directory
               (length (ai/prompt-lib-records))))))

(defun ai/prompt-lib-refresh ()
  "Refresh the prompt library from disk."
  (interactive)
  (ai/prompt-lib-activate))

(defun ai/prompt-lib--record-search-text (record)
  "Return completion search text for RECORD."
  (string-join
   (delq nil
         (list (plist-get record :id)
               (plist-get record :title)
               (plist-get record :description)
               (string-join (plist-get record :tags) " ")
               (string-join (plist-get record :aliases) " ")))
   " "))

(defun ai/prompt-lib--completion-candidates ()
  "Return completion candidates paired with prompt records."
  (mapcar
   (lambda (record)
     (let ((label (format "%s  [%s]  %s"
                          (plist-get record :title)
                          (plist-get record :id)
                          (string-join (plist-get record :tags) ","))))
       (cons (propertize label 'ai/prompt-search
                         (ai/prompt-lib--record-search-text record))
             record)))
   (ai/prompt-lib-records)))

(defun ai/prompt-lib-read-record (&optional prompt)
  "Read and return a prompt record using PROMPT."
  (let ((candidates (ai/prompt-lib--completion-candidates)))
    (unless candidates
      (user-error "No prompt templates found"))
    (cdr (assoc (completing-read (or prompt "Prompt: ") candidates nil t)
                candidates))))

(defun ai/prompt-lib--read-template (record)
  "Read prompt body for RECORD."
  (let ((path (plist-get record :path)))
    (unless (file-readable-p path)
      (user-error "Prompt file is not readable: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun ai/prompt-lib-render-record (record)
  "Fill placeholders and return rendered prompt RECORD."
  (let ((template (ai/prompt-lib--read-template record)))
    (ai/prompt-template-render-string
     template (ai/prompt-template--ask-values template))))

(defun ai/prompt-lib--preview-string (rendered)
  "Show RENDERED prompt in the prompt preview buffer."
  (with-current-buffer (get-buffer-create "*Prompt Library Preview*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert rendered)
      (goto-char (point-min))
      (text-mode))
    (pop-to-buffer (current-buffer))))

(defun ai/prompt-lib-render (&optional record)
  "Render RECORD or read one interactively and preview it."
  (interactive)
  (let ((rendered (ai/prompt-lib-render-record
                   (or record (ai/prompt-lib-read-record)))))
    (ai/prompt-lib--preview-string rendered)
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
  "Open prompt RECORD for editing."
  (interactive)
  (find-file
   (plist-get (or record (ai/prompt-lib-read-record "Edit prompt: ")) :path)))

(defun ai/prompt-lib-open-repository ()
  "Open the prompt library repository in Dired."
  (interactive)
  (unless (file-directory-p ai/prompt-lib-directory)
    (user-error "Prompt library checkout does not exist: %s" ai/prompt-lib-directory))
  (dired ai/prompt-lib-directory))

(defun ai/prompt-lib-new (name)
  "Create uncataloged prompt NAME in the active prompt directory."
  (interactive "sNew prompt name: ")
  (ai/prompt-template-new name))

(defun ai/prompt-lib--record-by-id (id)
  "Return prompt record matching ID."
  (cl-find id (ai/prompt-lib-records) :key (lambda (record) (plist-get record :id))
           :test #'equal))

(defun ai/prompt-lib--entry-record ()
  "Return the prompt record at point in the browser."
  (or (ai/prompt-lib--record-by-id (tabulated-list-get-id))
      (user-error "No prompt on this line")))

(defun ai/prompt-lib-browser-preview ()
  "Preview the prompt at point."
  (interactive)
  (ai/prompt-lib-render (ai/prompt-lib--entry-record)))

(defun ai/prompt-lib-browser-copy ()
  "Copy the prompt at point."
  (interactive)
  (ai/prompt-lib-copy (ai/prompt-lib--entry-record)))

(defun ai/prompt-lib-browser-insert ()
  "Insert the prompt at point in the previously selected window."
  (interactive)
  (let ((record (ai/prompt-lib--entry-record)))
    (quit-window)
    (ai/prompt-lib-insert record)))

(defun ai/prompt-lib-browser-edit ()
  "Edit the prompt at point."
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
                         (symbol-name (plist-get record :source)))))
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
         ("Source" 10 t)]
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
    ("n" "New" ai/prompt-lib-new)
    ("g" "Refresh" ai/prompt-lib-refresh)
    ("o" "Open repo" ai/prompt-lib-open-repository)]
   ["Image tools"
    ("G" "Generate from prompt" ai/image-generate)
    ("T" "Generate from template" ai/image-generate-template)
    ("X" "Edit image" ai/image-edit)]])

(defalias 'ai/prompt-menu #'ai/prompt-lib-menu)

(ai/prompt-lib-activate)

(provide 'prompt-lib)
;;; prompt-lib.el ends here
