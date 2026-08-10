;;; ai-prompts.el --- Reusable prompt templates for the LLM stack -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

(defgroup ai/prompts nil
  "Reusable prompt templates."
  :group 'applications
  :prefix "ai/prompt-")

(defconst ai/prompt--module-directory
  (file-name-directory (or load-file-name buffer-file-name user-emacs-directory)))

(defcustom ai/prompt-template-directory
  (expand-file-name "prompts/" ai/prompt--module-directory)
  "Directory containing reusable prompt templates."
  :type 'directory
  :group 'ai/prompts)

(defcustom ai/prompt-template-extension ".prompt"
  "Filename extension used by prompt templates."
  :type 'string
  :group 'ai/prompts)

(defconst ai/prompt--field-regexp
  "{{\\([[:alnum:]_.-]+\\)\\(?:|\\([^}\n]*\\)\\)?}}")

(defun ai/prompt-template-files ()
  "Return prompt template files in `ai/prompt-template-directory'."
  (when (file-directory-p ai/prompt-template-directory)
    (directory-files
     ai/prompt-template-directory t
     (concat (regexp-quote ai/prompt-template-extension) "\\'") t)))

(defun ai/prompt-template-names ()
  "Return available prompt template names."
  (mapcar #'file-name-base (ai/prompt-template-files)))

(defun ai/prompt-template-read-name (&optional prompt)
  "Read an existing template name using PROMPT."
  (let ((names (ai/prompt-template-names)))
    (unless names
      (user-error "No prompt templates in %s" ai/prompt-template-directory))
    (completing-read (or prompt "Prompt template: ") names nil t)))

(defun ai/prompt-template-path (name)
  "Return the path for template NAME."
  (expand-file-name (concat name ai/prompt-template-extension)
                    ai/prompt-template-directory))

(defun ai/prompt-template--read (name)
  "Read template NAME as text."
  (let ((file (ai/prompt-template-path name)))
    (unless (file-readable-p file)
      (user-error "Prompt template does not exist: %s" name))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun ai/prompt-template--fields (template)
  "Return unique placeholder fields from TEMPLATE in appearance order."
  (let ((start 0)
        fields)
    (while (string-match ai/prompt--field-regexp template start)
      (let ((name (match-string 1 template))
            (default (match-string 2 template)))
        (unless (assoc-string name fields)
          (setq fields (append fields (list (cons name default)))))
        (setq start (match-end 0))))
    fields))

(defun ai/prompt-template--ask-values (template)
  "Prompt for every placeholder value in TEMPLATE."
  (mapcar
   (lambda (field)
     (let* ((name (car field))
            (default (cdr field))
            (label (replace-regexp-in-string "[_-]+" " " name))
            (prompt (if (and default (not (string-empty-p default)))
                        (format "%s [%s]: " label default)
                      (format "%s: " label))))
       (cons name (read-string prompt nil nil default))))
   (ai/prompt-template--fields template)))

(defun ai/prompt-template-render-string (template values)
  "Render TEMPLATE using placeholder VALUES.
VALUES is an alist mapping placeholder names to strings."
  (let ((rendered template)
        (start 0))
    (while (string-match ai/prompt--field-regexp rendered start)
      (let* ((name (match-string 1 rendered))
             (default (or (match-string 2 rendered) ""))
             (entry (assoc-string name values))
             (value (or (cdr entry) default)))
        (setq rendered (replace-match value t t rendered))
        (setq start (+ (match-beginning 0) (length value)))))
    rendered))

(defun ai/prompt-template-render (&optional name)
  "Fill and return prompt template NAME.
When called interactively, show the rendered prompt in a preview buffer."
  (interactive)
  (let* ((name (or name (ai/prompt-template-read-name)))
         (template (ai/prompt-template--read name))
         (rendered (ai/prompt-template-render-string
                    template (ai/prompt-template--ask-values template))))
    (when (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*AI Prompt Preview*")
        (erase-buffer)
        (insert rendered)
        (goto-char (point-min))
        (text-mode)
        (pop-to-buffer (current-buffer))))
    rendered))

(defun ai/prompt-template-copy (&optional name)
  "Fill template NAME and copy the result to the kill ring."
  (interactive)
  (let ((rendered (ai/prompt-template-render name)))
    (kill-new rendered)
    (message "Prompt copied (%d chars)" (length rendered))))

(defun ai/prompt-template-insert (&optional name)
  "Fill template NAME and insert the result at point."
  (interactive)
  (insert (ai/prompt-template-render name)))

(defun ai/prompt-template-edit (&optional name)
  "Open template NAME for editing."
  (interactive)
  (find-file (ai/prompt-template-path
              (or name (ai/prompt-template-read-name "Edit template: ")))))

(defun ai/prompt-template-new (name &optional initial)
  "Create prompt template NAME, optionally seeded with INITIAL text."
  (interactive
   (list (read-string "New template name: ")
         (when (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
  (unless (string-match-p "\\`[[:alnum:]_.-]+\\'" name)
    (user-error "Template names may contain only letters, numbers, ., _, and -"))
  (make-directory ai/prompt-template-directory t)
  (let ((file (ai/prompt-template-path name)))
    (when (file-exists-p file)
      (user-error "Prompt template already exists: %s" name))
    (write-region (or initial "") nil file nil 'silent)
    (find-file file)))

(declare-function ai/image-generate "ai-image")
(declare-function ai/image-generate-template "ai-image")
(declare-function ai/image-edit "ai-image")

(transient-define-prefix ai/prompt-menu ()
  "Prompt-template and image-generation commands."
  [["Prompt templates"
    ("c" "Copy filled prompt" ai/prompt-template-copy)
    ("i" "Insert filled prompt" ai/prompt-template-insert)
    ("p" "Preview filled prompt" ai/prompt-template-render)
    ("e" "Edit template" ai/prompt-template-edit)
    ("n" "New template" ai/prompt-template-new)]
   ["OpenAI image tools"
    ("g" "Generate from prompt" ai/image-generate)
    ("t" "Generate from template" ai/image-generate-template)
    ("x" "Edit image" ai/image-edit)]])

(provide 'ai-prompts)
;;; ai-prompts.el ends here
