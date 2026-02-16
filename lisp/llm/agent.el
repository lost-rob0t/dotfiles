;;; agent.el --- agent tool system -*- lexical-binding: t; -*-

;; Author: nsaspy 
;; Version: 0.2
;; Package-Requires: ((emacs "29.1") (gptel "0.9") (plz "0.9"))

;;; Commentary:
;; A Set of tools for LLM workflows.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'plz)

(let ((ai-lib (expand-file-name "ai.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p ai-lib)
    (load ai-lib nil t)))


(defun ai/--expand-file (filename)
  "Expand FILENAME, handling TRAMP paths correctly."
  (if (file-remote-p filename)
      filename
    (expand-file-name filename)))

(defun ai/--file-exists (filename)
  "Check if FILENAME exists."
  (file-exists-p (ai/--expand-file filename)))

(defun ai/--read-file (filename)
  "Read contents of FILENAME."
  (let ((file (ai/--expand-file filename)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      (error "File not found: %s" filename))))

(defun ai/--write-file (filename content)
  "Write CONTENT to FILENAME atomically."
  (let ((file (ai/--expand-file filename)))
    (with-temp-file file
      (insert content))
    "File written successfully"))

(defun ai/--file-backup (filename)
  "Create .bak backup of FILENAME."
  (let ((file (ai/--expand-file filename)))
    (when (file-exists-p file)
      (let ((backup (concat file ".bak")))
        (copy-file file backup t)
        backup))))

(defun ai/--list-files (&optional path)
  "List files in PATH (defaults to current directory)."
  (let ((dir (if (or (null path) (string= path ""))
                 default-directory
               (if (file-remote-p path)
                   path
                 (expand-file-name path)))))
    (if (file-directory-p dir)
        (directory-files dir nil "^[^.].*")
      (error "Not a directory: %s" dir))))

(defun ai/--read-file-numbered (filename)
  "Read FILENAME with line numbers."
  (let* ((content (ai/--read-file filename))
         (lines (split-string content "\n"))
         (i 0))
    (mapconcat (lambda (line)
                 (setq i (1+ i))
                 (format "%4d| %s" i line))
               lines "\n")))

(defun ai/--get-file-lines (filename start end)
  "Get lines from START to END in FILENAME."
  (let* ((content (ai/--read-file filename))
         (lines (split-string content "\n"))
         (num-lines (length lines))
         ;; Ensure start/end are within bounds
         (start-line (max 1 (min start num-lines)))
         (end-line (max start-line (min end num-lines)))
         ;; Extract subset (nth is 0-indexed, lines are 1-indexed)
         (subset (seq-subseq lines (1- start-line) end-line))
         (i (1- start-line)))
    (mapconcat (lambda (line)
                 (setq i (1+ i))
                 (format "%4d| %s" i line))
               subset "\n")))

(defun ai/--search-replace (filename search replace &optional replace-all)
  "Search and replace in FILENAME."
  (let ((file (ai/--expand-file filename)))
    (if (file-exists-p file)
        (let ((count 0))
          (with-temp-file file
            (insert-file-contents (ai/--expand-file filename))
            (goto-char (point-min))
            (while (search-forward search nil t)
              (replace-match replace)
              (setq count (1+ count))
              (unless replace-all (goto-char (point-max)))))
          (format "Replaced %d occurrences" count))
      (error "File not found: %s" filename))))

(defun ai/--apply-line-changes (filename changes)
  "Apply CHANGES to FILENAME. CHANGES is a list of plists with :line and :content."
  (let ((file (ai/--expand-file filename)))
    (if (file-exists-p file)
        (let ((lines (with-temp-buffer
                       (insert-file-contents file)
                       (split-string (buffer-string) "\n")))
              (updates (make-hash-table :test 'eql))
              (count 0))
          ;; Index updates
          (dolist (change changes)
            (puthash (plist-get change :line) (plist-get change :content) updates))
          ;; Write back
          (with-temp-file file
            (let ((i 0))
              (dolist (line lines)
                (setq i (1+ i))
                (if-let ((new-content (gethash i updates)))
                    (progn
                      (insert new-content "\n")
                      (setq count (1+ count)))
                  (insert line "\n")))))
          (format "Applied %d line changes to %s" count filename))
      (error "File not found: %s" filename))))

(defun ai/--preview-changes (filename changes)
  "Preview CHANGES to FILENAME without applying."
  (let ((file (ai/--expand-file filename)))
    (if (file-exists-p file)
        (let ((lines (with-temp-buffer
                       (insert-file-contents file)
                       (split-string (buffer-string) "\n")))
              (output ""))
          (dolist (change changes)
            (let* ((line-num (plist-get change :line))
                   (new-content (plist-get change :content))
                   (old-content (if (<= line-num (length lines))
                                    (nth (1- line-num) lines)
                                  "[EOF]")))
              (setq output (concat output
                                   (format "Line %d:\n- %s\n+ %s\n\n"
                                           line-num (or old-content "") new-content)))))
          (if (string-empty-p output) "No changes to preview" output))
      (error "File not found: %s" filename))))

;;; Helper Functions - Diff/Patch

(defun ai/--diff-files (file1 file2)
  "Generate unified diff between FILE1 and FILE2."
  (let ((f1 (ai/--expand-file file1))
        (f2 (ai/--expand-file file2)))
    (if (and (file-exists-p f1) (file-exists-p f2))
        (with-temp-buffer
          (call-process "diff" nil t nil "-u" f1 f2)
          (buffer-string))
      (error "One or both files not found: %s, %s" file1 file2))))

(defun ai/--git-diff (mode &optional target)
  "Run git diff in MODE (staged, unstaged, ref) for TARGET."
  (let ((args (list "diff"))
        (default-directory (ai/--project-root)))
    (cond
     ((string= mode "staged") (push "--cached" args))
     ((string= mode "unstaged") nil) ;; default
     ((string= mode "ref") (when target (push target args)))) ;; ref is target

    ;; Append target file path if specific file requested (and not handled by ref)
    (when (and target (not (string= mode "ref")))
      (push "--" args)
      (push target args))

    (with-temp-buffer
      (apply #'call-process "git" nil t nil (nreverse args))
      (buffer-string))))

(defun ai/--diff-changes (filename)
  "Diff current FILENAME content against HEAD."
  (ai/--git-diff "unstaged" filename))

(defun ai/--count-patch-target-files (patch)
  "Return number of target files referenced by unified PATCH text."
  (with-temp-buffer
    (insert patch)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^diff --git " nil t)
        (setq count (1+ count)))
      (when (zerop count)
        (goto-char (point-min))
        (while (re-search-forward "^\\+\\+\\+ [ab]/" nil t)
          (setq count (1+ count))))
      count)))

(defun ai/--apply-patch (patch &optional dry-run)
  "Apply PATCH (string) to files. Enforces single-file apply unless specified."
  (let ((patch-file (make-temp-file "ai-patch")))
    (unwind-protect
        (progn
          (with-temp-file patch-file
            (insert patch))

          (when (> (ai/--count-patch-target-files patch) 1)
            (error "Multi-file patches are not allowed by default. Apply one file at a time."))

          (let ((args (append
                       '("apply" "--ignore-space-change" "--ignore-whitespace")
                       (when dry-run '("--check"))
                       (list patch-file)))
                (default-directory (ai/--project-root)))
            (with-temp-buffer
              (let ((exit-code (apply #'call-process "git" nil t nil args)))
                (if (zerop exit-code)
                    (if dry-run
                        "Patch applies cleanly (dry-run)"
                      "Patch applied successfully")
                  (format "Patch failed:\n%s" (buffer-string)))))))
      (when (file-exists-p patch-file)
        (delete-file patch-file)))))

(defun ai/--apply-diff-patch (patch &optional dry-run)
  "Alias for `ai/--apply-patch'."
  (ai/--apply-patch patch dry-run))

;;; Helper Functions - Emacs Introspection

(defun ai/--eval-elisp (expression)
  "Evaluate elisp EXPRESSION and return formatted result."
  (format "%S" (eval (read expression))))

(defun ai/--symbol-exists (name)
  "Check if symbol NAME exists in obarray."
  (intern-soft name))

(defun ai/--function-source (symbol)
  "Get source code for SYMBOL function."
  (when-let ((sym (intern-soft symbol)))
    (ai/--introspect-source sym nil)))

(defun ai/--variable-source (symbol)
  "Get source code for SYMBOL variable."
  (when-let ((sym (intern-soft symbol)))
    (ai/--introspect-source sym 'defvar)))

(defun ai/--introspect-source (symbol &optional type)
  "Get source code for SYMBOL of TYPE."
  (when-let* ((save-silently t)
              (vc-follow-symlinks t)
              (buffer-point (find-definition-noselect symbol type)))
    (with-current-buffer (car buffer-point)
      (goto-char (cdr buffer-point))
      (buffer-substring-no-properties
       (point)
       (progn
         (if (null type)
             (end-of-defun)
           (cond ((derived-mode-p 'c-mode)
                  (forward-sexp 2)
                  (forward-char))
                 ((derived-mode-p 'emacs-lisp-mode)
                  (forward-sexp))
                 (t (error "Unexpected file mode"))))
         (point))))))

(defun ai/--function-documentation (symbol)
  "Get documentation for SYMBOL function."
  (when-let ((sym (intern-soft symbol)))
    (documentation sym)))

(defun ai/--variable-documentation (symbol)
  "Get documentation for SYMBOL variable."
  (when-let ((sym (intern-soft symbol)))
    (custom-variable-documentation sym)))

(defun ai/--variable-value (symbol)
  "Get global value of SYMBOL variable."
  (when-let ((sym (intern-soft symbol)))
    (default-value sym)))

(defun ai/--function-completions (prefix)
  "Find functions matching PREFIX."
  (require 'orderless)
  (string-join (orderless-filter prefix obarray #'functionp) "\n"))

(defun ai/--variable-completions (prefix)
  "Find variables matching PREFIX."
  (require 'orderless)
  (string-join (orderless-filter prefix obarray #'boundp) "\n"))

(defun ai/--command-completions (prefix)
  "Find commands matching PREFIX."
  (require 'orderless)
  (string-join (orderless-filter prefix obarray #'commandp) "\n"))

(defun ai/--features ()
  "Return loaded features."
  (mapconcat #'symbol-name features "\n"))

(defun ai/--load-paths ()
  "Return load-path directories."
  (string-join load-path "\n"))

(defun ai/--library-source (library-name)
  "Get source code for LIBRARY-NAME."
  (if-let ((library (find-library-name library-name)))
      (with-temp-buffer
        (insert-file-contents library)
        (buffer-string))
    (error "Library not found: %s" library-name)))

(defun ai/--feature-available (feature)
  "Check if FEATURE is loaded or available."
  (if-let ((sym (intern-soft feature)))
      (when (featurep sym) feature)
    (find-library-name feature)))

;;; Helper Functions - Manual/Documentation

(defun ai/--manual-names ()
  "Get list of available Info manuals."
  (json-serialize (vconcat (info--filter-manual-names
                            (info--manual-names nil)))))

(defun ai/--manual-nodes (name)
  "Get list of nodes in manual NAME."
  (json-serialize
   (vconcat
    (mapcar #'car (Info-build-node-completions name)))))

(defun ai/--manual-node-contents (manual node)
  "Get contents of NODE in MANUAL."
  (condition-case err
      (save-window-excursion
        (Info-goto-node (format "(%s)%s" manual node))
        (buffer-substring-no-properties (point-min) (point-max)))
    (user-error
     (error (error-message-string err)))))

(defun ai/--symbol-in-manual (symbol)
  "Get manual contents for SYMBOL."
  (require 'helpful)
  (when-let* ((sym (intern-soft symbol))
              (_in-manual (helpful--in-manual-p sym)))
    (save-window-excursion
      (info-lookup 'symbol sym #'emacs-lisp-mode)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;; Helper Functions - Buffer Operations

(defun ai/--read-buffer (buffer)
  "Read complete contents of BUFFER."
  (unless (buffer-live-p (get-buffer buffer))
    (error "Buffer '%s' does not exist or is not live" buffer))
  (with-current-buffer (get-buffer buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ai/--append-to-buffer (buffer text)
  "Append TEXT to BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (save-excursion
      (goto-char (point-max))
      (insert text)))
  (format "Appended text to buffer %s" buffer))

(defun ai/--create-org-buffer (buffer-name content)
  "Create org-mode BUFFER-NAME with CONTENT."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (org-mode))
    (display-buffer buffer '((display-buffer-reuse-window
                              display-buffer-pop-up-window)
                             (reusable-frames . visible)))
    buffer))

;;; Helper Functions - Context System

(defvar ai/context-root nil
  "Root directory for context files. Cached after first lookup.")

(defun ai/--context-dir ()
  "Find nearest .context directory, with caching."
  (or ai/context-root
      (setq ai/context-root
            (locate-dominating-file default-directory ".context"))))

(defun ai/--context-allowed ()
  "Check if context system is enabled."
  (not (file-exists-p ".no-context")))

(defun ai/--list-context ()
  "List all context files."
  (when-let ((ctx-dir (ai/--context-dir)))
    (directory-files-recursively
     (expand-file-name ".context" ctx-dir)
     "" nil
     (lambda (file)
       (and (file-regular-p file)
            (file-readable-p file))))))

(defun ai/--read-context (filename)
  "Read FILENAME from context directory."
  (when-let ((ctx-dir (ai/--context-dir)))
    (let ((file (expand-file-name filename
                                  (expand-file-name ".context" ctx-dir))))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))))))

(defun ai/--init-context (&optional path)
  "Initialize context directory at PATH."
  (let* ((target (or path default-directory))
         (ctx-dir (expand-file-name ".context" target))
         (index (expand-file-name "00_index.org" ctx-dir)))
    (unless (file-exists-p ctx-dir)
      (make-directory ctx-dir t)
      (let ((default-directory ctx-dir))
        (shell-command-to-string "git init")
        (with-temp-file index
          (insert "#+TITLE: Project Context Index\n")
          (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
          (insert "* Project Context Files\n")
          (insert "This directory contains project context for AI assistance.\n"))
        (shell-command-to-string "git add .")
        (shell-command-to-string "git commit -m 'Initialize context'")))
    "Context initialized"))

(defun ai/--write-context (filename content)
  "Write CONTENT to context FILENAME."
  (when-let ((ctx-dir (ai/--context-dir)))
    (let* ((ctx-path (expand-file-name ".context" ctx-dir))
           (file (expand-file-name filename ctx-path)))
      (with-temp-file file
        (insert content))
      (let ((default-directory ctx-path))
        (shell-command-to-string (format "git add %s" (shell-quote-argument filename)))
        (shell-command-to-string
         (format "git commit -m 'Update: %s'" (shell-quote-argument filename))))
      t)))

(defun ai/--create-context (slug content priority)
  "Create context file with SLUG, CONTENT, and PRIORITY."
  (let ((filename (format "%02d_%s.org" priority slug)))
    (ai/--write-context filename content)))

;;; Helper Functions - Web Operations

(defun ai/--brave-search (query &optional country search-lang count offset)
  "Search using Brave API with QUERY and optional parameters."
  (let* ((api-key (or  (getenv "BRAVE_API_KEY")
                       (nsa/auth-source-get :host "api.brave.com")))
         (country (or country "US"))
         (search-lang (or search-lang "en"))
         (count (or count 5))
         (offset (or offset 0))
         (url (format "https://api.search.brave.com/res/v1/web/search?q=%s&country=%s&search_lang=%s&count=%d&offset=%d"
                      (url-encode-url query)
                      country search-lang count offset)))
    (plz 'get url
      :headers `(("Accept" . "application/json")
                 ("Accept-Encoding" . "gzip")
                 ("X-Subscription-Token" . ,api-key))
      :as #'json-read)))

(defun ai/--http-request (url &optional method headers data)
  "Make HTTP request to URL with METHOD, HEADERS, and DATA."
  (let* ((method (or method "GET"))
         (method-upper (upcase method))
         (cmd (list "curl" "-s" "-L" "--max-time" "30")))

    (unless (string-match-p "^https?://" url)
      (error "Invalid URL: must start with http:// or https://"))

    (push "-X" cmd)
    (push method-upper cmd)

    (dolist (header headers)
      (push "-H" cmd)
      (push (format "%s: %s" (car header) (cdr header)) cmd))

    (when (and data (member method-upper '("POST" "PUT")))
      (push "-d" cmd)
      (push data cmd))

    (push url cmd)

    (shell-command-to-string (string-join (nreverse cmd) " "))))

(defun ai/--http-get (url &optional headers)
  "HTTP GET request to URL with HEADERS."
  (ai/--http-request url "GET" headers))

(defun ai/--http-post (url data &optional headers)
  "HTTP POST DATA to URL with HEADERS."
  (ai/--http-request url "POST" headers data))

;;; Helper Functions - System

(defun ai/--current-dir ()
  "Get current directory."
  default-directory)

(defun ai/--shell-command (command)
  "Execute shell COMMAND and return output."
  (shell-command-to-string command))

(defun ai/--system-info ()
  "Get system information."
  (format "Emacs: %s\nSystem: %s\nOS: %s\nUser: %s\nDir: %s"
          emacs-version
          system-type
          (or (getenv "OS") "Unknown")
          (user-login-name)
          default-directory))

(defun ai/--project-root ()
  "Find project root directory."
  (or (when (fboundp 'projectile-project-root)
        (condition-case nil
            (projectile-project-root)
          (error nil)))
      (when (fboundp 'project-root)
        (condition-case nil
            (project-root (project-current))
          (error nil)))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun ai/--git-status ()
  "Get git status if in git repository."
  (when (file-exists-p ".git")
    (shell-command-to-string "git status --porcelain")))

(defun ai/--current-buffer-info ()
  "Get current buffer info."
  (buffer-name))

(defun ai/--buffer-stats ()
  "Get buffer statistics."
  (format "Buffer: %s\nSize: %d chars\nLine: %d\nPoint: %d"
          (buffer-name)
          (buffer-size)
          (line-number-at-pos)
          (point)))

(defun ai/--recent-files ()
  "Get recently opened files."
  (when (boundp 'recentf-list)
    (string-join recentf-list "\n")))

(defun ai/--open-buffers ()
  "Get info about open buffers."
  (mapconcat
   (lambda (buf)
     (with-current-buffer buf
       (format "%s (%s) - %s"
               (buffer-name)
               (or (buffer-file-name) "no file")
               (if (buffer-modified-p) "modified" "saved"))))
   (buffer-list) "\n"))

;;; Tool Definitions


;; Elisp tools

(gptel-make-tool
 :function #'ai/--eval-elisp
 :name "elisp_eval"
 :category "emacs"
 :confirm t
 :args '((:name "expression" :type string
          :description "Elisp expression to evaluate"))
 :description "Evaluate Elisp expression and return result")


(gptel-make-tool
 :function #'ai/--symbol-exists
 :name "symbol_exists"
 :category "emacs"
 :args '((:name "symbol" :type string
          :description "Symbol name to check"))
 :description "Check if symbol exists in obarray")


(gptel-make-tool
 :function #'ai/--function-source
 :name "function_source"
 :category "emacs"
 :args '((:name "function" :type string
          :description "Function name"))
 :description "Get source code for function")


(gptel-make-tool
 :function #'ai/--variable-source
 :name "variable_source"
 :category "emacs"
 :args '((:name "variable" :type string
          :description "Variable name"))
 :description "Get source code for variable")


(gptel-make-tool
 :function #'ai/--function-documentation
 :name "function_documentation"
 :category "emacs"
 :args '((:name "function" :type string
          :description "Function name"))
 :description "Get documentation for function")


(gptel-make-tool
 :function #'ai/--variable-documentation
 :name "variable_documentation"
 :category "emacs"
 :args '((:name "variable" :type string
          :description "Variable name"))
 :description "Get documentation for variable")


(gptel-make-tool
 :function #'ai/--variable-value
 :name "variable_value"
 :category "emacs"
 :confirm t
 :args '((:name "variable" :type string
          :description "Variable name"))
 :description "Get current value of variable")


(gptel-make-tool
 :function #'ai/--function-completions
 :name "function_completions"
 :category "emacs"
 :args '((:name "prefix" :type string
          :description "Function name prefix"))
 :description "Find functions matching prefix")


(gptel-make-tool
 :function #'ai/--variable-completions
 :name "variable_completions"
 :category "emacs"
 :args '((:name "prefix" :type string
          :description "Variable name prefix"))
 :description "Find variables matching prefix")


(gptel-make-tool
 :function #'ai/--command-completions
 :name "command_completions"
 :category "emacs"
 :args '((:name "prefix" :type string
          :description "Command name prefix"))
 :description "Find commands matching prefix")


(gptel-make-tool
 :function #'ai/--features
 :name "features"
 :category "emacs"
 :confirm t
 :description "Get list of loaded features")


(gptel-make-tool
 :function #'ai/--load-paths
 :name "load_paths"
 :category "emacs"
 :confirm t
 :description "Get Emacs load-path directories")


(gptel-make-tool
 :function #'ai/--library-source
 :name "library_source"
 :category "emacs"
 :args '((:name "library" :type string
          :description "Library name"))
 :description "Get source code for library")


(gptel-make-tool
 :function #'ai/--feature-available
 :name "feature_available"
 :category "emacs"
 :args '((:name "feature" :type string
          :description "Feature name"))
 :description "Check if feature is available")

;; Manual/documentation tools

(gptel-make-tool
 :function #'ai/--manual-names
 :name "manual_names"
 :category "documentation"
 :description "Get list of available Info manuals")


(gptel-make-tool
 :function #'ai/--manual-nodes
 :name "manual_nodes"
 :category "documentation"
 :args '((:name "manual" :type string
          :description "Manual name"))
 :description "Get list of nodes in manual")


(gptel-make-tool
 :function #'ai/--manual-node-contents
 :name "manual_node_contents"
 :category "documentation"
 :args '((:name "manual" :type string
          :description "Manual name")
         (:name "node" :type string
          :description "Node name"))
 :description "Get contents of manual node")


(gptel-make-tool
 :function #'ai/--symbol-in-manual
 :name "symbol_manual_section"
 :category "documentation"
 :args '((:name "symbol" :type string
          :description "Symbol name"))
 :description "Get manual section for symbol")

;; File operations

(gptel-make-tool
 :function #'ai/--file-exists
 :name "file_exists"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path"))
 :description "Check if file exists")


(gptel-make-tool
 :function #'ai/--read-file
 :name "read_file"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path"))
 :description "Read file contents")


(gptel-make-tool
 :function #'ai/--write-file
 :name "write_file"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path")
         (:name "content" :type string
          :description "Content to write"))
 :description "Write content to file. instruct the user on the filepath you used as they can not see it.")

(gptel-make-tool
 :function #'ai/--write-file
 :name "write_org"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path")
         (:name "org_mode_content" :type string
          :description "Content to write"))
 :description "Write org mode content to a file. When  writing Markdown (.md) files, prefer using this tool instead and write the content in Org mode format rather than Markdown. Convert Markdown syntax to equivalent Org mode syntax: use * for headers instead of #, use [[link][description]] for links instead of [description](link), use #+BEGIN_SRC language / #+END_SRC for code blocks instead of ```language, etc. instruct the user on the filepath you used as they can not see it.")




(gptel-make-tool
 :function #'ai/--file-backup
 :name "file_backup"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File to backup"))
 :description "Create backup of file")


(gptel-make-tool
 :function #'ai/--list-files
 :name "list_files"
 :category "filesystem"
 :args '((:name "path" :type string :optional t
          :description "Directory path"))
 :description "List files in directory")


(gptel-make-tool
 :function #'ai/--read-file-numbered
 :name "read_file_numbered"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path to read"))
 :description "Read file content with line numbers. Use this tool before making line-based edits to ensure you have the correct line numbers.")


(gptel-make-tool
 :function #'ai/--get-file-lines
 :name "get_file_lines"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path")
         (:name "start" :type integer
          :description "Start line number (1-based)")
         (:name "end" :type integer
          :description "End line number (1-based)"))
 :description "Read a specific range of lines from a file. Returns the lines with line numbers.")


(gptel-make-tool
 :function (lambda (filename changes)
             (ai/--apply-line-changes 
              filename 
              (mapcar (lambda (u) (list :line (plist-get u :line) :content (plist-get u :content)))
                      changes)))
 :name "apply_line_changes"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path")
         (:name "changes" :type array
          :description "List of line changes, each with :line (number) and :content (string)"))
 :description "Apply specific line-by-line changes to a file. Requires exact line numbers.")

(gptel-make-tool
 :function #'ai/--search-replace
 :name "search_replace"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path")
         (:name "search" :type string
          :description "String to search for")
         (:name "replace" :type string
          :description "Replacement string")
         (:name "replace_all" :type boolean :optional t
          :description "Replace all occurrences (default false)"))
 :description "Search and replace text in a file. Be careful with replacing common strings.")

(gptel-make-tool
 :function (lambda (filename changes)
             (ai/--preview-changes 
              filename 
              (mapcar (lambda (u) (list :line (plist-get u :line) :content (plist-get u :content)))
                      changes)))
 :name "preview_changes"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path")
         (:name "changes" :type array
          :description "List of proposed changes"))
 :description "Preview line changes before applying them.")

(gptel-make-tool
 :function #'ai/--diff-files
 :name "diff_files"
 :category "filesystem"
 :args '((:name "file1" :type string :description "First file path")
         (:name "file2" :type string :description "Second file path"))
 :description "Show unified diff between two files.")

(gptel-make-tool
 :function #'ai/--diff-changes
 :name "diff_changes"
 :category "filesystem"
 :args '((:name "filename" :type string
          :description "File path to diff against HEAD"))
 :description "Show diff for a file's current changes against HEAD.")

(gptel-make-tool
 :function #'ai/--git-diff
 :name "git_diff"
 :category "project"
 :args '((:name "mode" :type string :enum ["staged" "unstaged" "ref"]
          :description "Diff mode: staged (cached), unstaged (working), or ref (commit)")
         (:name "target" :type string :optional t
          :description "File path or ref (commit/branch) to diff against"))
 :description "Run git diff to see changes in the repository.")

(gptel-make-tool
 :function #'ai/--apply-patch
 :name "apply_patch"
 :category "filesystem"
 :args '((:name "patch" :type string
          :description "Unified diff content to apply")
         (:name "dry_run" :type boolean :optional t
          :description "Test patch application without modifying files"))
 :description "Apply a unified patch to files. Single-file patches only by default.")

(gptel-make-tool
 :function #'ai/--apply-diff-patch
 :name "apply_diff_patch"
 :category "filesystem"
 :args '((:name "patch" :type string
          :description "Unified diff content to apply")
         (:name "dry_run" :type boolean :optional t
          :description "Test patch application without modifying files"))
 :description "Apply a unified diff patch. Single-file patches only by default.")


(gptel-make-tool
 :function (lambda (path filename content)
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created %s" filename)))
 :name "create_file"
 :category "filesystem"
 :args '((:name "path" :type string
          :description "Directory path")
         (:name "filename" :type string
          :description "Filename")
         (:name "content" :type string
          :description "File content"))
 :description "Create new file with content")


(gptel-make-tool
 :function (lambda (parent name)
             (make-directory (expand-file-name name parent) t)
             (format "Created directory %s" name))
 :name "make_directory"
 :category "filesystem"
 :args '((:name "parent" :type string
          :description "Parent directory")
         (:name "name" :type string
          :description "Directory name"))
 :description "Create new directory")


(gptel-make-tool
 :function #'ai/--current-dir
 :name "current_directory"
 :category "filesystem"
 :description "Get current working directory")

;; Buffer operations

(gptel-make-tool
 :function #'ai/--read-buffer
 :name "read_buffer"
 :category "buffer"
 :args '((:name "buffer" :type string
          :description "Buffer name"))
 :description "Read complete buffer contents")


(gptel-make-tool
 :function #'ai/--append-to-buffer
 :name "append_to_buffer"
 :category "buffer"
 :args '((:name "buffer" :type string
          :description "Buffer name")
         (:name "text" :type string
          :description "Text to append"))
 :description "Append text to buffer")


(gptel-make-tool
 :function #'buffer-list
 :name "list_buffers"
 :category "buffer"
 :confirm t
 :description "List all open buffers")


(gptel-make-tool
 :function #'ai/--create-org-buffer
 :name "create_org_buffer"
 :category "buffer"
 :args '((:name "buffer_name" :type string
          :description "Buffer name")
         (:name "content" :type string
          :description "Org content"))
 :description "Create org-mode buffer with content. Use this to display a pop up to the user. If you choose to include a summery, best to place it here.")


(gptel-make-tool
 :function #'ai/--current-buffer-info
 :name "current_buffer"
 :category "buffer"
 :description "Get current buffer name")


(gptel-make-tool
 :function #'point
 :name "cursor_position"
 :category "buffer"
 :description "Get cursor position")


(gptel-make-tool
 :function #'ai/--buffer-stats
 :name "buffer_stats"
 :category "buffer"
 :description "Get buffer statistics")

;; Context system
(gptel-make-tool
 :function #'ai/--context-allowed
 :name "context_allowed"
 :category "context"
 :description
 "Check if the project context system is enabled. ALWAYS call this first *only when the user explicitly requests context use* or when a command clearly depends on context (e.g., 'show project architecture'). 
 It checks for a .no-context flag — if present, context operations are disabled.
 Returns true if context use is permitted; false otherwise.
 The LLM must never perform implicit context checks or loading — only when directed by the user.")



(gptel-make-tool
 :function #'ai/--init-context
 :name "init_context"
 :category "context"
 :confirm t
 :args '((:name "path" :type string :optional t
          :description "Directory path where .context folder should be created. Defaults to current working directory."))
 :description
 "Initialize a .context directory system for a project. Run this *only* when:
  - The user explicitly asks to enable or set up project context.
  - context_allowed returned false and the user confirms setup.
 This creates .context/, initializes git if needed, and generates 00_index.org.
 It must not auto-run during normal interaction.
 This command sets up the agentic memory layer for project documentation.")




(gptel-make-tool
 :function #'ai/--list-context
 :name "list_context_files"
 :category "context"
 :description "List all context files in the current project's .context directory. ALWAYS use this tool at the beginning of a conversation to discover what context is available. This returns the full paths of all readable regular files in the .context directory tree. Use this to understand what project documentation exists before attempting to read specific files. The returned list helps you determine what information you can access about the project, its architecture, conventions, and requirements. Call this tool whenever you need to know what context files exist or when a user asks about available project documentation.")


(gptel-make-tool
 :function #'ai/--read-context
 :name "read_context"
 :category "context"
 :args '((:name "filename" :type string
          :description "Relative path of the context file to read, as returned by list_context_files. Include the full path relative to .context directory (e.g., '00_index.org' or 'architecture/01_system-design.org')."))
 :description "Read the contents of a specific context file from the project's .context directory. ALWAYS use this tool to load project context at the start of conversations or when you need specific project information. Context files contain critical information about project architecture, coding standards, requirements, design decisions, and other persistent knowledge. Read relevant context files to understand the project before making suggestions or writing code. Use list_context_files first to discover available files, then read the ones relevant to the current task. This ensures your responses align with project conventions and requirements.")


(gptel-make-tool
 :function #'ai/--create-context
 :name "create_context_file"
 :category "context"
 :args '((:name "slug" :type string
          :description "Short descriptive identifier (e.g., 'architecture', 'api-design').")
         (:name "content" :type string
          :description "Org-mode content.")
         (:name "priority" :type integer
          :description "Priority (0-99). 0-9 foundational, 10-49 functional, 50-99 supplementary."))
 :description "Create a new context file in .context directory.")


(gptel-make-tool
 :function #'ai/--write-context
 :name "update_context_file"
 :category "context"
 :args '((:name "filename" :type string
          :description "Context filename")
         (:name "content" :type string
          :description "New content"))
 :description "Update context file")

;; Web operations

(gptel-make-tool
 :function #'ai/--brave-search
 :name "brave_search"
 :category "web"
 :confirm t
 :args '((:name "query" :type string
          :description "Search query"))
 :description "Search web using Brave API")


(gptel-make-tool
 :function #'ai/--http-get
 :name "http_get"
 :category "web"
 :confirm t
 :args '((:name "url" :type string
          :description "URL to fetch"))
 :description "Make HTTP GET request")


(gptel-make-tool
 :function #'ai/--http-post
 :name "http_post"
 :category "web"
 :confirm t
 :args '((:name "url" :type string
          :description "URL to post to")
         (:name "data" :type string
          :description "Data to send"))
 :description "Make HTTP POST request")

;; System operations

(gptel-make-tool
 :function #'ai/--shell-command
 :name "shell_command"
 :category "system"
 :confirm t
 :args '((:name "command" :type string
          :description "Shell command"))
 :description "Execute shell command")


(gptel-make-tool
 :function #'ai/--system-info
 :name "system_info"
 :category "system"
 :description "Get system information")


(gptel-make-tool
 :function #'ai/--project-root
 :name "project_root"
 :category "project"
 :description "Find project root directory")


(gptel-make-tool
 :function #'ai/--git-status
 :name "git_status"
 :category "project"
 :description "Get git repository status")


(gptel-make-tool
 :function #'ai/--recent-files
 :name "recent_files"
 :category "project"
 :description "Get recently opened files")


(gptel-make-tool
 :function #'ai/--open-buffers
 :name "open_buffers"
 :category "project"
 :description "Get info about open buffers")

;;; Initialize


(defvar ai/agent-system-prompt
  "### Role

You are an **agentic Emacs assistant** running inside Emacs via gptel with explicit tools.
You plan, call tools, and edit files/buffers/context; the human reviews and runs things.

---

### Core Behavior

- Think in a loop: **understand → plan → act (tool calls) → verify → summarize**.
- Be terse and mechanical; default to ≤4 lines of natural language.
- Prefer concrete outputs (code, patches, org snippets, command lines) over explanation.
- Never invent files, paths, functions, or context; verify via tools first.

---

### Tool Use (High Level)

You have tools for:
- **Emacs introspection** (functions, variables, features, libraries).
- **Filesystem** (read/write files, list dirs, apply line edits, search/replace, patches).
- **Buffers/UI** (read/append buffers, create org buffers, show stats).
- **Context** (project memory in .context/), **web**, and **system/project** info.

Behavior:
- Choose the **minimal** tool set for the current request.
- Chain tools only when necessary; avoid noisy exploratory calls.
- Prefer specialized helpers (e.g., apply_line_changes, search_replace, read_file_numbered)
  over raw shell commands.

---

### Context / Memory (.context/)

Project memory is stored under a .context directory.
Only touch it when:
- The user explicitly asks for context, architecture, conventions, or \"remember/save/store\".
- Or the task clearly depends on project-wide knowledge (e.g., \"follow existing architecture\").

Flow:
1. Check `context_allowed()` before any context usage.
2. Discover with `list_context_files()`.
3. Load only the files you need via `read_context(filename)`.
4. Create/update with `create_context_file` / `update_context_file` **only when instructed**.

When the user says \"remember/save/store this\":
- Append an entry to a suitable context file (e.g. an *active state* or *notes* file).
- Include timestamp, short summary, and any relevant paths.
- Then report which file you updated.

Do **not** silently preload or update context.

---

### Files & Patches

Treat the filesystem as the main world state.

Rules:
- Before editing: inspect with `read_file`, `read_file_numbered`, or `get_file_lines`.
- For structural edits: use `apply_line_changes` or `search_replace` instead of rewriting whole files.
- For complex diffs: generate and (optionally) apply unified patches with the patch helpers.
- Backups: rely on provided helpers (e.g., backup-creating tools) when available.

After any mutating operation, always:
1. Assume the change may be wrong until verified.
2. Verify with `read_file` or `git_status` when useful.
3. Report what changed plus file paths.

---

### Filepath Echoing

Whenever you **create, overwrite, patch, or update** files, include a block like:

<filepaths>
[[file:/absolute/path/to/file1][file1]]
[[file:/absolute/path/to/dir/file2][file2]]
</filepaths>

- Use **absolute paths** whenever you can infer them.
- For read-only or pure Q&A responses, you may omit `<filepaths>`.

---

### Interaction Style

- No preambles, no sign-offs; answer directly.
- If the user wants detail, give it, but still keep it structured and focused.
- If something is ambiguous, state your assumption in one short line before acting.
- If you genuinely do not know or cannot safely infer, say so plainly.

---

### Planning Template (Implicit, Not Printed Every Time)

1. Parse the request and classify it: {analysis | refactor | create file | debug | architecture | context}.
2. Decide if tools are needed:
   - If yes: pick the smallest set of tools to inspect state first, then mutate.
   - If no: answer directly with code/text.
3. After tool calls, verify effects when possible.
4. Return:
   - short summary of what you did or propose,
   - any code/diff/org,
   - optional `<filepaths>` block if files were touched.

Operate like a careful, deterministic Emacs operator, not a generic chatbot."
  "System prompt used by the ai/agent preset.")

(defvar ai/agent-tools
  '("elisp_eval"
    "symbol_exists"
    "function_source"
    "variable_source"
    "function_documentation"
    "variable_documentation"
    "variable_value"
    "function_completions"
    "variable_completions"
    "command_completions"
    "features"
    "load_paths"
    "library_source"
    "feature_available"
    "manual_names"
    "manual_nodes"
    "manual_node_contents"
    "symbol_manual_section"
    "file_exists"
    "read_file"
    "write_file"
    "write_org"
    "file_backup"
    "list_files"
    "create_file"
    "make_directory"
    "current_directory"
    "read_buffer"
    "append_to_buffer"
    "list_buffers"
    "create_org_buffer"
    "current_buffer"
    "cursor_position"
    "buffer_stats"
    "context_allowed"
    "init_context"
    "list_context_files"
    "read_context"
    "create_context_file"
    "update_context_file"
    "brave_search"
    "http_get"
    "http_post"
    "shell_command"
    "system_info"
    "project_root"
    "git_status"
    "recent_files"
    "open_buffers"
    "apply_line_changes"
    "search_replace"
    "get_file_lines"
    "diff_changes"
    "apply_patch"
    "apply_diff_patch"
    "preview_changes"
    "read_file_numbered"
    "diff_files"
    "git_diff")
  "Tool names used by the ai/agent preset.")

(gptel-make-preset 'agent
  :description "Full Emacs/filesystem/context/web/system agent with all tools enabled."
  :backend (ai/llm-openrouter-backend :stream t :name "OpenRouter")
  :model (ai/llm-resolve-model)
  :system ai/agent-system-prompt
  :stream t
  :tools ai/agent-tools
  :temperature 0.7
  :max-tokens nil
  :use-context 'system
  :track-media nil
  :include-reasoning t)


(provide 'ai-agent)
;;; ai-agent-tools.el ends here
