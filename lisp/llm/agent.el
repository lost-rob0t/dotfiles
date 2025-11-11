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
  (let* ((api-key (getenv "BRAVE_API_KEY"))
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
 :description
 "List all .org context files in the project's .context directory.
 Returns absolute or project-relative paths for reference.")

(gptel-make-tool
 :function #'ai/--read-context
 :name "read_context"
 :category "context"
 :args '((:name "filename" :type string
          :description "Path relative to .context/ (e.g., '00_index.org', 'architecture/01_system-design.org')."))
 :description
 "Read a specific context file when the user or workflow explicitly requests project context loading.
 Context files are authoritative knowledge documents containing architecture, coding conventions, and specifications.
 The LLM must never automatically read or pre-load these files — it must do when the current task directly requires project understanding (e.g., 'follow architecture guidelines from context').")


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
 :args '((:name "project_name" :type string
          :description "Name of the project or repository root (e.g., 'hackmode-expert', 'temple').")
         (:name "project_path" :type string
          :description "Full path to the project directory (e.g., '~/Documents/Projects/hackmode-expert'). Include tilde if home-relative.")
         (:name "slug" :type string
          :description "Short descriptive identifier for the context file (e.g., 'architecture', 'api-design'). Filename generated as 'NN_slug.org'.")
         (:name "content" :type string
          :description "Comprehensive Org-mode formatted content. Must include a project tree with full paths and brief 1–2 sentence descriptions for each file.")
         (:name "priority" :type integer
          :description "Priority number (0–99). Lower numbers (0–9) are foundational; 10–49 are functional; 50–99 are supplementary."))
 :description
 "Create or update a project context file in .context/ only when the user explicitly requests it.
 The LLM must NOT automatically generate or update context files unless prompted by the user.

 Context files serve as long-term project memory and must follow a strict structure:

 1. Include #+TITLE and #+DATE headers.
 2. Start with a top-level heading naming the project and its full path.
 3. Provide an Org-mode tree of directories and files, each with:
    - a 1–2 sentence purpose summary
    - a full or project-relative file path
    - nested sections for submodules if applicable.

 Example format:

 * Project: Hackmode Expert
   Path: ~/Documents/Projects/hackmode-expert

 ** modules/
  - modules/targets.prolog :: Handles classification and storage of program targets (domain, URL, IP).
  - modules/operations.prolog :: Defines phases, workflows, and run sequences for tools.

 ** tools/
  - tools/ffuf.prolog :: Fuzzing tool integration and output parser.
  - tools/bbot.prolog :: JSON scanner output parser for discovery.

 The AI should treat these context files as agentic memory — persistent references describing project structure, purpose, and interrelations — but should only write or update them when explicitly instructed by the user.")


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


;; Set default preset

(provide 'ai-agent)
;;; ai-agent-tools.el ends here
