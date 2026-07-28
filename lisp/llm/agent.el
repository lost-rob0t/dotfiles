;;; agent.el --- gptel project-agent tools and presets -*- lexical-binding: t; -*-

(require 'ai-agent-core)

(defun ai/agent--patch-paths (patch)
  "Return every non-null path referenced by unified PATCH headers."
  (with-temp-buffer
    (insert patch)
    (goto-char (point-min))
    (let (paths)
      (while (re-search-forward
              "^\\(?:---\\|+++\\) \\(?:[ab]/\\)?\\([^\t\n]+\\)" nil t)
        (let ((path (match-string 1)))
          (unless (string= path "/dev/null")
            (push path paths))))
      (delete-dups (nreverse paths)))))

(defun ai/agent--run-process (program arguments &optional directory timeout)
  "Run PROGRAM with ARGUMENTS in DIRECTORY with TIMEOUT seconds."
  (let* ((buffer (generate-new-buffer " *ai-agent-process*"))
         (default-directory (or directory default-directory))
         (deadline (+ (float-time) (or timeout ai/agent-command-timeout)))
         (process (make-process :name "ai-agent-process"
                                :buffer buffer
                                :command (cons program arguments)
                                :connection-type 'pipe
                                :noquery t))
         timed-out)
    (unwind-protect
        (progn
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output process 0.1))
          (when (process-live-p process)
            (setq timed-out t)
            (delete-process process))
          (while (accept-process-output process 0.05))
          (with-current-buffer buffer
            (list :exit-code (unless timed-out (process-exit-status process))
                  :timed-out timed-out
                  :output (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer buffer))))

(defun ai/agent--register (name function category description &optional args confirm aliases)
  "Register a gptel tool and optional ALIASES."
  (dolist (tool-name (cons name aliases))
    (when (fboundp 'gptel-get-tool)
      (ignore-errors (setf (gptel-get-tool tool-name) nil)))
    (apply #'gptel-make-tool
           (append (list :name tool-name
                         :function function
                         :category category
                         :description description)
                   (when args (list :args args))
                   (when confirm (list :confirm t))))
    (cl-pushnew tool-name ai/agent-tools :test #'equal)))

(setq ai/agent-tools nil)

(ai/agent--register
 "Read" #'ai/agent-read-file "filesystem"
 "Read a text file by line range. Paths are project-relative by default."
 '((:name "path" :type string :description "Project-relative file path")
   (:name "offset" :type integer :optional t :description "First line, one-based")
   (:name "limit" :type integer :optional t :description "Maximum lines to return"))
 nil '("read_file" "read_file_numbered"))

(ai/agent--register
 "Glob" #'ai/agent-glob "filesystem"
 "Find project files by glob pattern."
 '((:name "pattern" :type string :description "Glob such as **/*.el")
   (:name "path" :type string :optional t :description "Directory to search")
   (:name "max_results" :type integer :optional t :description "Result cap")))

(ai/agent--register
 "Grep" #'ai/agent-grep "filesystem"
 "Search file contents recursively with ripgrep when available."
 '((:name "pattern" :type string :description "Regex to search")
   (:name "path" :type string :optional t :description "Directory to search")
   (:name "glob" :type string :optional t :description "Optional file glob")
   (:name "case_sensitive" :type boolean :optional t :description "Case-sensitive search")
   (:name "max_results" :type integer :optional t :description "Result cap")))

(ai/agent--register
 "LS" #'ai/agent-list-directory "filesystem"
 "List a project directory as a bounded tree."
 '((:name "path" :type string :optional t :description "Directory path")
   (:name "depth" :type integer :optional t :description "Maximum recursion depth"))
 nil '("list_files"))

(ai/agent--register
 "FileStat" #'ai/agent-file-stat "filesystem"
 "Return type, size, modification time, and modes for a path."
 '((:name "path" :type string :description "Project-relative path"))
 nil nil)

(ai/agent--register
 "Edit" #'ai/agent-edit-file "filesystem"
 "Replace exact text in a file atomically and return a unified diff. Fails on ambiguous matches."
 '((:name "path" :type string :description "File path")
   (:name "old_text" :type string :description "Exact text to replace, including context")
   (:name "new_text" :type string :description "Replacement text")
   (:name "replace_all" :type boolean :optional t :description "Replace every exact match")
   (:name "preview" :type boolean :optional t :description "Return diff without writing"))
 t '("search_replace"))

(ai/agent--register
 "MultiEdit" #'ai/agent-multi-edit "filesystem"
 "Apply multiple exact edits to one file as one validated atomic transaction."
 '((:name "path" :type string :description "File path")
   (:name "edits" :type array
          :items (:type object
                  :properties (:old_text (:type string :description "Exact text to replace")
                               :new_text (:type string :description "Replacement text")
                               :replace_all (:type boolean :description "Replace all exact matches"))
                  :required ["old_text" "new_text"]
                  :additionalProperties :json-false)
          :description "Ordered exact-match edits")
   (:name "preview" :type boolean :optional t :description "Return diff without writing"))
 t nil)

(ai/agent--register
 "Write" #'ai/agent-write-file "filesystem"
 "Create or explicitly overwrite a complete file atomically. Prefer Edit for existing files."
 '((:name "path" :type string :description "File path")
   (:name "content" :type string :description "Complete file content")
   (:name "overwrite" :type boolean :optional t :description "Permit replacement of existing file"))
 t '("write_file" "write_org"))

(ai/agent--register
 "ApplyPatch" #'ai/agent-apply-patch "filesystem"
 "Validate and apply a standard unified Git patch. Supports multi-file patches inside the project."
 '((:name "patch" :type string :description "Unified patch text")
   (:name "check" :type boolean :optional t :description "Check without changing files")
   (:name "reverse" :type boolean :optional t :description "Apply in reverse"))
 t '("apply_patch" "apply_diff_patch"))

(ai/agent--register
 "Mkdir" #'ai/agent-make-directory "filesystem"
 "Create a directory and missing parents."
 '((:name "path" :type string :description "Directory path")) t nil)

(ai/agent--register
 "Move" #'ai/agent-move-path "filesystem"
 "Move or rename a path inside the project."
 '((:name "source" :type string :description "Source path")
   (:name "destination" :type string :description "Destination path")
   (:name "overwrite" :type boolean :optional t :description "Permit destination replacement")) t)

(ai/agent--register
 "Delete" #'ai/agent-delete-path "filesystem"
 "Delete a file or, with recursive=true, a directory."
 '((:name "path" :type string :description "Path to delete")
   (:name "recursive" :type boolean :optional t :description "Allow recursive directory deletion")) t)

(ai/agent--register
 "Bash" #'ai/agent-bash "system"
 "Run a shell command in a project directory with a timeout and structured output."
 '((:name "command" :type string :description "Shell command")
   (:name "timeout" :type integer :optional t :description "Timeout in seconds")
   (:name "working_directory" :type string :optional t :description "Project-relative directory"))
 t '("shell_command"))

(ai/agent--register "GitStatus" #'ai/agent-git-status "git"
                    "Return porcelain-v2 Git status." nil nil '("git_status"))
(ai/agent--register
 "GitDiff" #'ai/agent-git-diff "git"
 "Return a bounded Git diff."
 '((:name "cached" :type boolean :optional t :description "Show staged changes")
   (:name "ref" :type string :optional t :description "Commit or branch to compare")
   (:name "path" :type string :optional t :description "Restrict to a path"))
 nil nil)
(ai/agent--register
 "DiffFiles" #'ai/agent-diff-files "filesystem"
 "Show a unified diff between two files."
 '((:name "path_a" :type string :description "First path")
   (:name "path_b" :type string :description "Second path"))
 nil '("diff_files"))

(ai/agent--register
 "ReadBuffer" #'ai/agent-read-buffer "emacs"
 "Read an Emacs buffer or a bounded region."
 '((:name "buffer" :type string :description "Buffer name")
   (:name "start" :type integer :optional t :description "Start position")
   (:name "end" :type integer :optional t :description "End position"))
 nil '("read_buffer"))
(ai/agent--register
 "EditBuffer" #'ai/agent-edit-buffer "emacs"
 "Apply an exact-match edit to a live buffer."
 '((:name "buffer" :type string :description "Buffer name")
   (:name "old_text" :type string :description "Exact text to replace")
   (:name "new_text" :type string :description "Replacement text")
   (:name "replace_all" :type boolean :optional t :description "Replace every match")
   (:name "preview" :type boolean :optional t :description "Validate without editing")) t)
(ai/agent--register "ListBuffers" #'ai/agent-list-buffers "emacs"
                    "List live user-visible buffers." nil nil '("list_buffers" "open_buffers"))
(ai/agent--register
 "SymbolInfo" #'ai/agent-symbol-info "emacs"
 "Inspect an Emacs symbol's type, documentation, and source file."
 '((:name "name" :type string :description "Symbol name"))
 nil '("function_source" "variable_source" "function_documentation"
       "variable_documentation" "symbol_exists"))
(ai/agent--register
 "EvalElisp" #'ai/agent-eval-elisp "emacs"
 "Evaluate Emacs Lisp. Use only when a dedicated tool cannot perform the operation."
 '((:name "expression" :type string :description "Elisp expression"))
 t '("elisp_eval"))

(ai/agent--register
 "WebFetch" #'ai/agent-web-fetch "web"
 "Fetch an HTTP(S) URL without shell interpolation."
 '((:name "url" :type string :description "HTTP or HTTPS URL")
   (:name "method" :type string :optional t :enum ["GET" "POST" "PUT" "PATCH" "DELETE"]
          :description "HTTP method")
   (:name "headers" :type array :optional t
          :items (:type object
                  :properties (:name (:type string :description "Header name")
                               :value (:type string :description "Header value"))
                  :required ["name" "value"]
                  :additionalProperties :json-false)
          :description "HTTP headers")
   (:name "body" :type string :optional t :description "Request body"))
 t nil)

(ai/agent--register "ListContext" #'ai/agent-list-context "context"
                    "List files below the project's .context directory." nil nil '("list_context_files"))
(ai/agent--register
 "ReadContext" #'ai/agent-read-context "context"
 "Read one project context file."
 '((:name "path" :type string :description "Path relative to .context"))
 nil '("read_context"))
(ai/agent--register
 "WriteContext" #'ai/agent-write-context "context"
 "Create or explicitly overwrite one project context file."
 '((:name "path" :type string :description "Path relative to .context")
   (:name "content" :type string :description "Complete content")
   (:name "overwrite" :type boolean :optional t :description "Permit replacement"))
 t nil)

(setq ai/agent-tools
      '("Read" "Glob" "Grep" "LS" "FileStat"
        "Edit" "MultiEdit" "Write" "ApplyPatch"
        "Mkdir" "Move" "Delete" "Bash"
        "GitStatus" "GitDiff" "DiffFiles"
        "ReadBuffer" "EditBuffer" "ListBuffers"
        "SymbolInfo" "EvalElisp" "WebFetch"
        "ListContext" "ReadContext" "WriteContext"))

(defvar ai/agent-system-prompt
  "You are a coding agent operating inside Emacs through gptel.

Operate in a tight inspect -> edit -> verify loop.

Filesystem rules:
- Treat the project root as the workspace boundary.
- Use Glob and Grep to discover, Read to inspect, Edit for one exact change, and MultiEdit for a coherent set of exact changes.
- Prefer exact-match edits over line-number edits or whole-file rewrites.
- Use Write only for new files or deliberate complete replacements.
- Use ApplyPatch for genuine unified patches; check it first when the patch is large or generated externally.
- After mutations, verify with Read, GitDiff, GitStatus, or the relevant test command.
- Never claim a change succeeded unless a tool result confirms it.

Execution rules:
- Use Bash for builds, tests, formatters, and repository commands only when a dedicated tool is insufficient.
- Do not run destructive commands or external writes unless the user's request clearly authorizes them.
- Keep tool output bounded and avoid dumping entire large files when a focused range or search is enough.
- Report changed paths, validation performed, and unresolved failures.

Be direct. Do the work instead of narrating hypothetical steps."
  "System prompt for the project agent preset.")

(gptel-make-preset 'agent
  :description "GLM-5.2 project agent with Claude Code-style tools."
  :backend (ai/llm-backend 'zai)
  :model 'glm-5.2
  :system ai/agent-system-prompt
  :tools ai/agent-tools
  :stream t
  :temperature nil
  :use-context 'system
  :track-media t
  :include-reasoning t)

(gptel-make-preset 'agent-gpt-5.6-sol
  :parents '(agent)
  :description "GPT-5.6 Sol project agent with the same tool suite."
  :backend (ai/llm-backend 'openai)
  :model 'gpt-5.6-sol)

(define-minor-mode ai/agent-context-mode
  "Add common project instruction files to `gptel-context'."
  :lighter " AgentCtx"
  (if ai/agent-context-mode
      (let* ((root (ai/agent--project-root))
             (files (seq-filter #'file-exists-p
                                (mapcar (lambda (path) (expand-file-name path root))
                                        ai/agent-context-files))))
        (setq-local gptel-context
                    (delete-dups (append files (copy-sequence gptel-context))))
        (message "Agent context: added %d project instruction file(s)" (length files)))
    (kill-local-variable 'gptel-context)
    (message "Agent context disabled")))

(provide 'ai-agent)
;;; agent.el ends here
