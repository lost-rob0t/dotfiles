;;; agent-core.el --- Claude Code-style gptel agent core -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'ai)

(defgroup ai/agent nil
  "Project-aware tools for gptel coding agents."
  :group 'ai/llm
  :prefix "ai/agent-")

(defcustom ai/agent-restrict-to-project t
  "When non-nil, filesystem tools reject paths outside the project root."
  :type 'boolean
  :group 'ai/agent)

(defcustom ai/agent-max-read-bytes 262144
  "Maximum number of bytes returned by a single read operation."
  :type 'integer
  :group 'ai/agent)

(defcustom ai/agent-max-output-bytes 262144
  "Maximum number of bytes returned by commands, diffs, and HTTP requests."
  :type 'integer
  :group 'ai/agent)

(defcustom ai/agent-max-search-results 200
  "Maximum number of results returned by search tools."
  :type 'integer
  :group 'ai/agent)

(defcustom ai/agent-command-timeout 120
  "Default timeout in seconds for the Bash tool."
  :type 'integer
  :group 'ai/agent)

(defcustom ai/agent-ignored-directories
  '(".git" ".direnv" ".cache" ".venv" "node_modules" "dist" "build" "target")
  "Directory names excluded from recursive fallback searches."
  :type '(repeat string)
  :group 'ai/agent)

(defcustom ai/agent-context-files '("AGENTS.md" "CLAUDE.md")
  "Project instruction files added by `ai/agent-context-mode'."
  :type '(repeat string)
  :group 'ai/agent)

(defvar ai/agent-tools nil
  "Names of tools registered for the agent preset.")

(defun ai/agent--object (&rest pairs)
  "Return an alist from alternating string keys and values in PAIRS."
  (let (result)
    (while pairs
      (push (cons (pop pairs) (pop pairs)) result))
    (nreverse result)))

(defun ai/agent--json (&rest pairs)
  "Serialize alternating string keys and values in PAIRS as JSON."
  (json-serialize (apply #'ai/agent--object pairs)
                  :null-object nil
                  :false-object :json-false))

(defun ai/agent--json-error (message &optional details)
  "Return a structured JSON error with MESSAGE and optional DETAILS."
  (ai/agent--json "ok" :json-false "error" message "details" details))

(defun ai/agent--truncate (string &optional limit)
  "Truncate STRING to LIMIT bytes and return (TEXT . TRUNCATED-P)."
  (let* ((limit (or limit ai/agent-max-output-bytes))
         (bytes (string-bytes string)))
    (if (<= bytes limit)
        (cons string nil)
      (let ((end (min (length string) limit)))
        (while (and (> end 0)
                    (> (string-bytes (substring string 0 end)) limit))
          (setq end (- end (max 1 (/ end 20)))))
        (cons (concat (substring string 0 end)
                      (format "\n... truncated at %d bytes ..." limit))
              t)))))

(defun ai/agent--arg (object key &optional default)
  "Read KEY from OBJECT, accepting plists and alists."
  (let* ((name (substring (symbol-name key) 1))
         (symbol (intern name)))
    (cond
     ((and (listp object) (keywordp (car object)))
      (if (plist-member object key) (plist-get object key) default))
     ((listp object)
      (let ((cell (or (assoc key object)
                      (assoc symbol object)
                      (assoc name object))))
        (if cell (cdr cell) default)))
     (t default))))

(defun ai/agent--project-root ()
  "Return the current project root as an absolute directory name."
  (file-name-as-directory
   (expand-file-name
    (or (when-let ((project (project-current nil)))
          (project-root project))
        (locate-dominating-file default-directory ".git")
        default-directory))))

(defun ai/agent--nearest-existing-parent (path)
  "Return the nearest existing ancestor of PATH."
  (let ((candidate (expand-file-name path)))
    (while (and candidate (not (file-exists-p candidate)))
      (let ((parent (file-name-directory (directory-file-name candidate))))
        (setq candidate
              (unless (or (null parent) (equal parent candidate)) parent))))
    candidate))

(defun ai/agent--inside-p (path root)
  "Return non-nil when PATH resolves inside ROOT."
  (let* ((root-real (file-name-as-directory (file-truename root)))
         (existing (or (ai/agent--nearest-existing-parent path) root))
         (existing-real (file-name-as-directory
                         (if (file-directory-p existing)
                             (file-truename existing)
                           (file-name-directory (file-truename existing))))))
    (string-prefix-p root-real existing-real)))

(defun ai/agent--resolve-path (path &optional allow-outside)
  "Resolve PATH relative to the project and enforce project confinement."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "Path must be a non-empty string"))
  (let* ((root (ai/agent--project-root))
         (expanded (expand-file-name path root)))
    (when (and ai/agent-restrict-to-project
               (not allow-outside)
               (not (ai/agent--inside-p expanded root)))
      (error "Path escapes project root: %s" path))
    expanded))

(defun ai/agent--relative-path (path)
  "Return PATH relative to the current project root."
  (file-relative-name path (ai/agent--project-root)))

(defun ai/agent--ensure-parent (path)
  "Create PATH's parent directory when necessary."
  (let ((parent (file-name-directory path)))
    (unless (file-directory-p parent)
      (make-directory parent t))))

(defun ai/agent--atomic-write (path content)
  "Atomically write CONTENT to PATH, preserving existing file modes."
  (ai/agent--ensure-parent path)
  (let* ((directory (file-name-directory path))
         (temporary (make-temp-file (expand-file-name ".ai-agent-" directory)))
         (modes (and (file-exists-p path) (file-modes path))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (insert content))
          (when modes (set-file-modes temporary modes))
          (rename-file temporary path t))
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun ai/agent--read-string (path)
  "Read PATH as text without properties."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ai/agent--count-occurrences (needle haystack)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (when (string-empty-p needle)
    (error "old_text must not be empty"))
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun ai/agent--replace-exact (content old-text new-text replace-all)
  "Replace OLD-TEXT with NEW-TEXT in CONTENT.
Require exactly one match unless REPLACE-ALL is non-nil."
  (let ((count (ai/agent--count-occurrences old-text content)))
    (cond
     ((zerop count)
      (error "old_text was not found"))
     ((and (> count 1) (not replace-all))
      (error "old_text matched %d times; provide more context or set replace_all" count))
     (replace-all
      (cons (replace-regexp-in-string (regexp-quote old-text) new-text content t t)
            count))
     (t
      (let ((position (string-match (regexp-quote old-text) content)))
        (cons (concat (substring content 0 position)
                      new-text
                      (substring content (+ position (length old-text))))
              1))))))

(defun ai/agent--diff-strings (path old-content new-content)
  "Return a unified diff for PATH between OLD-CONTENT and NEW-CONTENT."
  (let ((old-file (make-temp-file "ai-agent-old-"))
        (new-file (make-temp-file "ai-agent-new-")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old-content))
          (with-temp-file new-file (insert new-content))
          (with-temp-buffer
            (let ((status (call-process "diff" nil t nil "-u"
                                        "--label" (concat "a/" (ai/agent--relative-path path))
                                        "--label" (concat "b/" (ai/agent--relative-path path))
                                        old-file new-file)))
              (unless (memq status '(0 1))
                (error "diff failed: %s" (string-trim (buffer-string))))
              (buffer-string))))
      (delete-file old-file)
      (delete-file new-file))))

(defun ai/agent-read-file (path &optional offset limit)
  "Read PATH starting at one-based line OFFSET for LIMIT lines."
  (let* ((file (ai/agent--resolve-path path))
         (offset (max 1 (or offset 1)))
         (limit (max 1 (or limit 300))))
    (unless (file-regular-p file)
      (error "Not a regular file: %s" path))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((total-lines (line-number-at-pos (point-max)))
            start end)
        (goto-char (point-min))
        (forward-line (1- offset))
        (setq start (point))
        (forward-line limit)
        (setq end (min (point) (point-max)))
        (let* ((raw (buffer-substring-no-properties start end))
               (truncated (ai/agent--truncate raw ai/agent-max-read-bytes)))
          (ai/agent--json
           "ok" t
           "path" (ai/agent--relative-path file)
           "start_line" offset
           "end_line" (min total-lines (+ offset limit -1))
           "total_lines" total-lines
           "truncated" (if (cdr truncated) t :json-false)
           "content" (car truncated)))))))

(defun ai/agent-file-stat (path)
  "Return metadata for PATH."
  (let* ((file (ai/agent--resolve-path path))
         (attributes (file-attributes file 'string)))
    (unless attributes (error "Path does not exist: %s" path))
    (ai/agent--json
     "ok" t
     "path" (ai/agent--relative-path file)
     "type" (cond ((file-directory-p file) "directory")
                  ((file-symlink-p file) "symlink")
                  (t "file"))
     "size" (file-attribute-size attributes)
     "modified" (format-time-string "%FT%T%z" (file-attribute-modification-time attributes))
     "modes" (file-attribute-modes attributes))))

(defun ai/agent-list-directory (&optional path depth)
  "List PATH recursively to DEPTH levels."
  (let* ((directory (ai/agent--resolve-path (or path ".")))
         (depth (max 1 (min 8 (or depth 2))))
         (root directory)
         (results nil))
    (unless (file-directory-p directory)
      (error "Not a directory: %s" path))
    (cl-labels ((walk (dir level)
                  (when (<= level depth)
                    (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp t))
                      (unless (member (file-name-nondirectory entry) ai/agent-ignored-directories)
                        (push (ai/agent--object
                               "path" (file-relative-name entry root)
                               "type" (if (file-directory-p entry) "directory" "file"))
                              results)
                        (when (file-directory-p entry)
                          (walk entry (1+ level))))))))
      (walk directory 1))
    (ai/agent--json "ok" t "root" (ai/agent--relative-path root)
                    "entries" (vconcat (nreverse results)))))

(defun ai/agent-glob (pattern &optional path max-results)
  "Find files below PATH whose relative names match glob PATTERN."
  (let* ((directory (ai/agent--resolve-path (or path ".")))
         (regexp (wildcard-to-regexp pattern))
         (max-results (or max-results ai/agent-max-search-results))
         (results nil))
    (unless (file-directory-p directory)
      (error "Not a directory: %s" path))
    (catch 'done
      (dolist (file (directory-files-recursively
                     directory "." nil
                     (lambda (candidate)
                       (not (member (file-name-nondirectory candidate)
                                    ai/agent-ignored-directories)))))
        (let ((relative (file-relative-name file directory)))
          (when (and (file-regular-p file) (string-match-p regexp relative))
            (push (ai/agent--relative-path file) results)
            (when (>= (length results) max-results)
              (throw 'done nil))))))
    (ai/agent--json "ok" t "pattern" pattern
                    "matches" (vconcat (nreverse results))
                    "limited" (if (>= (length results) max-results) t :json-false))))

(defun ai/agent--grep-with-rg (pattern directory glob case-sensitive max-results)
  "Search with ripgrep and return structured match objects."
  (let ((arguments (append '("--line-number" "--column" "--no-heading" "--color" "never")
                           (unless case-sensitive '("--ignore-case"))
                           (when (and glob (not (string-empty-p glob)))
                             (list "--glob" glob))
                           (list "--max-count" (number-to-string max-results)
                                 "--" pattern directory)))
        results)
    (with-temp-buffer
      (let ((status (apply #'process-file "rg" nil t nil arguments)))
        (unless (memq status '(0 1))
          (error "rg failed: %s" (string-trim (buffer-string))))
        (goto-char (point-min))
        (while (and (< (length results) max-results)
                    (re-search-forward "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)$" nil t))
          (push (ai/agent--object
                 "path" (ai/agent--relative-path (match-string 1))
                 "line" (string-to-number (match-string 2))
                 "column" (string-to-number (match-string 3))
                 "text" (match-string 4))
                results))))
    (nreverse results)))

(defun ai/agent--grep-fallback (pattern directory glob case-sensitive max-results)
  "Search text files with Emacs when ripgrep is unavailable."
  (let ((case-fold-search (not case-sensitive))
        (file-regexp (and glob (wildcard-to-regexp glob)))
        results)
    (catch 'done
      (dolist (file (directory-files-recursively directory "."))
        (when (and (file-regular-p file)
                   (or (null file-regexp)
                       (string-match-p file-regexp (file-relative-name file directory))))
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents file nil 0 ai/agent-max-read-bytes)
                (goto-char (point-min))
                (while (and (< (length results) max-results)
                            (re-search-forward pattern nil t))
                  (push (ai/agent--object
                         "path" (ai/agent--relative-path file)
                         "line" (line-number-at-pos (match-beginning 0))
                         "column" (1+ (- (match-beginning 0)
                                         (line-beginning-position)))
                         "text" (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))
                        results)))
            (error nil)))
        (when (>= (length results) max-results)
          (throw 'done nil))))
    (nreverse results)))

(defun ai/agent-grep (pattern &optional path glob case-sensitive max-results)
  "Search PATTERN recursively below PATH, optionally restricted by GLOB."
  (let* ((directory (ai/agent--resolve-path (or path ".")))
         (max-results (or max-results ai/agent-max-search-results))
         (matches (if (executable-find "rg")
                      (ai/agent--grep-with-rg pattern directory glob case-sensitive max-results)
                    (ai/agent--grep-fallback pattern directory glob case-sensitive max-results))))
    (ai/agent--json "ok" t "pattern" pattern
                    "matches" (vconcat matches)
                    "limited" (if (>= (length matches) max-results) t :json-false))))

(defun ai/agent-write-file (path content &optional overwrite)
  "Write CONTENT to PATH atomically.
Refuse to replace an existing file unless OVERWRITE is non-nil."
  (let ((file (ai/agent--resolve-path path)))
    (when (and (file-exists-p file) (not overwrite))
      (error "File exists; set overwrite=true or use Edit: %s" path))
    (ai/agent--atomic-write file content)
    (ai/agent--json "ok" t "path" (ai/agent--relative-path file)
                    "bytes" (string-bytes content)
                    "operation" (if overwrite "overwritten" "created"))))

(defun ai/agent-edit-file (path old-text new-text &optional replace-all preview)
  "Replace exact OLD-TEXT with NEW-TEXT in PATH.
When PREVIEW is non-nil, return the diff without writing."
  (let* ((file (ai/agent--resolve-path path))
         (old-content (ai/agent--read-string file))
         (replacement (ai/agent--replace-exact old-content old-text new-text replace-all))
         (new-content (car replacement))
         (count (cdr replacement))
         (diff (ai/agent--diff-strings file old-content new-content))
         (truncated (ai/agent--truncate diff)))
    (unless preview (ai/agent--atomic-write file new-content))
    (ai/agent--json "ok" t "path" (ai/agent--relative-path file)
                    "replacements" count
                    "preview" (if preview t :json-false)
                    "diff_truncated" (if (cdr truncated) t :json-false)
                    "diff" (car truncated))))

(defun ai/agent-multi-edit (path edits &optional preview)
  "Apply sequential exact-match EDITS to PATH in one atomic transaction."
  (let* ((file (ai/agent--resolve-path path))
         (original (ai/agent--read-string file))
         (content original)
         (applied 0))
    (dolist (edit edits)
      (let* ((old-text (ai/agent--arg edit :old_text))
             (new-text (ai/agent--arg edit :new_text ""))
             (replace-all (ai/agent--arg edit :replace_all nil))
             (replacement (ai/agent--replace-exact content old-text new-text replace-all)))
        (setq content (car replacement)
              applied (+ applied (cdr replacement)))))
    (let* ((diff (ai/agent--diff-strings file original content))
           (truncated (ai/agent--truncate diff)))
      (unless preview (ai/agent--atomic-write file content))
      (ai/agent--json "ok" t "path" (ai/agent--relative-path file)
                      "edits" (length edits)
                      "replacements" applied
                      "preview" (if preview t :json-false)
                      "diff_truncated" (if (cdr truncated) t :json-false)
                      "diff" (car truncated)))))

(defun ai/agent--patch-paths (patch)
  "Return target paths referenced by unified PATCH."
  (with-temp-buffer
    (insert patch)
    (goto-char (point-min))
    (let (paths)
      (while (re-search-forward "^+++ \\(?:b/\\)?\\([^\t\n]+\\)" nil t)
        (let ((path (match-string 1)))
          (unless (string= path "/dev/null")
            (push path paths))))
      (delete-dups (nreverse paths)))))

(defun ai/agent--validate-patch (patch)
  "Validate that PATCH only targets project-relative paths."
  (let ((paths (ai/agent--patch-paths patch)))
    (unless paths (error "Patch contains no target paths"))
    (dolist (path paths)
      (when (or (file-name-absolute-p path)
                (member ".." (split-string path "/" t)))
        (error "Unsafe patch path: %s" path))
      (ai/agent--resolve-path path))
    paths))

(defun ai/agent-apply-patch (patch &optional check reverse)
  "Apply unified PATCH with `git apply'.
CHECK performs validation only.  REVERSE applies the patch in reverse."
  (let* ((root (ai/agent--project-root))
         (paths (ai/agent--validate-patch patch))
         (patch-file (make-temp-file "ai-agent-patch-"))
         (arguments (append '("apply" "--whitespace=nowarn")
                            (when check '("--check"))
                            (when reverse '("--reverse"))
                            (list patch-file))))
    (unwind-protect
        (progn
          (with-temp-file patch-file (insert patch))
          (with-temp-buffer
            (let ((default-directory root)
                  (status (apply #'process-file "git" nil t nil arguments)))
              (if (zerop status)
                  (ai/agent--json "ok" t
                                  "operation" (if check "checked" "applied")
                                  "paths" (vconcat paths))
                (ai/agent--json-error "Patch failed" (string-trim (buffer-string)))))))
      (delete-file patch-file))))

(defun ai/agent-make-directory (path)
  "Create PATH and missing parents."
  (let ((directory (ai/agent--resolve-path path)))
    (make-directory directory t)
    (ai/agent--json "ok" t "path" (ai/agent--relative-path directory))))

(defun ai/agent-move-path (source destination &optional overwrite)
  "Move SOURCE to DESTINATION."
  (let ((source-path (ai/agent--resolve-path source))
        (destination-path (ai/agent--resolve-path destination)))
    (unless (file-exists-p source-path)
      (error "Source does not exist: %s" source))
    (when (and (file-exists-p destination-path) (not overwrite))
      (error "Destination exists: %s" destination))
    (ai/agent--ensure-parent destination-path)
    (rename-file source-path destination-path overwrite)
    (ai/agent--json "ok" t
                    "source" (ai/agent--relative-path source-path)
                    "destination" (ai/agent--relative-path destination-path))))

(defun ai/agent-delete-path (path &optional recursive)
  "Delete PATH.  Directories require RECURSIVE."
  (let ((target (ai/agent--resolve-path path)))
    (cond
     ((file-directory-p target)
      (unless recursive (error "Directory deletion requires recursive=true"))
      (delete-directory target t))
     ((file-exists-p target) (delete-file target))
     (t (error "Path does not exist: %s" path)))
    (ai/agent--json "ok" t "deleted" (ai/agent--relative-path target))))

(defun ai/agent--run-process (program arguments &optional directory timeout)
  "Run PROGRAM with ARGUMENTS in DIRECTORY with TIMEOUT seconds."
  (let* ((buffer (generate-new-buffer " *ai-agent-process*"))
         (default-directory (or directory default-directory))
         (deadline (+ (float-time) (or timeout ai/agent-command-timeout)))
         (process (make-process :name "ai-agent-process"
                                :buffer buffer
                                :stderr buffer
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
          (with-current-buffer buffer
            (list :exit-code (unless timed-out (process-exit-status process))
                  :timed-out timed-out
                  :output (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer buffer))))

(defun ai/agent-bash (command &optional timeout working-directory)
  "Run shell COMMAND inside the project and return structured output."
  (let* ((directory (ai/agent--resolve-path (or working-directory ".")))
         (result (ai/agent--run-process shell-file-name
                                       (list shell-command-switch command)
                                       directory timeout))
         (truncated (ai/agent--truncate (plist-get result :output))))
    (ai/agent--json
     "ok" (if (and (not (plist-get result :timed-out))
                    (zerop (or (plist-get result :exit-code) 1)))
               t :json-false)
     "exit_code" (plist-get result :exit-code)
     "timed_out" (if (plist-get result :timed-out) t :json-false)
     "working_directory" (ai/agent--relative-path directory)
     "output_truncated" (if (cdr truncated) t :json-false)
     "output" (car truncated))))

(defun ai/agent-git-status ()
  "Return porcelain-v2 Git status for the current project."
  (let* ((root (ai/agent--project-root))
         (result (ai/agent--run-process "git" '("status" "--porcelain=v2" "--branch") root 30))
         (truncated (ai/agent--truncate (plist-get result :output))))
    (ai/agent--json "ok" (if (zerop (or (plist-get result :exit-code) 1)) t :json-false)
                    "root" root "output" (car truncated))))

(defun ai/agent-git-diff (&optional cached ref path)
  "Return Git diff, optionally CACHED, against REF, or restricted to PATH."
  (let* ((root (ai/agent--project-root))
         (arguments '("diff" "--no-ext-diff")))
    (when cached (setq arguments (append arguments '("--cached"))))
    (when (and ref (not (string-empty-p ref)))
      (setq arguments (append arguments (list ref))))
    (when path
      (let ((file (ai/agent--resolve-path path)))
        (setq arguments (append arguments (list "--" (file-relative-name file root))))))
    (let* ((result (ai/agent--run-process "git" arguments root 60))
           (truncated (ai/agent--truncate (plist-get result :output))))
      (ai/agent--json "ok" (if (zerop (or (plist-get result :exit-code) 1)) t :json-false)
                      "truncated" (if (cdr truncated) t :json-false)
                      "diff" (car truncated)))))

(defun ai/agent-diff-files (path-a path-b)
  "Return a unified diff between PATH-A and PATH-B."
  (let* ((a (ai/agent--resolve-path path-a))
         (b (ai/agent--resolve-path path-b))
         (result (ai/agent--run-process "diff" (list "-u" a b) (ai/agent--project-root) 30))
         (status (plist-get result :exit-code))
         (truncated (ai/agent--truncate (plist-get result :output))))
    (unless (memq status '(0 1))
      (error "diff failed: %s" (plist-get result :output)))
    (ai/agent--json "ok" t "different" (if (= status 1) t :json-false)
                    "truncated" (if (cdr truncated) t :json-false)
                    "diff" (car truncated))))

(defun ai/agent-read-buffer (buffer &optional start end)
  "Read BUFFER between START and END positions."
  (unless-let ((target (get-buffer buffer)))
    (error "Buffer is not live: %s" buffer))
  (with-current-buffer target
    (let* ((start (max (point-min) (or start (point-min))))
           (end (min (point-max) (or end (point-max))))
           (truncated (ai/agent--truncate
                       (buffer-substring-no-properties start end)
                       ai/agent-max-read-bytes)))
      (ai/agent--json "ok" t "buffer" buffer "start" start "end" end
                      "truncated" (if (cdr truncated) t :json-false)
                      "content" (car truncated)))))

(defun ai/agent-edit-buffer (buffer old-text new-text &optional replace-all preview)
  "Apply an exact-match edit to BUFFER."
  (unless-let ((target (get-buffer buffer)))
    (error "Buffer is not live: %s" buffer))
  (with-current-buffer target
    (let* ((original (buffer-substring-no-properties (point-min) (point-max)))
           (replacement (ai/agent--replace-exact original old-text new-text replace-all))
           (new-content (car replacement)))
      (unless preview
        (atomic-change-group
          (erase-buffer)
          (insert new-content)))
      (ai/agent--json "ok" t "buffer" buffer
                      "replacements" (cdr replacement)
                      "preview" (if preview t :json-false)))))

(defun ai/agent-list-buffers ()
  "Return useful metadata for live buffers."
  (let (results)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (unless (string-prefix-p " " (buffer-name))
          (push (ai/agent--object
                 "name" (buffer-name)
                 "file" (or buffer-file-name nil)
                 "mode" (symbol-name major-mode)
                 "modified" (if (buffer-modified-p) t :json-false))
                results))))
    (ai/agent--json "ok" t "buffers" (vconcat (nreverse results)))))

(defun ai/agent-symbol-info (name)
  "Return documentation and source location for Emacs symbol NAME."
  (let ((symbol (intern-soft name)))
    (unless symbol (error "Unknown symbol: %s" name))
    (let* ((function-p (fboundp symbol))
           (variable-p (boundp symbol))
           (documentation
            (cond (function-p (ignore-errors (documentation symbol t)))
                  (variable-p (ignore-errors (documentation-property symbol 'variable-documentation t)))))
           (source (or (symbol-file symbol 'defun) (symbol-file symbol 'defvar))))
      (ai/agent--json "ok" t "symbol" name
                      "function" (if function-p t :json-false)
                      "variable" (if variable-p t :json-false)
                      "source" source
                      "documentation" documentation))))

(defun ai/agent-eval-elisp (expression)
  "Evaluate Elisp EXPRESSION and return the printed value."
  (let ((value (eval (read expression) lexical-binding)))
    (ai/agent--json "ok" t "value" (prin1-to-string value))))

(defun ai/agent-web-fetch (url &optional method headers body)
  "Fetch URL with METHOD, HEADERS, and BODY using Emacs URL."
  (unless (string-match-p "\\`https?://" url)
    (error "Only HTTP and HTTPS URLs are allowed"))
  (let* ((url-request-method (upcase (or method "GET")))
         (url-request-extra-headers
          (mapcar (lambda (header)
                    (cons (or (ai/agent--arg header :name)
                              (ai/agent--arg header :key))
                          (ai/agent--arg header :value)))
                  headers))
         (url-request-data body)
         (buffer (url-retrieve-synchronously url t t 30)))
    (unless buffer (error "Request failed: %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((status (or (bound-and-true-p url-http-response-status) 0)))
            (re-search-forward "\r?\n\r?\n" nil 'move)
            (let ((truncated (ai/agent--truncate
                              (buffer-substring-no-properties (point) (point-max)))))
              (ai/agent--json "ok" (if (and (>= status 200) (< status 400)) t :json-false)
                              "status" status
                              "url" url
                              "truncated" (if (cdr truncated) t :json-false)
                              "body" (car truncated)))))
      (kill-buffer buffer))))

(defun ai/agent--context-root ()
  "Return the project .context directory."
  (expand-file-name ".context" (ai/agent--project-root)))

(defun ai/agent-list-context ()
  "List readable project context files."
  (let ((root (ai/agent--context-root)))
    (if (file-directory-p root)
        (ai/agent--json "ok" t "files"
                        (vconcat (mapcar (lambda (file) (file-relative-name file root))
                                        (directory-files-recursively root "."))))
      (ai/agent--json "ok" t "files" []))))

(defun ai/agent-read-context (path)
  "Read PATH below the project .context directory."
  (let* ((root (ai/agent--context-root))
         (file (expand-file-name path root)))
    (unless (ai/agent--inside-p file root)
      (error "Context path escapes .context: %s" path))
    (ai/agent-read-file (file-relative-name file (ai/agent--project-root)))))

(defun ai/agent-write-context (path content &optional overwrite)
  "Write CONTENT to PATH below the project .context directory."
  (let* ((root (ai/agent--context-root))
         (file (expand-file-name path root)))
    (unless (ai/agent--inside-p file root)
      (error "Context path escapes .context: %s" path))
    (ai/agent-write-file (file-relative-name file (ai/agent--project-root)) content overwrite)))

(provide 'ai-agent-core)
;;; agent-core.el ends here
