;;; ai-agent-core.el --- Canonical loader for the project-agent core -*- lexical-binding: t; -*-

(unless (featurep 'ai-agent-core)
  (load (expand-file-name
         "agent-core.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil
        'nomessage))

(defcustom ai/agent-allowed-roots '("~")
  "Additional roots accessible to filesystem tools.

The current project root is always allowed.  Entries are expanded with
`expand-file-name' and checked through `file-truename', so symlinks cannot
escape an allowed root."
  :type '(repeat directory)
  :group 'ai/agent)

(defun ai/agent--object (&rest pairs)
  "Return a JSON-compatible alist from alternating keys and values in PAIRS."
  (let (result)
    (while pairs
      (let ((key (pop pairs))
            (value (pop pairs)))
        (push (cons (if (symbolp key) key (intern key)) value) result)))
    (nreverse result)))

(defun ai/agent--path-allowed-p (path project-root)
  "Return non-nil when PATH is inside PROJECT-ROOT or an allowed root."
  (or (ai/agent--inside-p path project-root)
      (cl-some (lambda (root)
                 (ai/agent--inside-p path (expand-file-name root)))
               ai/agent-allowed-roots)))

(defun ai/agent--resolve-path (path &optional allow-outside)
  "Resolve PATH and enforce the configured filesystem roots."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "Path must be a non-empty string"))
  (let* ((root (ai/agent--project-root))
         (expanded (expand-file-name path root)))
    (when (and ai/agent-restrict-to-project
               (not allow-outside)
               (not (ai/agent--path-allowed-p expanded root)))
      (error "Path escapes allowed roots: %s" path))
    expanded))

(defun ai/agent--relative-path (path)
  "Return a project-relative path or an abbreviated absolute path."
  (let ((root (ai/agent--project-root)))
    (if (ai/agent--inside-p path root)
        (file-relative-name path root)
      (abbreviate-file-name (expand-file-name path)))))

(provide 'ai-agent-core)
;;; ai-agent-core.el ends here
