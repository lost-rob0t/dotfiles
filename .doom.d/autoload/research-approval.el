;;; research-approval.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defconst nsa/research-approval--path-regexp
  "\\`research/[^/]+\\.org\\'"
  "Repository-relative paths accepted by the approval command.")

(defconst nsa/research-approval--fields
  '("approval_schema" "approval_state" "approval_actor"
    "approval_evidence" "approval_base_commit" "approval_base_blob"
    "approval_decided_at")
  "Approval fields in their required order.")

(defconst nsa/research-approval--schema
  "prolog-rlm.research-approval.v1"
  "Canonical research approval schema identifier.")

(defun nsa/research-approval--git-result (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (cons (apply #'process-file
                   "git" nil (list (current-buffer) t) nil arguments)
            (string-trim-right (buffer-string))))))

(defun nsa/research-approval--git (directory &rest arguments)
  "Run Git with ARGUMENTS and signal a `user-error' on failure."
  (pcase-let ((`(,status . ,output)
               (apply #'nsa/research-approval--git-result
                      directory arguments)))
    (unless (and (integerp status) (zerop status))
      (user-error "Git %s failed%s"
                  (string-join arguments " ")
                  (if (string-empty-p output) "" (format ": %s" output))))
    output))

(defun nsa/research-approval--git-lines (directory &rest arguments)
  "Run Git with ARGUMENTS and return nonempty output lines."
  (split-string
   (apply #'nsa/research-approval--git directory arguments) "\n" t))

(defun nsa/research-approval--one (description values)
  "Return the only member of VALUES or reject ambiguous DESCRIPTION."
  (unless (= (length values) 1)
    (user-error "Cannot prove %s: expected one value, found %d"
                description (length values)))
  (car values))

(defun nsa/research-approval--eligible-path-p (path)
  "Return non-nil when PATH is a tracked research Org record."
  (string-match-p nsa/research-approval--path-regexp path))

(defun nsa/research-approval--github-url-p (url)
  "Return non-nil when URL is an unambiguous GitHub repository URL."
  (string-match-p
   (concat "\\`\\(?:git@github\\.com:"
           "\\|ssh://git@github\\.com/"
           "\\|https://github\\.com/\\)"
           "[^/[:space:]]+/[^/[:space:]]+\\(?:\\.git\\)?/?\\'")
   url))

(defun nsa/research-approval--context (&optional require-clean)
  "Return verified Git context for the current buffer.
When REQUIRE-CLEAN is non-nil, reject every visible working-tree change."
  (unless (and buffer-file-name (file-regular-p buffer-file-name))
    (user-error "This buffer is not visiting a regular file"))
  (when (file-remote-p buffer-file-name)
    (user-error "Remote files are not supported"))
  (when (buffer-modified-p)
    (user-error "Save or discard existing buffer edits before approval"))
  (let* ((file (file-truename buffer-file-name))
         (directory (file-name-directory file))
         (root (file-name-as-directory
                (file-truename
                 (nsa/research-approval--git directory
                                              "rev-parse" "--show-toplevel"))))
         (path (file-relative-name file root)))
    (unless (nsa/research-approval--eligible-path-p path)
      (user-error "%s is not a research/*.org record" path))
    (nsa/research-approval--git root "ls-files" "--error-unmatch" "--" path)
    (when require-clean
      (let ((status (nsa/research-approval--git
                     root "status" "--porcelain=v1" "--untracked-files=all")))
        (unless (string-empty-p status)
          (user-error "Repository is dirty; approval requires a clean checkout:\n%s"
                      status))))
    (let* ((branch-result
            (nsa/research-approval--git-result
             root "symbolic-ref" "--quiet" "--short" "HEAD"))
           (branch (cdr branch-result)))
      (unless (and (zerop (car branch-result))
                   (not (string-empty-p branch)))
        (user-error "Detached HEAD: approval requires a named current branch"))
      (let* ((remote
              (nsa/research-approval--one
               (format "remote for branch %s" branch)
               (nsa/research-approval--git-lines
                root "config" "--get-all" (format "branch.%s.remote" branch))))
             (merge-ref
              (nsa/research-approval--one
               (format "upstream branch for %s" branch)
               (nsa/research-approval--git-lines
                root "config" "--get-all" (format "branch.%s.merge" branch)))))
        (when (string= remote ".")
          (user-error "Branch %s tracks a local branch, not GitHub" branch))
        (unless (string-prefix-p "refs/heads/" merge-ref)
          (user-error "Upstream %s is not a remote branch" merge-ref))
        (let* ((upstream-branch (string-remove-prefix "refs/heads/" merge-ref))
               (upstream
                (nsa/research-approval--git
                 root "rev-parse" "--abbrev-ref" "--symbolic-full-name"
                 "@{upstream}"))
               (expected-upstream (concat remote "/" upstream-branch))
               (push-url
                (nsa/research-approval--one
                 (format "push URL for remote %s" remote)
                 (nsa/research-approval--git-lines
                  root "remote" "get-url" "--push" "--all" remote))))
          (unless (string= upstream expected-upstream)
            (user-error "Upstream mismatch: configured %s, resolved %s"
                        expected-upstream upstream))
          (unless (nsa/research-approval--github-url-p push-url)
            (user-error "Push URL is not a proven GitHub repository: %s" push-url))
          (list :root root
                :path path
                :branch branch
                :remote remote
                :merge-ref merge-ref
                :upstream upstream
                :push-url push-url
                :head (nsa/research-approval--git root "rev-parse" "HEAD")))))))

(defun nsa/research-approval--field-regexp (field)
  "Return the exact canonical keyword regexp for FIELD."
  (format "^#\\+%s: \\(.*\\)$" (regexp-quote field)))

(defun nsa/research-approval--field-values (field)
  "Return (LINE . VALUE) entries for canonical FIELD in the buffer."
  (let ((regexp (nsa/research-approval--field-regexp field))
        values)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (line-number-at-pos (match-beginning 0))
                    (match-string-no-properties 1))
              values)))
    (nreverse values)))

(defun nsa/research-approval--keyword-lines (keyword)
  "Return line numbers for case-insensitive Org KEYWORD entries."
  (let (lines)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward
                (format "^#\\+%s:[ \\t]*.*$" (regexp-quote keyword))
                nil t)
          (push (line-number-at-pos (match-beginning 0)) lines))))
    (nreverse lines)))

(defun nsa/research-approval--canonical-block-errors (&optional expected-state)
  "Return human-readable errors for the current record.
When EXPECTED-STATE is non-nil, require that approval state."
  (let ((titles (nsa/research-approval--keyword-lines "title"))
        (statuses (nsa/research-approval--keyword-lines "status"))
        errors)
    (unless (= (length titles) 1)
      (push (format "line 1: expected exactly one #+title keyword, found %d"
                    (length titles)) errors))
    (unless (= (length statuses) 1)
      (push (format "line 1: expected exactly one #+status keyword, found %d"
                    (length statuses)) errors))
    (when (and (= (length titles) 1) (= (length statuses) 1)
               (>= (car titles) (car statuses)))
      (push (format "line %d: #+title must precede lifecycle #+status"
                    (car statuses)) errors))
    (when (= (length statuses) 1)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (car statuses)))
        (let ((case-fold-search t))
          (when (re-search-forward "^#\\+status:[ \\t]*\\(.*\\)$"
                                   (line-end-position) t)
            (let ((lifecycle (upcase (string-trim (match-string-no-properties 1)))))
              (when (member lifecycle '("APPROVED" "REJECTED"))
                (push (format "line %d: lifecycle status cannot be %s; use #+approval_state"
                              (car statuses) lifecycle)
                      errors)))))))
    (dolist (field nsa/research-approval--fields)
      (let ((values (nsa/research-approval--field-values field)))
        (unless (= (length values) 1)
          (push (format "line 1: approval field #+%s must occur exactly once, found %d"
                        field (length values)) errors))))
    (when (= (length statuses) 1)
      (let ((status-line (car statuses)))
        (cl-loop for field in nsa/research-approval--fields
                 for offset from 1
                 for expected-line = (+ status-line offset)
                 for expected =
                 (if (string= field "approval_schema")
                     (format "#+%s: %s" field nsa/research-approval--schema)
                   (let ((value (cdr (car (nsa/research-approval--field-values
                                           field)))))
                     (format "#+%s: %s" field (or value ""))))
                 do (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- expected-line))
                      (let ((actual (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
                        (unless (string= actual expected)
                          (push (format "line %d: canonical approval block expected #+%s immediately after #+status"
                                        expected-line field)
                                errors)))))))
    (let ((case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "^[ \\t]*#\\+\\(?:approval\\|approved\\|approve\\|reject\\)[[:alnum:]_-]*:"
                nil t)
          (let ((line (line-number-at-pos (match-beginning 0)))
                (text (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (or (string-match-p
                         "\\`#\\+approval_schema: prolog-rlm\\.research-approval\\.v1\\'"
                         text)
                        (string-match-p
                         "\\`#\\+approval_\\(?:state\\|actor\\|evidence\\|base_commit\\|base_blob\\|decided_at\\): "
                         text))
              (push (format "line %d: noncanonical approval keyword or layout"
                            line) errors)))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "^[ \\t]*:[^: \\t]*\\(?:approval\\|approve\\|reject\\)[^:]*:"
                nil t)
          (push (format "line %d: noncanonical approval property"
                        (line-number-at-pos (match-beginning 0))) errors)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "^#\\+property:.*\\(?:approval\\|approve\\|reject\\)"
                nil t)
          (push (format "line %d: noncanonical approval property"
                        (line-number-at-pos (match-beginning 0))) errors)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "^[ \\t]*\\(?:[-+*][ \\t]+\\)?\\[[xX]\\][ \\t]+\\(?:APPROVE[D]?\\|REJECT[ED]?\\)\\(?:[ \\t]\\|$\\)"
                nil t)
          (push (format "line %d: checked approval box is not authoritative; use #+approval_state"
                        (line-number-at-pos (match-beginning 0))) errors))))
    (let* ((state-values (nsa/research-approval--field-values "approval_state"))
           (state (and state-values (cdr (car state-values)))))
      (when (= (length state-values) 1)
        (unless (member state '("PENDING" "APPROVED" "REJECTED"))
          (push (format "line %d: approval_state must be exactly PENDING, APPROVED, or REJECTED"
                        (caar state-values)) errors))
        (when (string= state "PENDING")
          (dolist (field (cdr (cdr nsa/research-approval--fields)))
            (let ((values (nsa/research-approval--field-values field)))
              (when values
                (let ((value (cdr (car values))))
                  (unless (string= value "NONE")
                    (push (format "line %d: PENDING %s must be NONE"
                                  (car (car values)) field)
                          errors)))))))
        (when (and expected-state (not (string= state expected-state)))
          (push (format "line %d: approval command requires state %s, found %s"
                        (caar state-values) expected-state state) errors))))
    (nreverse errors)))

(defun nsa/research-approval--validate-record (&optional expected-state)
  "Signal a `user-error' unless the current record is canonical."
  (let ((errors (nsa/research-approval--canonical-block-errors expected-state)))
    (when errors
      (user-error "Invalid research approval layout:\n%s"
                  (string-join errors "\n")))))

(defun nsa/research-approval--only-intended-edit-p (context)
  "Return non-nil when CONTEXT's file is the sole unstaged edit."
  (let* ((root (plist-get context :root))
         (path (plist-get context :path)))
    (and (equal (nsa/research-approval--git-lines
                 root "status" "--porcelain=v1" "--untracked-files=all")
                (list (concat " M " path)))
         (equal (nsa/research-approval--git-lines root "diff" "--name-only" "--")
                (list path))
         (null (nsa/research-approval--git-lines
                root "diff" "--cached" "--name-only" "--")))))

(defun nsa/research-approval--approval-diff-p (context)
  "Return non-nil when only canonical approval fields changed."
  (let* ((root (plist-get context :root))
         (path (plist-get context :path))
         (diff (nsa/research-approval--git
                root "diff" "--no-ext-diff" "--no-color" "--unified=0"
                "--" path))
         (lines (split-string diff "\n" t)))
    (and lines
         (cl-every
          (lambda (line)
            (or (string-prefix-p "@@" line)
                (string-prefix-p "diff --" line)
                (string-prefix-p "index " line)
                (string-prefix-p "---" line)
                (string-prefix-p "+++" line)
                (and (string-match-p "^[+-]#\\+approval_[a-z_]+: " line)
                     (not (string-prefix-p " " line)))))
          lines))))

(defun nsa/research-approval--same-target-p (before after)
  "Return non-nil when BEFORE and AFTER describe the same checkout target."
  (cl-every (lambda (key) (equal (plist-get before key) (plist-get after key)))
            '(:root :path :branch :remote :merge-ref :upstream :push-url :head)))

(defun nsa/research-approval--same-push-target-p (before after)
  "Return non-nil when BEFORE and AFTER have the same push target."
  (cl-every (lambda (key) (equal (plist-get before key) (plist-get after key)))
            '(:root :path :branch :remote :merge-ref :upstream :push-url)))

(defun nsa/research-approval--restore-buffer (contents point)
  "Restore the current buffer to CONTENTS and POINT, then save it."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert contents)
    (goto-char (min point (point-max)))
    (save-buffer)))

(defun nsa/research-approval--replace-field (field value)
  "Replace the sole canonical FIELD value with VALUE."
  (let ((values (nsa/research-approval--field-values field)))
    (unless (= (length values) 1)
      (user-error "Cannot replace #+%s: expected one canonical field" field))
    (goto-char (point-min))
    (unless (re-search-forward (nsa/research-approval--field-regexp field) nil t)
      (user-error "Cannot replace missing #+%s" field))
    (replace-match (format "#+%s: %s" field value) t t)))

(defun nsa/research-approval--safe-value-p (value)
  "Return non-nil when VALUE is a nonempty, one-line field value."
  (and (stringp value)
       (not (string= value "NONE"))
       (not (string-match-p (string 10) value))
       (not (string-match-p (string 13) value))
       (string-match-p "\\`[^ \\t]" value)
       (string-match-p "[^ \\t]\\'" value)))

(defun nsa/research-approval--verify-binding (context base-commit base-blob)
  "Verify that BASE-COMMIT:path resolves exactly to BASE-BLOB."
  (let* ((root (plist-get context :root))
         (path (plist-get context :path))
         (commit-spec (concat base-commit "^{commit}"))
         (object-spec (concat base-commit ":" path)))
    (unless (string= (nsa/research-approval--git root "rev-parse" "--verify"
                                                  commit-spec)
                     base-commit)
      (user-error "Base commit is not a proven commit object"))
    (unless (and (string= (nsa/research-approval--git root "cat-file" "-t"
                                                       object-spec)
                          "blob")
                 (string= (nsa/research-approval--git root "rev-parse" "--verify"
                                                       object-spec)
                          base-blob))
      (user-error "Base commit does not resolve to the recorded file blob"))
    t))

(defun nsa/research-approval--commit-verified-p (context base-commit)
  "Return non-nil when HEAD is a verified approval-only child of BASE-COMMIT."
  (let* ((root (plist-get context :root))
         (path (plist-get context :path))
         (parent (nsa/research-approval--git root "rev-parse" "HEAD^"))
         (files (nsa/research-approval--git-lines
                 root "diff-tree" "--no-commit-id" "--name-only" "-r" "HEAD")))
    (let ((clean (string-empty-p
                  (nsa/research-approval--git root "status" "--porcelain=v1"
                                               "--untracked-files=all"))))
      (and (string= parent base-commit)
           (equal files (list path))
           clean))))

(defun nsa/research-approval--prompt-value (prompt default)
  "Read a safe nonempty field value for PROMPT, using DEFAULT."
  (let ((value (read-string prompt default)))
    (unless (nsa/research-approval--safe-value-p value)
      (user-error "Approval field must be a nonempty one-line value other than NONE"))
    value))

(defun nsa/research-approval--preview (context diff base-commit base-blob state)
  "Display DIFF and its verified approval target before confirmation."
  (let ((buffer (get-buffer-create "*Research approval preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Research approval dry-run\n  file: %s\n  state: %s\n  branch: %s\n"
                        (plist-get context :path) state
                        (plist-get context :branch)))
        (insert (format "  upstream: %s\n  push URL: %s\n"
                        (plist-get context :upstream)
                        (plist-get context :push-url)))
        (insert (format "  base commit: %s\n  base blob: %s\n\n"
                        base-commit base-blob))
        (insert diff "\n")
        (diff-mode)
        (goto-char (point-min))))
    (display-buffer buffer)))

(defun nsa/research-approval--decide-and-push (state)
  "Record STATE for the current research record, commit it, and push upstream."
  (interactive)
  (let* ((context (nsa/research-approval--context t))
         (root (plist-get context :root))
         (path (plist-get context :path))
         (_validated (progn
                       (nsa/research-approval--validate-record "PENDING")
                       t))
         (base-commit (plist-get context :head))
         (base-blob (nsa/research-approval--git
                     root "rev-parse" "--verify"
                     (concat base-commit ":" path)))
         (_binding (progn
                     (nsa/research-approval--verify-binding
                      context base-commit base-blob)
                     t))
         (actor (nsa/research-approval--prompt-value
                 "Human decision-maker: "
                 (nsa/research-approval--git root "config" "user.name")))
         (evidence (nsa/research-approval--prompt-value
                    "Durable approval evidence: " ""))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))
         (original (buffer-substring-no-properties (point-min) (point-max)))
         (original-point (point))
         (approval-contents nil)
         (committed nil))
    (nsa/research-approval--git
     root "push" "--dry-run" "--porcelain"
     (plist-get context :remote) (concat "HEAD:" (plist-get context :merge-ref)))
    (unwind-protect
        (progn
          (dolist (field `(("approval_state" . ,state)
                           ("approval_actor" . ,actor)
                           ("approval_evidence" . ,evidence)
                           ("approval_base_commit" . ,base-commit)
                           ("approval_base_blob" . ,base-blob)
                           ("approval_decided_at" . ,timestamp)))
            (nsa/research-approval--replace-field (car field) (cdr field)))
          (setq approval-contents
                (buffer-substring-no-properties (point-min) (point-max)))
          (save-buffer)
          (unless (equal approval-contents
                         (buffer-substring-no-properties (point-min) (point-max)))
            (user-error "A save hook changed more than the approval fields"))
          (nsa/research-approval--validate-record state)
          (unless (and (nsa/research-approval--only-intended-edit-p context)
                       (nsa/research-approval--approval-diff-p context))
            (user-error "Repository changed beyond the canonical approval fields"))
          (nsa/research-approval--preview
           context
           (nsa/research-approval--git
            root "diff" "--no-ext-diff" "--no-color" "--unified=0" "--" path)
           base-commit base-blob state)
          (let ((current (nsa/research-approval--context nil)))
            (unless (nsa/research-approval--same-target-p context current)
              (user-error "Checkout or push target changed; refusing to commit")))
          (unless (yes-or-no-p
                   (format "Commit only %s as %s and push to %s? "
                           path state (plist-get context :upstream)))
            (user-error "Approval cancelled; file restored"))
          (nsa/research-approval--git
           root "commit" "--only" "-m"
           (format "docs(research): %s %s"
                   (downcase state) (file-name-base path))
           "--" path)
          (setq committed t)
          (unless (and (nsa/research-approval--commit-verified-p
                        context base-commit)
                       (nsa/research-approval--verify-binding
                         context base-commit base-blob))
            (user-error "Local approval commit verification failed; nothing was pushed"))
          (let ((current (nsa/research-approval--context t)))
            (unless (nsa/research-approval--same-push-target-p context current)
              (user-error "Push target changed after commit; approval commit remains local")))
          (condition-case error-data
              (nsa/research-approval--git
               root "push" "--porcelain"
               (plist-get context :remote)
               (concat "HEAD:" (plist-get context :merge-ref)))
            (error
             (user-error "Approval commit %s is verified locally but push failed: %s"
                         (nsa/research-approval--git root "rev-parse" "HEAD")
                         (error-message-string error-data))))
          (message "Recorded %s for %s, verified commit, and pushed %s"
                   state path (plist-get context :upstream)))
      (unless committed
        (when (and approval-contents
                   (equal (buffer-substring-no-properties (point-min) (point-max))
                          (with-temp-buffer
                            (insert-file-contents buffer-file-name)
                            (buffer-string)))
                   (string= (nsa/research-approval--git root "rev-parse" "HEAD")
                            base-commit)
                   (equal (nsa/research-approval--git-lines
                           root "status" "--porcelain=v1" "--untracked-files=all")
                          (list (concat " M " path)))
                   (equal (nsa/research-approval--git-lines
                           root "diff" "--name-only" "--" path)
                          (list path)))
          (ignore-errors
            (nsa/research-approval--git root "restore" "--staged" "--" path))
          (nsa/research-approval--restore-buffer original original-point))))))

;;;###autoload
(defun nsa/research-approve-and-push ()
  "Record APPROVED for the current canonical research record and push it."
  (interactive)
  (nsa/research-approval--decide-and-push "APPROVED"))

;;;###autoload
(defun nsa/research-reject-and-push ()
  "Record REJECTED for the current canonical research record and push it."
  (interactive)
  (nsa/research-approval--decide-and-push "REJECTED"))

(provide 'research-approval)
