;;; research-dashboard.el --- Cross-repository research review UI -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)

(defgroup nsa/research-dashboard nil
  "Review research across GitHub repositories."
  :group 'tools)

(defcustom nsa/research-dashboard-owners nil
  "Owners to scan.  Nil means the authenticated `gh' user plus visible orgs."
  :type '(repeat string))

(defconst nsa/research-dashboard--schema "prolog-rlm.research-approval.v1")
(defconst nsa/research-dashboard--fields
  '("approval_schema" "approval_state" "approval_actor"
    "approval_evidence" "approval_base_commit" "approval_base_blob"
    "approval_decided_at"))

(cl-defstruct (nsa/research-item (:constructor nsa/research-item-create))
  repo branch path blob title lifecycle approval content)

(defvar-local nsa/research-dashboard--items nil)
(defvar-local nsa/research-dashboard--errors nil)
(defvar-local nsa/research-dashboard--generation 0)
(defvar-local nsa/research-dashboard--scanning nil)

(defvar nsa/research-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'nsa/research-dashboard-refresh)
    (define-key map (kbd "RET") #'nsa/research-dashboard-view)
    (define-key map (kbd "a") #'nsa/research-dashboard-approve)
    (define-key map (kbd "r") #'nsa/research-dashboard-reject)
    (define-key map (kbd "e") #'nsa/research-dashboard-errors)
    map))

(defun nsa/research-dashboard--gh (&rest args)
  "Run `gh' with ARGS and return stdout."
  (unless (executable-find "gh")
    (user-error "GitHub CLI `gh' is required"))
  (with-temp-buffer
    (let ((status (apply #'process-file "gh" nil (list (current-buffer) t) nil args)))
      (unless (and (integerp status) (zerop status))
        (error "gh %s failed: %s" (string-join args " ")
               (string-trim (buffer-string))))
      (buffer-string))))

(defun nsa/research-dashboard--json (text)
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun nsa/research-dashboard--get (key object)
  (alist-get key object nil nil #'string=))

(defun nsa/research-dashboard--keyword (content keyword)
  "Return the first Org KEYWORD before the first heading."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (limit (or (and (re-search-forward "^\\*" nil t)
                          (line-beginning-position))
                     (point-max))))
      (goto-char (point-min))
      (when (re-search-forward
             (format "^#\\+%s:[ \\t]*\\(.*\\)$" (regexp-quote keyword))
             limit t)
        (string-trim (match-string-no-properties 1))))))

(defun nsa/research-dashboard--candidate-path-p (path)
  (and (stringp path)
       (string-suffix-p ".org" path t)
       (not (string-prefix-p "../" path))
       (or (string-prefix-p "research/" path)
           (string-match-p "/research/" path))))

(defun nsa/research-dashboard--legacy-open-p (repo lifecycle)
  (let ((state (upcase (or lifecycle ""))))
    (if (string= repo "lost-rob0t/starintel-auto-research")
        (not (member state '("DONE" "REJECTED")))
      (not (member state '("DONE" "REJECTED" "CLOSED" "ARCHIVED"))))))

(defun nsa/research-dashboard--item (repo branch path blob content)
  (let* ((title (or (nsa/research-dashboard--keyword content "title")
                    (file-name-base path)))
         (lifecycle (or (nsa/research-dashboard--keyword content "status") "MISSING"))
         (approval (upcase (or (nsa/research-dashboard--keyword
                                content "approval_state") "LEGACY"))))
    (when (or (string= approval "PENDING")
              (and (string= approval "LEGACY")
                   (nsa/research-dashboard--legacy-open-p repo lifecycle)))
      (nsa/research-item-create :repo repo :branch branch :path path :blob blob
                                :title title :lifecycle lifecycle
                                :approval approval :content content))))

(defun nsa/research-dashboard--owners ()
  (or nsa/research-dashboard-owners
      (let* ((login (string-trim
                     (nsa/research-dashboard--gh "api" "user" "--jq" ".login")))
             (orgs (split-string
                    (nsa/research-dashboard--gh
                     "api" "--paginate" "user/orgs" "--jq" ".[].login")
                    "\n" t)))
        (delete-dups (cons login orgs)))))

(defun nsa/research-dashboard--search (scope)
  "Return code-search items for research paths in SCOPE."
  (let* ((query (format "research in:path extension:org %s" scope))
         (pages (nsa/research-dashboard--json
                 (nsa/research-dashboard--gh
                  "api" "--method" "GET" "--paginate" "--slurp"
                  "search/code" "-f" (concat "q=" query) "-f" "per_page=100"))))
    (apply #'append
           (mapcar (lambda (page) (nsa/research-dashboard--get "items" page))
                   pages))))

(defun nsa/research-dashboard--blob-content (repo blob)
  (let* ((object (nsa/research-dashboard--json
                  (nsa/research-dashboard--gh
                   "api" (format "repos/%s/git/blobs/%s" repo blob))))
         (encoding (nsa/research-dashboard--get "encoding" object))
         (content (nsa/research-dashboard--get "content" object)))
    (unless (and (string= encoding "base64") (stringp content))
      (error "unsupported blob encoding for %s:%s" repo blob))
    (decode-coding-string (base64-decode-string content) 'utf-8)))

(defun nsa/research-dashboard--repo-branch (repo-object repo)
  (or (nsa/research-dashboard--get "default_branch" repo-object)
      (string-trim
       (nsa/research-dashboard--gh
        "repo" "view" repo "--json" "defaultBranchRef"
        "--jq" ".defaultBranchRef.name"))))

(defun nsa/research-dashboard--scan ()
  "Scan all configured GitHub owners and return (ITEMS ERRORS)."
  (let (items errors seen)
    (dolist (owner (nsa/research-dashboard--owners))
      (let* ((login (string-trim
                     (nsa/research-dashboard--gh "api" "user" "--jq" ".login")))
             (scope (if (string= owner login)
                        (concat "user:" owner)
                      (concat "org:" owner))))
        (condition-case error-data
            (dolist (hit (nsa/research-dashboard--search scope))
              (let* ((path (nsa/research-dashboard--get "path" hit))
                     (blob (nsa/research-dashboard--get "sha" hit))
                     (repo-object (nsa/research-dashboard--get "repository" hit))
                     (repo (nsa/research-dashboard--get "full_name" repo-object))
                     (id (and repo path (concat repo ":" path))))
                (when (and id (not (member id seen))
                           (nsa/research-dashboard--candidate-path-p path))
                  (push id seen)
                  (condition-case file-error
                      (let* ((branch (nsa/research-dashboard--repo-branch
                                      repo-object repo))
                             (content (nsa/research-dashboard--blob-content repo blob))
                             (item (nsa/research-dashboard--item
                                    repo branch path blob content)))
                        (when item (push item items)))
                    (error
                     (push (format "%s: %s" id
                                   (error-message-string file-error)) errors))))))
          (error
           (push (format "%s: %s" owner (error-message-string error-data)) errors)))))
    (list items errors)))

(defun nsa/research-dashboard--row (item)
  (let* ((path (nsa/research-item-path item))
         (parts (split-string path "/" t))
         (project (or (cadr (member "research" parts)) "(root)")))
    (list (concat (nsa/research-item-repo item) ":" path)
          (vector (nsa/research-item-approval item)
                  (nsa/research-item-repo item)
                  project
                  (nsa/research-item-lifecycle item)
                  (nsa/research-item-title item)
                  path))))

(defun nsa/research-dashboard--render ()
  (setq tabulated-list-entries
        (mapcar #'nsa/research-dashboard--row nsa/research-dashboard--items))
  (setq header-line-format
        (format "Open research: %d%s   errors: %d"
                (length nsa/research-dashboard--items)
                (if nsa/research-dashboard--scanning "   [scanning]" "")
                (length nsa/research-dashboard--errors)))
  (tabulated-list-print t))

(defun nsa/research-dashboard-refresh ()
  "Refresh open research without blocking the Emacs UI."
  (interactive)
  (cl-incf nsa/research-dashboard--generation)
  (let ((generation nsa/research-dashboard--generation)
        (buffer (current-buffer)))
    (setq nsa/research-dashboard--scanning t
          nsa/research-dashboard--items nil
          nsa/research-dashboard--errors nil)
    (nsa/research-dashboard--render)
    (make-thread
     (lambda ()
       (condition-case error-data
           (let ((result (nsa/research-dashboard--scan)))
             (run-at-time
              0 nil
              (lambda ()
                (when (and (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (= generation nsa/research-dashboard--generation)))
                  (with-current-buffer buffer
                    (setq nsa/research-dashboard--items (car result)
                          nsa/research-dashboard--errors (cadr result)
                          nsa/research-dashboard--scanning nil)
                    (nsa/research-dashboard--render))))))
         (error
          (run-at-time
           0 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq nsa/research-dashboard--errors
                       (list (error-message-string error-data))
                       nsa/research-dashboard--scanning nil)
                 (nsa/research-dashboard--render)))))))))))

(defun nsa/research-dashboard--at-point ()
  (let ((id (tabulated-list-get-id)))
    (or (seq-find (lambda (item)
                    (string= id (concat (nsa/research-item-repo item) ":"
                                        (nsa/research-item-path item))))
                  nsa/research-dashboard--items)
        (user-error "No research item on this row"))))

(defun nsa/research-dashboard-view ()
  "Read the selected research item."
  (interactive)
  (let* ((item (nsa/research-dashboard--at-point))
         (buffer (get-buffer-create (format "*Research: %s*"
                                            (nsa/research-item-title item)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (nsa/research-item-content item))
        (org-mode)
        (read-only-mode 1)
        (setq-local header-line-format
                    (format "%s — %s" (nsa/research-item-repo item)
                            (nsa/research-item-path item)))))
    (pop-to-buffer buffer)))

(defun nsa/research-dashboard-errors ()
  "Show scan errors."
  (interactive)
  (with-output-to-temp-buffer "*Research Dashboard Errors*"
    (princ (if nsa/research-dashboard--errors
               (string-join (reverse nsa/research-dashboard--errors) "\n")
             "No scan errors."))))

(defun nsa/research-dashboard--encode-path (path)
  (mapconcat #'url-hexify-string (split-string path "/" t) "/"))

(defun nsa/research-dashboard--branch-head (repo branch)
  (let* ((object (nsa/research-dashboard--json
                  (nsa/research-dashboard--gh
                   "api" (format "repos/%s/branches/%s"
                                 repo (url-hexify-string branch)))))
         (commit (nsa/research-dashboard--get "commit" object)))
    (nsa/research-dashboard--get "sha" commit)))

(defun nsa/research-dashboard--remote-file (repo path commit)
  (let* ((object (nsa/research-dashboard--json
                  (nsa/research-dashboard--gh
                   "api" "--method" "GET"
                   (format "repos/%s/contents/%s"
                           repo (nsa/research-dashboard--encode-path path))
                   "-f" (concat "ref=" commit))))
         (blob (nsa/research-dashboard--get "sha" object))
         (content (nsa/research-dashboard--get "content" object)))
    (cons blob (decode-coding-string (base64-decode-string content) 'utf-8))))

(defun nsa/research-dashboard--field-regexp (field)
  (format "^#\\+%s:[ \\t]*.*$" (regexp-quote field)))

(defun nsa/research-dashboard--has-approval-field-p (content)
  (seq-some (lambda (field)
              (string-match-p (nsa/research-dashboard--field-regexp field) content))
            nsa/research-dashboard--fields))

(defun nsa/research-dashboard--canonical-p (content)
  (string-match-p
   (concat "^#\\+status:.*\n"
           "#\\+approval_schema: " (regexp-quote nsa/research-dashboard--schema) "\n"
           "#\\+approval_state: PENDING\n"
           "#\\+approval_actor: .*\n"
           "#\\+approval_evidence: .*\n"
           "#\\+approval_base_commit: .*\n"
           "#\\+approval_base_blob: .*\n"
           "#\\+approval_decided_at: .*$")
   content))

(defun nsa/research-dashboard--replace-field (content field value)
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (unless (re-search-forward (nsa/research-dashboard--field-regexp field) nil t)
      (user-error "Missing #+%s" field))
    (replace-match (format "#+%s: %s" field value) t t)
    (when (re-search-forward (nsa/research-dashboard--field-regexp field) nil t)
      (user-error "Duplicate #+%s" field))
    (buffer-string)))

(defun nsa/research-dashboard--decision-content
    (content state actor evidence commit blob timestamp)
  (let ((values `(("approval_schema" . ,nsa/research-dashboard--schema)
                  ("approval_state" . ,state)
                  ("approval_actor" . ,actor)
                  ("approval_evidence" . ,evidence)
                  ("approval_base_commit" . ,commit)
                  ("approval_base_blob" . ,blob)
                  ("approval_decided_at" . ,timestamp))))
    (cond
     ((nsa/research-dashboard--canonical-p content)
      (dolist (pair values content)
        (setq content (nsa/research-dashboard--replace-field
                       content (car pair) (cdr pair)))))
     ((nsa/research-dashboard--has-approval-field-p content)
      (user-error "Partial/noncanonical approval metadata exists"))
     (t
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (unless (re-search-forward "^#\\+status:.*$" nil t)
            (user-error "Missing #+status; lifecycle will not be invented"))
          (end-of-line)
          (insert "\n" (mapconcat
                        (lambda (field)
                          (format "#+%s: %s" field
                                  (alist-get field values nil nil #'string=)))
                        nsa/research-dashboard--fields "\n")))
        (buffer-string))))))

(defun nsa/research-dashboard--decide (state)
  (let* ((item (nsa/research-dashboard--at-point))
         (repo (nsa/research-item-repo item))
         (branch (nsa/research-item-branch item))
         (path (nsa/research-item-path item))
         (commit (nsa/research-dashboard--branch-head repo branch))
         (remote (nsa/research-dashboard--remote-file repo path commit))
         (blob (car remote))
         (content (cdr remote))
         (actor (read-string "Human decision-maker: "
                             (string-trim (nsa/research-dashboard--gh
                                           "api" "user" "--jq" ".login"))))
         (evidence (read-string "Durable approval evidence: "
                                "human:emacs-research-dashboard"))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))
         (updated (nsa/research-dashboard--decision-content
                   content state actor evidence commit blob timestamp)))
    (unless (and (not (string-empty-p (string-trim actor)))
                 (not (string-empty-p (string-trim evidence))))
      (user-error "Actor and evidence must be nonempty"))
    (with-output-to-temp-buffer "*Research Approval Preview*"
      (princ (format "%s\n%s\n%s\n\n%s"
                     repo path commit
                     (mapconcat
                      (lambda (field)
                        (format "#+%s: %s" field
                                (nsa/research-dashboard--keyword updated field)))
                      nsa/research-dashboard--fields "\n"))))
    (unless (yes-or-no-p (format "%s %s:%s? " state repo path))
      (user-error "Decision cancelled"))
    (let* ((head2 (nsa/research-dashboard--branch-head repo branch))
           (remote2 (nsa/research-dashboard--remote-file repo path head2)))
      (unless (and (string= commit head2) (string= blob (car remote2))
                   (string= content (cdr remote2)))
        (user-error "Research changed during review; refresh first")))
    (nsa/research-dashboard--gh
     "api" "--method" "PUT"
     (format "repos/%s/contents/%s" repo (nsa/research-dashboard--encode-path path))
     "-f" (format "message=docs(research): %s %s"
                  (downcase state) (file-name-base path))
     "-f" (concat "content=" (base64-encode-string updated t))
     "-f" (concat "sha=" blob) "-f" (concat "branch=" branch))
    (message "%s %s:%s via gh API" state repo path)
    (nsa/research-dashboard-refresh)))

(defun nsa/research-dashboard-approve () (interactive)
  (nsa/research-dashboard--decide "APPROVED"))
(defun nsa/research-dashboard-reject () (interactive)
  (nsa/research-dashboard--decide "REJECTED"))

(define-derived-mode nsa/research-dashboard-mode tabulated-list-mode "Research"
  "Cross-repository human research approval."
  (setq tabulated-list-format
        [("Approval" 10 t) ("Repository" 30 t) ("Project" 18 t)
         ("Lifecycle" 12 t) ("Title" 48 t) ("Path" 60 t)])
  (setq tabulated-list-sort-key '("Repository" . nil))
  (tabulated-list-init-header))

;;;###autoload
(defun nsa/research-dashboard ()
  "Show open research across the authenticated GitHub account and organizations."
  (interactive)
  (let ((buffer (get-buffer-create "*Research Dashboard*")))
    (with-current-buffer buffer
      (nsa/research-dashboard-mode)
      (nsa/research-dashboard-refresh))
    (pop-to-buffer buffer)))

(provide 'research-dashboard)
;;; research-dashboard.el ends here
