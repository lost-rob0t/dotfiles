;;; research-dashboard.el --- Async cross-repository research review UI -*- lexical-binding: t; -*-

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

(defcustom nsa/research-dashboard-max-workers 6
  "Maximum concurrent research-file fetches."
  :type 'integer)

(defcustom nsa/research-dashboard-show-legacy-default nil
  "Whether unmigrated review-ready research is visible when the dashboard opens."
  :type 'boolean)

(defconst nsa/research-dashboard--auxiliary-files
  '("index.org" "sources.org" "search-log.org"))

(defconst nsa/research-dashboard--schema "prolog-rlm.research-approval.v1")
(defconst nsa/research-dashboard--approval-pr-marker
  "<!-- starintel-research-approval:v1 -->")
(defconst nsa/research-dashboard--fields
  '("approval_schema" "approval_state" "approval_actor" "approval_evidence"
    "approval_base_commit" "approval_base_blob" "approval_decided_at"))
(defconst nsa/research-dashboard--legacy-reviewable-lifecycles
  '("REVIEW" "RESEARCHED" "VERIFIED"))

(defface nsa/research-dashboard-pending-face
  '((t :inherit warning :weight bold))
  "Face for pending approval.")
(defface nsa/research-dashboard-review-face
  '((t :inherit font-lock-keyword-face :weight semibold))
  "Face for active research lifecycle states.")
(defface nsa/research-dashboard-repo-face
  '((t :inherit font-lock-constant-face :weight semibold))
  "Face for repository names.")
(defface nsa/research-dashboard-project-face
  '((t :inherit font-lock-type-face))
  "Face for project names.")
(defface nsa/research-dashboard-lifecycle-face
  '((t :inherit font-lock-keyword-face))
  "Face for lifecycle states.")
(defface nsa/research-dashboard-title-face
  '((t :inherit default :weight semibold))
  "Face for research titles.")
(defface nsa/research-dashboard-path-face
  '((t :inherit shadow))
  "Face for repository paths.")
(defface nsa/research-dashboard-unmigrated-face
  '((t :inherit shadow :slant italic))
  "Face for old-format reviewable research.")
(defface nsa/research-dashboard-busy-face
  '((t :inherit success :weight bold))
  "Face for decisions being written.")

(defconst nsa/research-dashboard-font-lock-keywords
  '(("\\_<PENDING\\_>" . 'nsa/research-dashboard-pending-face)
    ("\\_<UNMIGRATED\\_>" . 'nsa/research-dashboard-unmigrated-face)
    ("\\_<\\(REVIEW\\|RESEARCHED\\|VERIFIED\\|DONE\\)\\_>"
     . 'nsa/research-dashboard-lifecycle-face)))

(cl-defstruct (nsa/research-item (:constructor nsa/research-item-create))
  repo branch path blob title lifecycle approval content busy)
(cl-defstruct (nsa/research-decision (:constructor nsa/research-decision-create))
  dashboard item state actor evidence commit blob content updated
  approval-branch approval-commit pr-number pr-url)

(defvar-local nsa/research-dashboard--items nil)
(defvar-local nsa/research-dashboard--errors nil)
(defvar-local nsa/research-dashboard--generation 0)
(defvar-local nsa/research-dashboard--scanning nil)
(defvar-local nsa/research-dashboard--login nil)
(defvar-local nsa/research-dashboard--processes nil)
(defvar-local nsa/research-dashboard--queue nil)
(defvar-local nsa/research-dashboard--active 0)
(defvar-local nsa/research-dashboard--searches-left 0)
(defvar-local nsa/research-dashboard--seen nil)
(defvar-local nsa/research-dashboard--render-timer nil)
(defvar-local nsa/research-dashboard--search-text "")
(defvar-local nsa/research-dashboard--field-filters nil)
(defvar-local nsa/research-dashboard--show-legacy nil)

(defun nsa/research-dashboard--mode-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'nsa/research-dashboard-refresh)
    (define-key map (kbd "RET") #'nsa/research-dashboard-view)
    (define-key map (kbd "a") #'nsa/research-dashboard-approve)
    (define-key map (kbd "r") #'nsa/research-dashboard-reject)
    (define-key map (kbd "e") #'nsa/research-dashboard-errors)
    (define-key map (kbd "s") #'nsa/research-dashboard-search)
    (define-key map (kbd "/") #'nsa/research-dashboard-search)
    (define-key map (kbd "f") #'nsa/research-dashboard-filter)
    (define-key map (kbd "c") #'nsa/research-dashboard-clear-filters)
    (define-key map (kbd "L") #'nsa/research-dashboard-toggle-legacy)
    (define-key map (kbd "?") #'nsa/research-dashboard-help)
    map))

(defvar nsa/research-dashboard-mode-map (nsa/research-dashboard--mode-map))

(defun nsa/research-dashboard--json (text)
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))
(defun nsa/research-dashboard--get (key object)
  (alist-get key object nil nil #'string=))

(defun nsa/research-dashboard--cancel ()
  (dolist (process nsa/research-dashboard--processes)
    (when (process-live-p process)
      (process-put process 'nsa-cancelled t)
      (delete-process process)))
  (setq nsa/research-dashboard--processes nil)
  (when (timerp nsa/research-dashboard--render-timer)
    (cancel-timer nsa/research-dashboard--render-timer))
  (setq nsa/research-dashboard--render-timer nil))

(cl-defun nsa/research-dashboard--gh (dashboard callback args &key input)
  "Run `gh' asynchronously; CALLBACK receives (OK OUTPUT)."
  (if (not (executable-find "gh"))
      (funcall callback nil "GitHub CLI `gh' is required")
    (let ((buffer (generate-new-buffer " *research-gh*")) process)
      (condition-case error-data
          (setq process
                (make-process
                 :name (make-temp-name "research-gh-")
                 :buffer buffer :command (cons "gh" args)
                 :connection-type 'pipe :coding 'utf-8-unix :noquery t
                 :sentinel
                 (lambda (proc _event)
                   (when (memq (process-status proc) '(exit signal))
                     (let ((ok (and (eq (process-status proc) 'exit)
                                    (zerop (process-exit-status proc))))
                           (output (if (buffer-live-p buffer)
                                       (with-current-buffer buffer (buffer-string)) "")))
                       (when (buffer-live-p dashboard)
                         (with-current-buffer dashboard
                           (setq nsa/research-dashboard--processes
                                 (delq proc nsa/research-dashboard--processes))))
                       (when (buffer-live-p buffer) (kill-buffer buffer))
                       (when (and (buffer-live-p dashboard)
                                  (not (process-get proc 'nsa-cancelled)))
                         (funcall callback ok output)))))))
        (error
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (funcall callback nil (error-message-string error-data))))
      (when process
        (with-current-buffer dashboard
          (push process nsa/research-dashboard--processes))
        (when input
          (condition-case error-data
              (progn (process-send-string process input)
                     (process-send-eof process))
            (error
             (process-put process 'nsa-cancelled t)
             (when (process-live-p process) (delete-process process))
             (funcall callback nil (error-message-string error-data))))))
      process)))

(defun nsa/research-dashboard--gh-json (dashboard callback args &optional input)
  (nsa/research-dashboard--gh
   dashboard
   (lambda (ok output)
     (if (not ok)
         (funcall callback nil (string-trim output))
       (condition-case error-data
           (funcall callback t (nsa/research-dashboard--json output))
         (error (funcall callback nil (error-message-string error-data))))))
   args :input input))

(defun nsa/research-dashboard--keyword (content keyword)
  (with-temp-buffer
    (insert content) (goto-char (point-min))
    (let ((case-fold-search t)
          (limit (or (and (re-search-forward "^\\*" nil t)
                          (line-beginning-position))
                     (point-max))))
      (goto-char (point-min))
      (when (re-search-forward
             (format "^#\\+%s:[ \\t]*\\(.*\\)$" (regexp-quote keyword)) limit t)
        (string-trim (match-string-no-properties 1))))))

(defun nsa/research-dashboard--candidate-path-p (path)
  (and (stringp path)
       (string-suffix-p ".org" path t)
       (not (string-prefix-p "../" path))
       (or (string-prefix-p "research/" path) (string-match-p "/research/" path))
       (not (member (downcase (file-name-nondirectory path))
                    nsa/research-dashboard--auxiliary-files))))

(defun nsa/research-dashboard--legacy-reviewable-p (lifecycle)
  (member (upcase (string-trim (or lifecycle "")))
          nsa/research-dashboard--legacy-reviewable-lifecycles))

(defun nsa/research-dashboard--item (repo branch path blob content)
  (let* ((title (or (nsa/research-dashboard--keyword content "title")
                    (file-name-base path)))
         (lifecycle (or (nsa/research-dashboard--keyword content "status") "MISSING"))
         (approval (upcase (or (nsa/research-dashboard--keyword content "approval_state")
                               "LEGACY"))))
    (when (and (nsa/research-dashboard--candidate-path-p path)
               (or (string= approval "PENDING")
                   (and (string= approval "LEGACY")
                        (nsa/research-dashboard--legacy-reviewable-p lifecycle))))
      (nsa/research-item-create
       :repo repo :branch branch :path path :blob blob :title title
       :lifecycle lifecycle :approval approval :content content))))

(defun nsa/research-dashboard--project (item)
  (let* ((parts (split-string (nsa/research-item-path item) "/" t))
         (after (cdr (member "research" parts))))
    (or (car after) "(root)")))

(defun nsa/research-dashboard--search-haystack (item)
  (mapconcat #'identity
             (list (nsa/research-item-repo item)
                   (nsa/research-dashboard--project item)
                   (nsa/research-item-lifecycle item)
                   (nsa/research-item-approval item)
                   (nsa/research-item-title item)
                   (nsa/research-item-path item))
             "\n"))

(defun nsa/research-dashboard--filter-value (item field)
  (pcase field
    ('repository (nsa/research-item-repo item))
    ('project (nsa/research-dashboard--project item))
    ('lifecycle (nsa/research-item-lifecycle item))
    ('approval (nsa/research-item-approval item))
    (_ "")))

(defun nsa/research-dashboard--visible-p (item)
  (and (or nsa/research-dashboard--show-legacy
           (not (string= (nsa/research-item-approval item) "LEGACY")))
       (or (string-empty-p nsa/research-dashboard--search-text)
           (string-match-p
            (regexp-quote (downcase nsa/research-dashboard--search-text))
            (downcase (nsa/research-dashboard--search-haystack item))))
       (seq-every-p
        (lambda (filter)
          (string= (downcase (cdr filter))
                   (downcase (nsa/research-dashboard--filter-value item (car filter)))))
        nsa/research-dashboard--field-filters)))

(defun nsa/research-dashboard--visible-items ()
  (seq-filter #'nsa/research-dashboard--visible-p nsa/research-dashboard--items))

(defun nsa/research-dashboard--face-state (value)
  (let ((state (upcase (or value ""))))
    (cond
     ((string= state "PENDING") 'nsa/research-dashboard-pending-face)
     ((member state '("REVIEW" "RESEARCHED" "RESEARCHING" "VERIFIED"
                      "DRAFT" "ACCEPTED-FOR-REALIZATION"))
      'nsa/research-dashboard-review-face)
     ((string= state "LEGACY") 'nsa/research-dashboard-unmigrated-face)
     (t 'default))))

(defun nsa/research-dashboard--row (item)
  (let ((approval (nsa/research-item-approval item))
        (lifecycle (nsa/research-item-lifecycle item)))
    (list
     (concat (nsa/research-item-repo item) ":" (nsa/research-item-path item))
     (vector
      (propertize
       (if (nsa/research-item-busy item)
           (concat (if (string= approval "LEGACY") "UNMIGRATED" approval) " …")
         (if (string= approval "LEGACY") "UNMIGRATED" approval))
       'face (if (nsa/research-item-busy item)
                 'nsa/research-dashboard-busy-face
               (if (string= approval "LEGACY")
                   'nsa/research-dashboard-unmigrated-face
                 (nsa/research-dashboard--face-state approval))))
      (propertize (nsa/research-item-repo item) 'face 'nsa/research-dashboard-repo-face)
      (propertize (nsa/research-dashboard--project item) 'face 'nsa/research-dashboard-project-face)
      (propertize lifecycle 'face 'nsa/research-dashboard-lifecycle-face)
      (propertize (nsa/research-item-title item) 'face 'nsa/research-dashboard-title-face)
      (propertize (nsa/research-item-path item) 'face 'nsa/research-dashboard-path-face)))))

(defun nsa/research-dashboard--filter-summary ()
  (string-join
   (delq nil
         (list
          (unless (string-empty-p nsa/research-dashboard--search-text)
            (format "search:%s" nsa/research-dashboard--search-text))
          (when nsa/research-dashboard--field-filters
            (mapconcat (lambda (filter) (format "%s:%s" (car filter) (cdr filter)))
                       (reverse nsa/research-dashboard--field-filters) ","))
          (when nsa/research-dashboard--show-legacy "unmigrated:on")))
   "  "))

(defun nsa/research-dashboard--render ()
  (let ((visible (nsa/research-dashboard--visible-items)))
    (setq tabulated-list-entries (mapcar #'nsa/research-dashboard--row visible)
          header-line-format
          (format
           "Research  %d/%d shown%s  queued:%d active:%d errors:%d   [/ search] [f filter] [c clear] [L unmigrated] [a approve] [r reject] [g refresh]%s"
           (length visible) (length nsa/research-dashboard--items)
           (if nsa/research-dashboard--scanning "  scanning…" "")
           (length nsa/research-dashboard--queue)
           nsa/research-dashboard--active
           (length nsa/research-dashboard--errors)
           (let ((summary (nsa/research-dashboard--filter-summary)))
             (if (string-empty-p summary) "" (concat "   " summary)))))
    (tabulated-list-print t)))

(defun nsa/research-dashboard--schedule-render ()
  (unless (timerp nsa/research-dashboard--render-timer)
    (let ((dashboard (current-buffer)))
      (setq nsa/research-dashboard--render-timer
            (run-at-time 0.05 nil
                         (lambda ()
                           (when (buffer-live-p dashboard)
                             (with-current-buffer dashboard
                               (setq nsa/research-dashboard--render-timer nil)
                               (nsa/research-dashboard--render)))))))))

(defun nsa/research-dashboard--done-p ()
  (and (zerop nsa/research-dashboard--searches-left)
       (zerop nsa/research-dashboard--active)
       (null nsa/research-dashboard--queue)))
(defun nsa/research-dashboard--finish ()
  (when (and nsa/research-dashboard--scanning (nsa/research-dashboard--done-p))
    (setq nsa/research-dashboard--scanning nil)
    (nsa/research-dashboard--render)))

(defun nsa/research-dashboard--job-done (generation &optional item error-text)
  (when (= generation nsa/research-dashboard--generation)
    (when item (push item nsa/research-dashboard--items))
    (when error-text (push error-text nsa/research-dashboard--errors))
    (cl-decf nsa/research-dashboard--active)
    (nsa/research-dashboard--schedule-render)
    (nsa/research-dashboard--pump generation)
    (nsa/research-dashboard--finish)))

(defun nsa/research-dashboard--blob-job (generation repo branch path blob)
  (let ((dashboard (current-buffer)) (id (concat repo ":" path)))
    (nsa/research-dashboard--gh-json
     dashboard
     (lambda (ok result)
       (when (and (buffer-live-p dashboard)
                  (= generation (buffer-local-value 'nsa/research-dashboard--generation dashboard)))
         (with-current-buffer dashboard
           (if (not ok)
               (nsa/research-dashboard--job-done generation nil (format "%s: %s" id result))
             (condition-case error-data
                 (let ((encoding (nsa/research-dashboard--get "encoding" result))
                       (content (nsa/research-dashboard--get "content" result)))
                   (unless (and (string= encoding "base64") (stringp content))
                     (error "unsupported blob encoding"))
                   (nsa/research-dashboard--job-done
                    generation
                    (nsa/research-dashboard--item
                     repo branch path blob
                     (decode-coding-string (base64-decode-string content) 'utf-8))))
               (error
                (nsa/research-dashboard--job-done
                 generation nil (format "%s: %s" id (error-message-string error-data)))))))))
     (list "api" (format "repos/%s/git/blobs/%s" repo blob)))))

(defun nsa/research-dashboard--start-job (generation hit)
  (pcase-let ((`(,repo ,branch ,path ,blob) hit))
    (if (not (string-empty-p branch))
        (nsa/research-dashboard--blob-job generation repo branch path blob)
      (let ((dashboard (current-buffer)))
        (nsa/research-dashboard--gh-json
         dashboard
         (lambda (ok result)
           (when (and (buffer-live-p dashboard)
                      (= generation (buffer-local-value 'nsa/research-dashboard--generation dashboard)))
             (with-current-buffer dashboard
               (if ok
                   (let ((resolved (nsa/research-dashboard--get "default_branch" result)))
                     (if resolved
                         (nsa/research-dashboard--blob-job generation repo resolved path blob)
                       (nsa/research-dashboard--job-done
                        generation nil (format "%s:%s: no default branch" repo path))))
                 (nsa/research-dashboard--job-done
                  generation nil (format "%s:%s: %s" repo path result))))))
         (list "api" (format "repos/%s" repo)))))))

(defun nsa/research-dashboard--pump (generation)
  (while (and (= generation nsa/research-dashboard--generation)
              (< nsa/research-dashboard--active (max 1 nsa/research-dashboard-max-workers))
              nsa/research-dashboard--queue)
    (cl-incf nsa/research-dashboard--active)
    (nsa/research-dashboard--start-job generation (pop nsa/research-dashboard--queue)))
  (nsa/research-dashboard--schedule-render))

(defun nsa/research-dashboard--search-owner (generation owner login)
  (let* ((dashboard (current-buffer))
         (scope (if (string= owner login) (concat "user:" owner) (concat "org:" owner)))
         (query (format "research in:path extension:org %s" scope)))
    (nsa/research-dashboard--gh
     dashboard
     (lambda (ok output)
       (when (and (buffer-live-p dashboard)
                  (= generation (buffer-local-value 'nsa/research-dashboard--generation dashboard)))
         (with-current-buffer dashboard
           (if (not ok)
               (push (format "%s: %s" owner (string-trim output)) nsa/research-dashboard--errors)
             (dolist (line (split-string output "\n" t))
               (pcase-let ((`(,repo ,branch ,path ,blob) (split-string line "\t" nil)))
                 (let ((id (and repo path (concat repo ":" path))))
                   (when (and id (nsa/research-dashboard--candidate-path-p path)
                              (not (gethash id nsa/research-dashboard--seen)))
                     (puthash id t nsa/research-dashboard--seen)
                     (setq nsa/research-dashboard--queue
                           (nconc nsa/research-dashboard--queue
                                  (list (list repo (or branch "") path blob)))))))))
           (cl-decf nsa/research-dashboard--searches-left)
           (nsa/research-dashboard--pump generation)
           (nsa/research-dashboard--finish))))
     (list "api" "--method" "GET" "--paginate" "search/code"
           "-f" (concat "q=" query) "-f" "per_page=100"
           "--jq" ".items[] | [.repository.full_name, (.repository.default_branch // \"\"), .path, .sha] | @tsv"))))

(defun nsa/research-dashboard--start-searches (generation login owners)
  (setq nsa/research-dashboard--searches-left (length owners))
  (dolist (owner owners) (nsa/research-dashboard--search-owner generation owner login))
  (nsa/research-dashboard--finish))

(defun nsa/research-dashboard--discover (generation)
  (let ((dashboard (current-buffer)))
    (nsa/research-dashboard--gh
     dashboard
     (lambda (ok output)
       (when (and (buffer-live-p dashboard)
                  (= generation (buffer-local-value 'nsa/research-dashboard--generation dashboard)))
         (with-current-buffer dashboard
           (if (not ok)
               (progn
                 (push (string-trim output) nsa/research-dashboard--errors)
                 (setq nsa/research-dashboard--scanning nil)
                 (nsa/research-dashboard--render))
             (let ((login (string-trim output)))
               (setq nsa/research-dashboard--login login)
               (if nsa/research-dashboard-owners
                   (nsa/research-dashboard--start-searches
                    generation login (delete-dups (copy-sequence nsa/research-dashboard-owners)))
                 (nsa/research-dashboard--gh
                  dashboard
                  (lambda (org-ok org-output)
                    (when (buffer-live-p dashboard)
                      (with-current-buffer dashboard
                        (unless org-ok
                          (push (string-trim org-output) nsa/research-dashboard--errors))
                        (nsa/research-dashboard--start-searches
                         generation login
                         (delete-dups
                          (cons login (if org-ok (split-string org-output "\n" t) nil)))))))
                  (list "api" "--paginate" "user/orgs" "--jq" ".[].login"))))))))
     (list "api" "user" "--jq" ".login"))))

(defun nsa/research-dashboard-refresh ()
  "Refresh asynchronously; no GitHub operation waits on the Emacs UI thread."
  (interactive)
  (cl-incf nsa/research-dashboard--generation)
  (nsa/research-dashboard--cancel)
  (let ((generation nsa/research-dashboard--generation))
    (setq nsa/research-dashboard--items nil
          nsa/research-dashboard--errors nil
          nsa/research-dashboard--queue nil
          nsa/research-dashboard--active 0
          nsa/research-dashboard--searches-left 0
          nsa/research-dashboard--scanning t
          nsa/research-dashboard--seen (make-hash-table :test #'equal))
    (nsa/research-dashboard--render)
    (nsa/research-dashboard--discover generation)))

(defun nsa/research-dashboard-search (text)
  "Search all visible research fields for TEXT."
  (interactive
   (list (read-string "Research search (empty clears): " nsa/research-dashboard--search-text)))
  (setq nsa/research-dashboard--search-text (string-trim text))
  (nsa/research-dashboard--render))

(defun nsa/research-dashboard--field-values (field)
  (sort
   (delete-dups
    (mapcar (lambda (item) (nsa/research-dashboard--filter-value item field))
            nsa/research-dashboard--items))
   #'string-lessp))

(defun nsa/research-dashboard-filter (field value)
  "Filter the dashboard by FIELD and exact VALUE."
  (interactive
   (let* ((field-name
           (completing-read "Filter field: "
                            '("repository" "project" "lifecycle" "approval") nil t))
          (field (intern field-name))
          (value
           (completing-read (format "%s: " field-name)
                            (nsa/research-dashboard--field-values field) nil t)))
     (list field value)))
  (setq nsa/research-dashboard--field-filters
        (cons (cons field value)
              (assq-delete-all field nsa/research-dashboard--field-filters)))
  (nsa/research-dashboard--render))

(defun nsa/research-dashboard-clear-filters ()
  "Clear text search and all field filters."
  (interactive)
  (setq nsa/research-dashboard--search-text ""
        nsa/research-dashboard--field-filters nil)
  (nsa/research-dashboard--render))

(defun nsa/research-dashboard-toggle-legacy ()
  "Toggle visibility of review-ready unmigrated research."
  (interactive)
  (setq nsa/research-dashboard--show-legacy (not nsa/research-dashboard--show-legacy))
  (nsa/research-dashboard--render)
  (message "Unmigrated research %s"
           (if nsa/research-dashboard--show-legacy "shown" "hidden")))

(defun nsa/research-dashboard-help ()
  "Show dashboard key bindings and filtering semantics."
  (interactive)
  (with-help-window "*Research Dashboard Help*"
    (princ "Research Dashboard\n\n")
    (princ "RET  view research\n")
    (princ "a    approve\n")
    (princ "r    reject\n")
    (princ "g    async refresh\n")
    (princ "/ s  local search\n")
    (princ "f    exact field filter\n")
    (princ "c    clear search/filters\n")
    (princ "L    show/hide unmigrated review-ready research\n")
    (princ "e    show scan/decision errors\n")
    (princ "?    this help\n\n")
    (princ "By default only canonical PENDING research is visible.\n")
    (princ "UNMIGRATED means REVIEW/RESEARCHED/VERIFIED without the canonical approval block.\n")))

(defun nsa/research-dashboard--at-point ()
  (let ((id (tabulated-list-get-id)))
    (or (seq-find
         (lambda (item)
           (string= id (concat (nsa/research-item-repo item) ":"
                               (nsa/research-item-path item))))
         nsa/research-dashboard--items)
        (user-error "No research item on this row"))))

(defun nsa/research-dashboard-view ()
  (interactive)
  (let* ((item (nsa/research-dashboard--at-point))
         (buffer (get-buffer-create (format "*Research: %s*" (nsa/research-item-title item)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer) (insert (nsa/research-item-content item))
        (org-mode) (read-only-mode 1)
        (setq-local header-line-format
                    (format "%s — %s" (nsa/research-item-repo item)
                            (nsa/research-item-path item)))))
    (pop-to-buffer buffer)))

(defun nsa/research-dashboard-errors ()
  (interactive)
  (with-output-to-temp-buffer "*Research Dashboard Errors*"
    (princ (if nsa/research-dashboard--errors
               (string-join (reverse nsa/research-dashboard--errors) "\n\n")
             "No errors."))))

(defun nsa/research-dashboard--encode-path (path)
  (mapconcat #'url-hexify-string (split-string path "/" t) "/"))
(defun nsa/research-dashboard--field-regexp (field)
  (format "^#\\+%s:[ \\t]*.*$" (regexp-quote field)))
(defun nsa/research-dashboard--has-field-p (content)
  (seq-some (lambda (field)
              (string-match-p (nsa/research-dashboard--field-regexp field) content))
            nsa/research-dashboard--fields))
(defun nsa/research-dashboard--canonical-p (content)
  (string-match-p
   (concat "^#\\+status:.*\n#\\+approval_schema: "
           (regexp-quote nsa/research-dashboard--schema)
           "\n#\\+approval_state: PENDING\n#\\+approval_actor: .*\n"
           "#\\+approval_evidence: .*\n#\\+approval_base_commit: .*\n"
           "#\\+approval_base_blob: .*\n#\\+approval_decided_at: .*$") content))

(defun nsa/research-dashboard--replace-field (content field value)
  (with-temp-buffer
    (insert content) (goto-char (point-min))
    (unless (re-search-forward (nsa/research-dashboard--field-regexp field) nil t)
      (user-error "Missing #+%s" field))
    (replace-match (format "#+%s: %s" field value) t t)
    (when (re-search-forward (nsa/research-dashboard--field-regexp field) nil t)
      (user-error "Duplicate #+%s" field))
    (buffer-string)))

(defun nsa/research-dashboard--decision-content (content state actor evidence commit blob timestamp)
  (let ((values `(("approval_schema" . ,nsa/research-dashboard--schema)
                  ("approval_state" . ,state) ("approval_actor" . ,actor)
                  ("approval_evidence" . ,evidence) ("approval_base_commit" . ,commit)
                  ("approval_base_blob" . ,blob) ("approval_decided_at" . ,timestamp))))
    (cond
     ((nsa/research-dashboard--canonical-p content)
      (dolist (pair values content)
        (setq content (nsa/research-dashboard--replace-field content (car pair) (cdr pair)))))
     ((nsa/research-dashboard--has-field-p content)
      (user-error "Partial/noncanonical approval metadata exists"))
     (t
      (with-temp-buffer
        (insert content) (goto-char (point-min))
        (unless (re-search-forward "^#\\+status:.*$" nil t)
          (user-error "Legacy research has no #+status; cannot safely record approval"))
        (end-of-line)
        (insert "\n" (mapconcat
                       (lambda (field)
                         (format "#+%s: %s" field
                                 (alist-get field values nil nil #'string=)))
                       nsa/research-dashboard--fields "\n"))
        (buffer-string))))))

(defun nsa/research-dashboard--branch-head (decision callback)
  (let* ((item (nsa/research-decision-item decision))
         (dashboard (nsa/research-decision-dashboard decision)))
    (nsa/research-dashboard--gh-json
     dashboard
     (lambda (ok result)
       (if ok
           (funcall callback t
                    (nsa/research-dashboard--get "sha"
                                                 (nsa/research-dashboard--get "commit" result)))
         (funcall callback nil result)))
     (list "api" (format "repos/%s/branches/%s"
                         (nsa/research-item-repo item)
                         (url-hexify-string (nsa/research-item-branch item)))))))

(defun nsa/research-dashboard--remote-file (decision commit callback)
  (let* ((item (nsa/research-decision-item decision))
         (dashboard (nsa/research-decision-dashboard decision)))
    (nsa/research-dashboard--gh-json
     dashboard
     (lambda (ok result)
       (if (not ok) (funcall callback nil result)
         (let ((content (nsa/research-dashboard--get "content" result))
               (encoding (nsa/research-dashboard--get "encoding" result)))
           (if (not (and (string= encoding "base64") (stringp content)))
               (funcall callback nil "unsupported contents encoding")
             (funcall callback t
                      (cons (nsa/research-dashboard--get "sha" result)
                            (decode-coding-string (base64-decode-string content) 'utf-8)))))))
     (list "api" "--method" "GET"
           (format "repos/%s/contents/%s" (nsa/research-item-repo item)
                   (nsa/research-dashboard--encode-path (nsa/research-item-path item)))
           "-f" (concat "ref=" commit)))))

(defun nsa/research-dashboard--fail (decision text)
  (let ((dashboard (nsa/research-decision-dashboard decision))
        (item (nsa/research-decision-item decision)))
    (when (buffer-live-p dashboard)
      (with-current-buffer dashboard
        (setf (nsa/research-item-busy item) nil)
        (push text nsa/research-dashboard--errors)
        (nsa/research-dashboard--render)
        (message "Research decision failed: %s" text)))))

(defun nsa/research-dashboard--approval-branch-name (decision)
  (let* ((item (nsa/research-decision-item decision))
         (stem (replace-regexp-in-string
                "[^A-Za-z0-9._-]+" "-" (file-name-base (nsa/research-item-path item)))))
    (format "research-approval/%s-%s"
            (downcase stem) (format-time-string "%Y%m%d%H%M%S"))))

(defun nsa/research-dashboard--approval-commit-message (decision)
  (let ((item (nsa/research-decision-item decision)))
    (format "docs(research): %s %s [skip ci]"
            (downcase (nsa/research-decision-state decision))
            (file-name-base (nsa/research-item-path item)))))

(defun nsa/research-dashboard--create-approval-branch (decision)
  (let* ((dashboard (nsa/research-decision-dashboard decision))
         (item (nsa/research-decision-item decision))
         (branch (nsa/research-dashboard--approval-branch-name decision))
         (payload (json-encode
                   `((ref . ,(concat "refs/heads/" branch))
                     (sha . ,(nsa/research-decision-commit decision))))))
    (setf (nsa/research-decision-approval-branch decision) branch)
    (nsa/research-dashboard--gh-json
     dashboard
     (lambda (ok result)
       (if ok
           (nsa/research-dashboard--write-approval-branch decision)
         (nsa/research-dashboard--fail
          decision (format "Could not create approval branch: %s" result))))
     (list "api" "--method" "POST"
           (format "repos/%s/git/refs" (nsa/research-item-repo item))
           "--input" "-") payload)))

(defun nsa/research-dashboard--write-approval-branch (decision)
  (let* ((dashboard (nsa/research-decision-dashboard decision))
         (item (nsa/research-decision-item decision))
         (payload (json-encode
                   `((message . ,(nsa/research-dashboard--approval-commit-message decision))
                     (content . ,(base64-encode-string
                                  (nsa/research-decision-updated decision) t))
                     (sha . ,(nsa/research-decision-blob decision))
                     (branch . ,(nsa/research-decision-approval-branch decision))))))
    (nsa/research-dashboard--gh-json
     dashboard
     (lambda (ok result)
       (if (not ok)
           (nsa/research-dashboard--fail
            decision (format "Could not write approval branch: %s" result))
         (let* ((commit (nsa/research-dashboard--get "commit" result))
                (sha (nsa/research-dashboard--get "sha" commit)))
           (setf (nsa/research-decision-approval-commit decision) sha)
           (nsa/research-dashboard--create-approval-pr decision))))
     (list "api" "--method" "PUT"
           (format "repos/%s/contents/%s" (nsa/research-item-repo item)
                   (nsa/research-dashboard--encode-path (nsa/research-item-path item)))
           "--input" "-") payload)))

(defun nsa/research-dashboard--create-approval-pr (decision)
  (let* ((dashboard (nsa/research-decision-dashboard decision))
         (item (nsa/research-decision-item decision))
         (state (nsa/research-decision-state decision))
         (payload
          (json-encode
           `((title . ,(format "docs(research): %s %s"
                              (downcase state)
                              (file-name-base (nsa/research-item-path item))))
             (head . ,(nsa/research-decision-approval-branch decision))
             (base . ,(nsa/research-item-branch item))
             (body . ,(format
                       "%s\n\nHuman research decision recorded by the Emacs research dashboard.\n\nDecision: `%s`\nActor: `%s`\nEvidence: `%s`\n\nApproval-only metadata change. CI intentionally skipped."
                       nsa/research-dashboard--approval-pr-marker
                       state
                       (nsa/research-decision-actor decision)
                       (nsa/research-decision-evidence decision)))))))
    (nsa/research-dashboard--gh-json
     dashboard
     (lambda (ok result)
       (if (not ok)
           (nsa/research-dashboard--fail
            decision (format "Could not open approval PR: %s" result))
         (setf (nsa/research-decision-pr-number decision)
               (nsa/research-dashboard--get "number" result)
               (nsa/research-decision-pr-url decision)
               (nsa/research-dashboard--get "html_url" result))
         (nsa/research-dashboard--merge-approval-pr decision)))
     (list "api" "--method" "POST"
           (format "repos/%s/pulls" (nsa/research-item-repo item))
           "--input" "-") payload)))

(defun nsa/research-dashboard--merge-approval-pr (decision)
  (let* ((dashboard (nsa/research-decision-dashboard decision))
         (item (nsa/research-decision-item decision))
         (number (nsa/research-decision-pr-number decision))
         (payload (json-encode
                   `((merge_method . "rebase")
                     (sha . ,(nsa/research-decision-approval-commit decision))))))
    (nsa/research-dashboard--gh-json
     dashboard
     (lambda (ok result)
       (if (not ok)
           (nsa/research-dashboard--fail
            decision
            (format "Approval PR %s could not merge immediately: %s%s"
                    number result
                    (if (nsa/research-decision-pr-url decision)
                        (format "\n%s" (nsa/research-decision-pr-url decision)) "")))
         (if (nsa/research-dashboard--get "merged" result)
             (with-current-buffer dashboard
               (message "%s %s:%s — approval PR #%s merged"
                        (nsa/research-decision-state decision)
                        (nsa/research-item-repo item)
                        (nsa/research-item-path item) number)
               (nsa/research-dashboard-refresh))
           (nsa/research-dashboard--fail
            decision
            (format "Approval PR #%s was not merged: %s"
                    number (or (nsa/research-dashboard--get "message" result) result))))))
     (list "api" "--method" "PUT"
           (format "repos/%s/pulls/%s/merge" (nsa/research-item-repo item) number)
           "--input" "-") payload)))

(defun nsa/research-dashboard--recheck-file (decision ok remote)
  (if (not ok) (nsa/research-dashboard--fail decision remote)
    (if (not (and (string= (nsa/research-decision-blob decision) (car remote))
                  (string= (nsa/research-decision-content decision) (cdr remote))))
        (nsa/research-dashboard--fail
         decision "Research changed during review; refresh first")
      (nsa/research-dashboard--create-approval-branch decision))))

(defun nsa/research-dashboard--recheck-head (decision ok head)
  (if (not ok) (nsa/research-dashboard--fail decision head)
    (if (not (string= head (nsa/research-decision-commit decision)))
        (nsa/research-dashboard--fail
         decision "Branch changed during review; refresh first")
      (nsa/research-dashboard--remote-file
       decision head
       (lambda (file-ok remote)
         (nsa/research-dashboard--recheck-file decision file-ok remote))))))

(defun nsa/research-dashboard--confirm (decision)
  (let* ((item (nsa/research-decision-item decision))
         (buffer (get-buffer-create "*Research Approval Preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\n"
                        (nsa/research-item-repo item)
                        (nsa/research-item-path item)))
        (dolist (field nsa/research-dashboard--fields)
          (insert (format "#+%s: %s\n" field
                          (nsa/research-dashboard--keyword
                           (nsa/research-decision-updated decision) field))))
        (insert "\nDelivery: approval-only PR, [skip ci], immediate rebase merge\n")
        (special-mode)))
    (display-buffer buffer)
    (if (not (yes-or-no-p
              (format "%s %s/%s? "
                      (nsa/research-decision-state decision)
                      (nsa/research-item-repo item)
                      (file-name-nondirectory (nsa/research-item-path item)))))
        (let ((dashboard (nsa/research-decision-dashboard decision)))
          (with-current-buffer dashboard
            (setf (nsa/research-item-busy item) nil)
            (nsa/research-dashboard--render)))
      (nsa/research-dashboard--branch-head
       decision
       (lambda (ok head)
         (nsa/research-dashboard--recheck-head decision ok head))))))

(defun nsa/research-dashboard--got-file (decision ok remote)
  (if (not ok) (nsa/research-dashboard--fail decision remote)
    (let* ((item (nsa/research-decision-item decision))
           (blob (car remote)) (content (cdr remote)))
      (if (not (and (string= blob (nsa/research-item-blob item))
                    (string= content (nsa/research-item-content item))))
          (nsa/research-dashboard--fail
           decision "Research changed since refresh; refresh first")
        (condition-case error-data
            (progn
              (setf (nsa/research-decision-blob decision) blob
                    (nsa/research-decision-content decision) content
                    (nsa/research-decision-updated decision)
                    (nsa/research-dashboard--decision-content
                     content (nsa/research-decision-state decision)
                     (nsa/research-decision-actor decision)
                     (nsa/research-decision-evidence decision)
                     (nsa/research-decision-commit decision) blob
                     (format-time-string "%Y-%m-%dT%H:%M:%S%:z")))
              (nsa/research-dashboard--confirm decision))
          (error
           (nsa/research-dashboard--fail
            decision (error-message-string error-data))))))))

(defun nsa/research-dashboard--got-head (decision ok commit)
  (if (not ok) (nsa/research-dashboard--fail decision commit)
    (setf (nsa/research-decision-commit decision) commit)
    (nsa/research-dashboard--remote-file
     decision commit
     (lambda (file-ok remote)
       (nsa/research-dashboard--got-file decision file-ok remote)))))

(defun nsa/research-dashboard--decide (state)
  (let* ((dashboard (current-buffer)) (item (nsa/research-dashboard--at-point)))
    (when (nsa/research-item-busy item) (user-error "Decision already running"))
    (let ((actor (read-string "Human decision-maker: " (or nsa/research-dashboard--login "")))
          (evidence (read-string "Durable approval evidence: "
                                 "human:emacs-research-dashboard")))
      (when (or (string-empty-p (string-trim actor))
                (string-empty-p (string-trim evidence)))
        (user-error "Actor and evidence must be nonempty"))
      (setf (nsa/research-item-busy item) t)
      (nsa/research-dashboard--render)
      (let ((decision (nsa/research-decision-create
                       :dashboard dashboard :item item :state state
                       :actor actor :evidence evidence)))
        (nsa/research-dashboard--branch-head
         decision (lambda (ok commit)
                    (nsa/research-dashboard--got-head decision ok commit)))))))

(defun nsa/research-dashboard-approve () (interactive)
  (nsa/research-dashboard--decide "APPROVED"))
(defun nsa/research-dashboard-reject () (interactive)
  (nsa/research-dashboard--decide "REJECTED"))

(define-derived-mode nsa/research-dashboard-mode tabulated-list-mode "Research-Review"
  "Major mode for cross-repository human research approval."
  (setq-local truncate-lines t)
  (setq-local tabulated-list-padding 2)
  (setq-local font-lock-defaults '(nsa/research-dashboard-font-lock-keywords))
  (setq tabulated-list-format
        [("Approval" 12 t) ("Repository" 30 t) ("Project" 20 t)
         ("Lifecycle" 22 t) ("Title" 56 t) ("Path" 64 t)]
        tabulated-list-sort-key '("Repository" . nil)
        nsa/research-dashboard--show-legacy
        nsa/research-dashboard-show-legacy-default)
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (add-hook 'kill-buffer-hook #'nsa/research-dashboard--cancel nil t))

;;;###autoload
(defun nsa/research-dashboard ()
  "Show open research across GitHub without blocking Emacs."
  (interactive)
  (let ((buffer (get-buffer-create "*Research Dashboard*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'nsa/research-dashboard-mode)
        (nsa/research-dashboard-mode))
      (nsa/research-dashboard-refresh))
    (pop-to-buffer buffer)))

(provide 'research-dashboard)
;;; research-dashboard.el ends here
