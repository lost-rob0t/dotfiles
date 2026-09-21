;;; starintel-admin.el --- Org-native StarIntel operator UI -*- lexical-binding: t; -*-

(require 'button)
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defgroup starintel-admin nil
  "Operator UI over the portable starintel-admin CLI."
  :group 'tools)

(defcustom starintel-admin-command "starintel-admin"
  "Executable implementing the StarIntel administrator JSON interface."
  :type 'string)

(defcustom starintel-admin-default-document-limit 100
  "Default tenant document limit."
  :type 'integer)

(defface starintel-admin-title-face
  '((t (:inherit org-level-1 :height 1.45 :weight bold)))
  "Dashboard title.")
(defface starintel-admin-accent-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Dashboard accent.")
(defface starintel-admin-success-face
  '((t (:inherit success :weight bold)))
  "Successful state.")
(defface starintel-admin-danger-face
  '((t (:inherit error :weight bold)))
  "Dangerous or revoked state.")
(defface starintel-admin-muted-face
  '((t (:inherit shadow)))
  "Muted metadata.")
(defface starintel-admin-chip-face
  '((t (:inherit highlight :box (:line-width (1 . 1) :style released-button))))
  "Action chips.")

(defvar-local starintel-admin--generation 0)
(defvar-local starintel-admin--process nil)
(defvar-local starintel-admin--last-dashboard nil)
(defvar-local starintel-admin--last-error nil)

(defun starintel-admin--json (text)
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun starintel-admin--get (key object)
  (alist-get key object nil nil #'string=))

(defun starintel-admin--cell (value)
  (replace-regexp-in-string
   "|" "¦"
   (replace-regexp-in-string
    "[\n\r]+" " "
    (format "%s" (or value "")))))

(defun starintel-admin--process-live-p ()
  (and starintel-admin--process
       (process-live-p starintel-admin--process)))

(defun starintel-admin--cancel ()
  (when (starintel-admin--process-live-p)
    (delete-process starintel-admin--process))
  (setq starintel-admin--process nil))

(defun starintel-admin--call (callback &rest args)
  "Run the portable admin CLI asynchronously.
CALLBACK receives (OK OUTPUT)."
  (unless (executable-find starintel-admin-command)
    (user-error "Missing %s on PATH" starintel-admin-command))
  (let* ((origin (current-buffer))
         (buffer (generate-new-buffer " *starintel-admin-cli*"))
         (command (cons starintel-admin-command args))
         process)
    (setq process
          (make-process
           :name "starintel-admin"
           :buffer buffer
           :command command
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((ok (and (eq (process-status proc) 'exit)
                              (zerop (process-exit-status proc))))
                     (output (if (buffer-live-p buffer)
                                 (with-current-buffer buffer (buffer-string))
                               "")))
                 (when (buffer-live-p buffer)
                   (kill-buffer buffer))
                 (when (buffer-live-p origin)
                   (with-current-buffer origin
                     (when (eq starintel-admin--process proc)
                       (setq starintel-admin--process nil))
                     (funcall callback ok output))))))))
    (setq starintel-admin--process process)
    process))

(defun starintel-admin--call-json (callback &rest args)
  (apply
   #'starintel-admin--call
   (lambda (ok output)
     (if (not ok)
         (funcall callback nil (string-trim output))
       (condition-case err
           (funcall callback t (starintel-admin--json output))
         (error
          (funcall callback nil (error-message-string err))))))
   args))

(defun starintel-admin--button (label function &rest args)
  (insert-button
   label
   'face 'starintel-admin-chip-face
   'follow-link t
   'help-echo label
   'action (lambda (_button) (apply function args))))

(defun starintel-admin--insert-actions ()
  (insert "** Actions\n\n")
  (starintel-admin--button " Users " #'starintel-admin-users)
  (insert " ")
  (starintel-admin--button " Documents by tenant " #'starintel-admin-documents-by-tenant)
  (insert " ")
  (starintel-admin--button " Data size " #'starintel-admin-data-size)
  (insert " ")
  (starintel-admin--button " Create user " #'starintel-admin-create-user)
  (insert " ")
  (starintel-admin--button " Set plan " #'starintel-admin-set-plan)
  (insert " ")
  (starintel-admin--button " Dataset policy " #'starintel-admin-dataset-policy)
  (insert " ")
  (starintel-admin--button " Graph image " #'starintel-admin-graph-image)
  (insert "\n\n"))

(defun starintel-admin--status-face (status)
  (if (member (downcase (or status "")) '("active" "ok" "healthy"))
      'starintel-admin-success-face
    'starintel-admin-danger-face))

(defun starintel-admin--insert-summary (payload)
  (let* ((counts (starintel-admin--get "counts" payload))
         (health (starintel-admin--get "health" payload))
         (status (or (starintel-admin--get "status" health) "unknown")))
    (insert "* ")
    (insert (propertize "StarIntel Admin" 'face 'starintel-admin-title-face))
    (insert "\n\n")
    (insert (propertize
             "Org-rendered operator surface · portable CLI backend · no secrets in buffer"
             'face 'starintel-admin-muted-face))
    (insert "\n\n")
    (insert "** Control plane\n\n")
    (insert "| Metric | Value |\n|---+---|\n")
    (insert (format "| Server | %s |\n"
                    (propertize status 'face (starintel-admin--status-face status))))
    (insert (format "| Users | %s |\n" (or (starintel-admin--get "users" counts) 0)))
    (insert (format "| Active users | %s |\n" (or (starintel-admin--get "active_users" counts) 0)))
    (insert (format "| Active credentials | %s |\n"
                    (or (starintel-admin--get "active_credentials" counts) 0)))
    (insert (format "| Dataset policies | %s |\n"
                    (or (starintel-admin--get "datasets" counts) 0)))
    (org-table-align)))

(defun starintel-admin--tenant-scopes (user)
  (seq-filter
   (lambda (scope) (string-prefix-p "tenant:" scope))
   (or (starintel-admin--get "scopes" user) nil)))

(defun starintel-admin--insert-user-preview (payload)
  (let ((users (starintel-admin--get "users" payload)))
    (insert "\n** Users\n\n")
    (insert "| User | Status | Plans |\n|---+---+---|\n")
    (dolist (user (seq-take users 12))
      (insert
       (format "| %s | %s | %s |\n"
               (starintel-admin--cell (starintel-admin--get "username" user))
               (starintel-admin--cell (starintel-admin--get "status" user))
               (starintel-admin--cell
                (string-join
                 (mapcar (lambda (scope) (substring scope (length "tenant:")))
                         (starintel-admin--tenant-scopes user))
                 ", ")))))
    (org-table-align)
    (when (> (length users) 12)
      (insert (format "\n%s\n"
                      (propertize
                       (format "Showing 12/%d here — open Users for the full table."
                               (length users))
                       'face 'starintel-admin-muted-face))))))

(defun starintel-admin--insert-dataset-preview (payload)
  (let ((policies (starintel-admin--get "dataset_policies" payload)))
    (insert "\n** Dataset policy\n\n")
    (if (not policies)
        (insert (propertize "No explicit dataset policies. Unlisted datasets are private by policy.\n"
                            'face 'starintel-admin-muted-face))
      (insert "| Dataset | Mode | Plan / tenant |\n|---+---+---|\n")
      (dolist (entry policies)
        (let* ((dataset (car entry))
               (policy (cdr entry)))
          (insert
           (format "| %s | %s | %s |\n"
                   (starintel-admin--cell dataset)
                   (starintel-admin--cell (starintel-admin--get "mode" policy))
                   (starintel-admin--cell (starintel-admin--get "tenant_id" policy))))))
      (org-table-align))))

(defun starintel-admin--render (payload)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (starintel-admin--insert-summary payload)
    (starintel-admin--insert-actions)
    (starintel-admin--insert-user-preview payload)
    (starintel-admin--insert-dataset-preview payload)
    (insert "\n** Keys\n\n")
    (insert (propertize
             "g refresh · u users · d tenant docs · s data size · p set plan · P revoke plan · r revoke user · k revoke key · v dataset policy · i graph\n"
             'face 'starintel-admin-muted-face))
    (goto-char (point-min))
    (org-cycle-hide-drawers 'all)
    (when (fboundp 'org-modern-mode)
      (org-modern-mode 1))
    (org-display-inline-images)))

(defun starintel-admin-refresh ()
  "Refresh the Org-native StarIntel administrator dashboard."
  (interactive)
  (starintel-admin--cancel)
  (cl-incf starintel-admin--generation)
  (let ((generation starintel-admin--generation)
        (buffer (current-buffer)))
    (setq header-line-format " StarIntel Admin · loading… ")
    (starintel-admin--call-json
     (lambda (ok result)
       (when (and (buffer-live-p buffer)
                  (= generation
                     (buffer-local-value 'starintel-admin--generation buffer)))
         (with-current-buffer buffer
           (if ok
               (progn
                 (setq starintel-admin--last-dashboard result
                       starintel-admin--last-error nil
                       header-line-format " StarIntel Admin · live ")
                 (starintel-admin--render result))
             (setq starintel-admin--last-error result
                   header-line-format " StarIntel Admin · ERROR ")
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "* StarIntel Admin\n\n")
               (insert (propertize result 'face 'starintel-admin-danger-face)))))))
     "dashboard" "json")))

(defun starintel-admin--mode-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "g") #'starintel-admin-refresh)
    (define-key map (kbd "u") #'starintel-admin-users)
    (define-key map (kbd "d") #'starintel-admin-documents-by-tenant)
    (define-key map (kbd "s") #'starintel-admin-data-size)
    (define-key map (kbd "c") #'starintel-admin-create-user)
    (define-key map (kbd "p") #'starintel-admin-set-plan)
    (define-key map (kbd "P") #'starintel-admin-revoke-plan)
    (define-key map (kbd "r") #'starintel-admin-revoke-user)
    (define-key map (kbd "k") #'starintel-admin-revoke-key)
    (define-key map (kbd "v") #'starintel-admin-dataset-policy)
    (define-key map (kbd "i") #'starintel-admin-graph-image)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar starintel-admin-mode-map (starintel-admin--mode-map))

(define-derived-mode starintel-admin-mode org-mode "StarIntel-Admin"
  "Modern Org-native StarIntel administration dashboard."
  (setq-local buffer-read-only t
              truncate-lines nil
              org-hide-emphasis-markers t
              org-startup-folded 'content)
  (visual-line-mode 1)
  (when (fboundp 'org-indent-mode)
    (org-indent-mode 1))
  (when (fboundp 'org-modern-mode)
    (org-modern-mode 1)))

;;;###autoload
(defun starintel-admin ()
  "Open the StarIntel administrator dashboard.
This is intentionally separate from the normal Emacs startup dashboard."
  (interactive)
  (let ((buffer (get-buffer-create "*StarIntel Admin*")))
    (pop-to-buffer buffer)
    (unless (derived-mode-p 'starintel-admin-mode)
      (starintel-admin-mode))
    (starintel-admin-refresh)))

(defun starintel-admin--user-plan (user)
  (string-join
   (mapcar (lambda (scope) (substring scope (length "tenant:")))
           (starintel-admin--tenant-scopes user))
   ","))

(defun starintel-admin-users-refresh ()
  (interactive)
  (setq header-line-format " StarIntel Users · loading… ")
  (starintel-admin--call-json
   (lambda (ok result)
     (if (not ok)
         (setq header-line-format (concat " StarIntel Users · ERROR · " result))
       (setq tabulated-list-entries
             (mapcar
              (lambda (user)
                (let ((name (starintel-admin--get "username" user)))
                  (list name
                        (vector
                         name
                         (or (starintel-admin--get "status" user) "")
                         (starintel-admin--user-plan user)
                         (string-join
                          (seq-remove
                           (lambda (scope)
                             (or (string-prefix-p "tenant:" scope)
                                 (string-prefix-p "dataset:" scope)))
                           (or (starintel-admin--get "scopes" user) nil))
                          ",")))))
              result)
             header-line-format
             (format " StarIntel Users · %d users · RET inspect · p plan · r revoke · g refresh "
                     (length result)))
       (tabulated-list-print t)))
   "user" "list"))

(defun starintel-admin-users--mode-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'starintel-admin-users-refresh)
    (define-key map (kbd "RET") #'starintel-admin-user-at-point)
    (define-key map (kbd "p") #'starintel-admin-set-plan-at-point)
    (define-key map (kbd "P") #'starintel-admin-revoke-plan-at-point)
    (define-key map (kbd "r") #'starintel-admin-revoke-user-at-point)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar starintel-admin-users-mode-map (starintel-admin-users--mode-map))

(define-derived-mode starintel-admin-users-mode tabulated-list-mode "StarIntel-Users"
  "Dense table of all StarIntel users."
  (setq tabulated-list-format
        [("User" 24 t)
         ("Status" 12 t)
         ("Plan / tenant" 28 t)
         ("Capabilities" 0 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun starintel-admin-users ()
  "Open the dense all-users table."
  (interactive)
  (let ((buffer (get-buffer-create "*StarIntel Users*")))
    (pop-to-buffer buffer)
    (starintel-admin-users-mode)
    (starintel-admin-users-refresh)))

(defun starintel-admin--current-user ()
  (or (tabulated-list-get-id)
      (user-error "No user on this row")))

(defun starintel-admin-user-at-point ()
  (interactive)
  (let ((username (starintel-admin--current-user)))
    (starintel-admin--call-json
     (lambda (ok result)
       (if ok
           (starintel-admin--show-org-object
            (format "User · %s" username)
            result)
         (user-error "%s" result)))
     "user" "show" username)))

(defun starintel-admin--show-org-object (title object)
  (let ((buffer (get-buffer-create (format "*StarIntel · %s*" title))))
    (with-current-buffer buffer
      (starintel-admin-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "* " title "\n\n")
        (insert "#+begin_src json\n")
        (insert (json-encode object))
        (insert "\n#+end_src\n")
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun starintel-admin--mutation (args &optional refresh-buffer)
  (let ((origin (or refresh-buffer (current-buffer))))
    (apply
     #'starintel-admin--call-json
     (lambda (ok result)
       (if (not ok)
           (user-error "%s" result)
         (message "StarIntel admin operation complete")
         (when (buffer-live-p origin)
           (with-current-buffer origin
             (cond
              ((derived-mode-p 'starintel-admin-users-mode)
               (starintel-admin-users-refresh))
              ((derived-mode-p 'starintel-admin-mode)
               (starintel-admin-refresh)))))))
     args)))

(defun starintel-admin-create-user (username plan)
  (interactive
   (list (read-string "Username: ")
         (read-string "Plan / tenant (empty for none): ")))
  (let ((origin (current-buffer))
        (args (append (list "user" "create" username)
                      (unless (string-empty-p plan)
                        (list "--plan" plan)))))
    (apply
     #'starintel-admin--call-json
     (lambda (ok result)
       (if (not ok)
           (user-error "%s" result)
         (let ((password (starintel-admin--get "temporary_password" result)))
           (when password
             (starintel-admin--show-secret username password)))
         (when (buffer-live-p origin)
           (with-current-buffer origin
             (cond
              ((derived-mode-p 'starintel-admin-users-mode)
               (starintel-admin-users-refresh))
              ((derived-mode-p 'starintel-admin-mode)
               (starintel-admin-refresh)))))))
     args)))

(defun starintel-admin--show-secret (username password)
  (let ((buffer (get-buffer-create "*StarIntel Temporary Secret*")))
    (with-current-buffer buffer
      (setq buffer-undo-list t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Temporary password for %s\n\n%s\n\n" username password))
        (insert "This is returned once. Copy it intentionally, then kill this buffer.\n"))
      (special-mode))
    (pop-to-buffer buffer)))

(defun starintel-admin-set-plan (username plan)
  (interactive (list (read-string "User: ") (read-string "Plan / tenant: ")))
  (starintel-admin--mutation (list "plan" "set" username plan)))

(defun starintel-admin-set-plan-at-point ()
  (interactive)
  (starintel-admin-set-plan
   (starintel-admin--current-user)
   (read-string "Plan / tenant: ")))

(defun starintel-admin-revoke-plan (username &optional plan)
  (interactive
   (list (read-string "User: ")
         (let ((value (read-string "Specific plan / tenant (empty = all): ")))
           (unless (string-empty-p value) value))))
  (starintel-admin--mutation
   (append (list "plan" "revoke" username)
           (when plan (list plan)))))

(defun starintel-admin-revoke-plan-at-point ()
  (interactive)
  (starintel-admin-revoke-plan
   (starintel-admin--current-user)
   (let ((value (read-string "Specific plan / tenant (empty = all): ")))
     (unless (string-empty-p value) value))))

(defun starintel-admin-revoke-user (username)
  (interactive (list (read-string "User to revoke: ")))
  (when (yes-or-no-p (format "Disable %s and revoke owned credentials? " username))
    (starintel-admin--mutation (list "user" "revoke" username))))

(defun starintel-admin-revoke-user-at-point ()
  (interactive)
  (starintel-admin-revoke-user (starintel-admin--current-user)))

(defun starintel-admin-revoke-key (credential-id)
  (interactive (list (read-string "Credential ID: ")))
  (when (yes-or-no-p (format "Permanently revoke %s? " credential-id))
    (starintel-admin--mutation (list "key" "revoke" credential-id))))

(defun starintel-admin-dataset-policy (dataset mode &optional plan)
  (interactive
   (let* ((dataset (read-string "Dataset: "))
          (mode (completing-read "Policy: " '("private" "public" "planned") nil t))
          (plan (when (string= mode "planned")
                  (read-string "Plan / tenant: "))))
     (list dataset mode plan)))
  (starintel-admin--mutation
   (append (list "dataset" mode dataset)
           (when plan (list plan)))))

(defun starintel-admin--documents (payload)
  (or (starintel-admin--get "hits" payload)
      (starintel-admin--get "rows" payload)
      nil))

(defun starintel-admin--document-object (row)
  (or (starintel-admin--get "doc" row)
      (starintel-admin--get "document" row)
      row))

(defun starintel-admin-documents-by-tenant (tenant query limit)
  (interactive
   (list
    (read-string "Tenant: ")
    (read-string "Query: " "*:*")
    (read-number "Limit: " starintel-admin-default-document-limit)))
  (starintel-admin--call-json
   (lambda (ok result)
     (if (not ok)
         (user-error "%s" result)
       (let ((buffer (get-buffer-create
                      (format "*StarIntel Documents · %s*" tenant))))
         (with-current-buffer buffer
           (starintel-admin-mode)
           (let ((inhibit-read-only t)
                 (docs (starintel-admin--documents result)))
             (erase-buffer)
             (insert (format "* Documents · tenant:%s\n\n" tenant))
             (insert (format "Query: =%s= · results: %d\n\n" query (length docs)))
             (insert "| ID | Type | Dataset | Summary |\n|---+---+---+---|\n")
             (dolist (row docs)
               (let ((doc (starintel-admin--document-object row)))
                 (insert
                  (format "| %s | %s | %s | %s |\n"
                          (starintel-admin--cell
                           (or (starintel-admin--get "_id" doc)
                               (starintel-admin--get "id" row)))
                          (starintel-admin--cell (starintel-admin--get "dtype" doc))
                          (starintel-admin--cell (starintel-admin--get "dataset" doc))
                          (starintel-admin--cell
                           (or (starintel-admin--get "title" (starintel-admin--get "data" doc))
                               (starintel-admin--get "name" (starintel-admin--get "data" doc))
                               ""))))))
             (org-table-align)
             (goto-char (point-min))))
         (pop-to-buffer buffer))))
   "documents" "tenant" tenant query (number-to-string limit)))

(defun starintel-admin-data-size (kind id &optional tenant)
  (interactive
   (let* ((kind (completing-read "Aggregate: " '("user" "group" "tenant") nil t))
          (id (read-string (format "%s: " (capitalize kind))))
          (tenant (when (string= kind "group")
                    (let ((value (read-string "Tenant scope (optional): ")))
                      (unless (string-empty-p value) value)))))
     (list kind id tenant)))
  (apply
   #'starintel-admin--call-json
   (lambda (ok result)
     (if (not ok)
         (user-error "%s" result)
       (let ((buffer (get-buffer-create
                      (format "*StarIntel Data Size · %s:%s*" kind id))))
         (with-current-buffer buffer
           (starintel-admin-mode)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (format "* Logical data size · %s:%s\n\n" kind id))
             (insert (propertize
                      "Logical JSON bytes of returned documents; not physical CouchDB/S3 disk usage.\n\n"
                      'face 'starintel-admin-muted-face))
             (insert "| Metric | Value |\n|---+---|\n")
             (insert (format "| Documents | %s |\n"
                             (or (starintel-admin--get "document_count" result) 0)))
             (insert (format "| Logical bytes | %s |\n"
                             (or (starintel-admin--get "logical_json_bytes" result) 0)))
             (insert (format "| Logical MiB | %.3f |\n"
                             (or (starintel-admin--get "logical_mib" result) 0.0)))
             (org-table-align)
             (goto-char (point-min))))
         (pop-to-buffer buffer))))
   (append (list "stats" kind id)
           (when tenant (list tenant)))))

(defun starintel-admin-graph-image (&optional tenant)
  (interactive
   (list (let ((value (read-string "Tenant for graph metrics (optional): ")))
           (unless (string-empty-p value) value))))
  (let ((origin (current-buffer))
        (args (append (list "graph" "image")
                      (when tenant (list "--tenant" tenant)))))
    (apply
     #'starintel-admin--call-json
     (lambda (ok result)
       (if (not ok)
           (user-error "%s" result)
         (let ((file (starintel-admin--get "image" result)))
           (unless (and file (file-readable-p file))
             (user-error "Graph image missing: %s" file))
           (with-current-buffer origin
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert "\n** Generated graph\n\n")
               (insert (format "[[file:%s]]\n" file))
               (org-display-inline-images))
             (message "StarIntel graph generated: %s" file)))))
     args)))

(provide 'starintel-admin)
;;; starintel-admin.el ends here
