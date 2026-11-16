;;; ai-dashboard.el --- Programmable startup cockpit for gptel -*- lexical-binding: t; -*-

(require 'button)
(require 'cl-lib)
(require 'json)
(require 'project)
(require 'subr-x)

(defgroup ai-dashboard nil
  "Programmable startup dashboard for the local gptel stack."
  :group 'applications
  :prefix "ai/dashboard-")

(defcustom ai/dashboard-state-file
  (expand-file-name
   "emacs/ai-dashboard.json"
   (or (getenv "XDG_STATE_HOME")
       (expand-file-name "~/.local/state/")))
  "Persistent typed cards added to the dashboard."
  :type 'file
  :group 'ai-dashboard)

(defcustom ai/dashboard-ideas-file
  (expand-file-name "~/Documents/Notes/org/ideas.org")
  "Human-facing Org ideas file opened by the Ideas card."
  :type 'file
  :group 'ai-dashboard)

(defconst ai/dashboard-valid-actions
  '("chat" "zara" "agenda" "ideas" "project" "file")
  "Actions an LLM-created dashboard card may invoke.")

(defconst ai/dashboard-default-cards
  '(((id . "chat")
     (title . "Tool Chat")
     (body . "Open persistent Org-native gptel with Emacs, file, Git, Prolog-RLM, image, and MCP tools.")
     (action . "chat")
     (argument))
    ((id . "zara")
     (title . "Zara")
     (body . "Open the native Zara client; Zara keeps ownership of runtime, memory, Prolog, plugins, approvals, and remote workers.")
     (action . "zara")
     (argument))
    ((id . "agenda")
     (title . "Today")
     (body . "Open the normal Org agenda.")
     (action . "agenda")
     (argument))
    ((id . "ideas")
     (title . "Ideas")
     (body . "Open the synced idea inbox where new concepts use the IDEA TODO state.")
     (action . "ideas")
     (argument)))
  "Built-in dashboard cards. Persistent cards with matching ids override them.")

(defun ai/dashboard--string (value field &optional max-length)
  "Validate VALUE as FIELD and return its trimmed string."
  (unless (stringp value)
    (user-error "%s must be a string" field))
  (let ((text (string-trim value)))
    (when (string-empty-p text)
      (user-error "%s must not be empty" field))
    (when (and max-length (> (length text) max-length))
      (user-error "%s must be at most %d characters" field max-length))
    text))

(defun ai/dashboard--normalize-card (card)
  "Validate CARD and return a normalized alist."
  (unless (listp card)
    (user-error "Dashboard card must be an alist"))
  (let* ((id (ai/dashboard--string (alist-get 'id card) "id" 64))
         (title (ai/dashboard--string (alist-get 'title card) "title" 100))
         (body (ai/dashboard--string (alist-get 'body card) "body" 600))
         (action (ai/dashboard--string (alist-get 'action card) "action" 32))
         (raw-argument (alist-get 'argument card))
         (argument
          (when (and (stringp raw-argument)
                     (not (string-empty-p (string-trim raw-argument))))
            (ai/dashboard--string raw-argument "argument" 2048))))
    (unless (string-match-p "^[a-z0-9][a-z0-9_-]*$" id)
      (user-error "id must match [a-z0-9][a-z0-9_-]*"))
    (unless (member action ai/dashboard-valid-actions)
      (user-error "Unsupported dashboard action: %s" action))
    (when (and (member action '("project" "file")) (not argument))
      (user-error "%s cards require an argument" action))
    (list (cons 'id id)
          (cons 'title title)
          (cons 'body body)
          (cons 'action action)
          (cons 'argument argument))))

(defun ai/dashboard--read-cards ()
  "Read persistent cards, returning nil for a missing or invalid state file."
  (if (not (file-readable-p ai/dashboard-state-file))
      nil
    (condition-case error
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (json-false nil)
              (json-null nil))
          (mapcar #'ai/dashboard--normalize-card
                  (json-read-file ai/dashboard-state-file)))
      (error
       (message "AI dashboard state ignored: %s" (error-message-string error))
       nil))))

(defun ai/dashboard--write-cards (cards)
  "Atomically persist CARDS."
  (let* ((directory (file-name-directory ai/dashboard-state-file))
         (_created (make-directory directory t))
         (temporary
          (make-temp-file (expand-file-name ".ai-dashboard-" directory)
                          nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (insert (json-encode cards) "\n"))
          (set-file-modes temporary 384)
          (rename-file temporary ai/dashboard-state-file t)
          (set-file-modes ai/dashboard-state-file 384))
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun ai/dashboard--merged-cards ()
  "Return built-in cards overlaid by persistent cards with matching ids."
  (let ((cards (copy-tree ai/dashboard-default-cards))
        (positions (make-hash-table :test #'equal)))
    (cl-loop for card in cards
             for index from 0
             do (puthash (alist-get 'id card) index positions))
    (dolist (card (ai/dashboard--read-cards))
      (let* ((id (alist-get 'id card))
             (position (gethash id positions)))
        (if (numberp position)
            (setf (nth position cards) card)
          (puthash id (length cards) positions)
          (setq cards (append cards (list card))))))
    cards))

(defun ai/dashboard-list-cards ()
  "Return all effective dashboard cards as JSON."
  (json-encode (ai/dashboard--merged-cards)))

(defun ai/dashboard-upsert-card (id title body action &optional argument)
  "Persist a typed card with ID, TITLE, BODY, ACTION, and ARGUMENT."
  (let* ((card (ai/dashboard--normalize-card
                (list (cons 'id id)
                      (cons 'title title)
                      (cons 'body body)
                      (cons 'action action)
                      (cons 'argument argument))))
         (cards (ai/dashboard--read-cards))
         (replaced nil)
         (updated
          (mapcar
           (lambda (existing)
             (if (equal (alist-get 'id existing) (alist-get 'id card))
                 (progn (setq replaced t) card)
               existing))
           cards)))
    (unless replaced
      (setq updated (append updated (list card))))
    (ai/dashboard--write-cards updated)
    (when (get-buffer "*AI Dashboard*")
      (ai/dashboard-refresh))
    (json-encode
     (list (cons 'status "ok")
           (cons 'id (alist-get 'id card))
           (cons 'action (alist-get 'action card))))))

(defun ai/dashboard-remove-card (id)
  "Remove persistent card ID. Built-in cards reappear when overrides vanish."
  (setq id (ai/dashboard--string id "id" 64))
  (let* ((cards (ai/dashboard--read-cards))
         (updated
          (cl-remove-if
           (lambda (card) (equal (alist-get 'id card) id))
           cards)))
    (ai/dashboard--write-cards updated)
    (when (get-buffer "*AI Dashboard*")
      (ai/dashboard-refresh))
    (json-encode
     (list (cons 'status "ok")
           (cons 'id id)
           (cons 'removed (< (length updated) (length cards)))))))

(defun ai/dashboard--project-root ()
  "Return current project root or nil."
  (when-let ((current (project-current nil)))
    (expand-file-name (project-root current))))

(defun ai/dashboard--invoke-card (card)
  "Invoke the reviewed action represented by CARD."
  (let ((action (alist-get 'action card))
        (argument (alist-get 'argument card)))
    (pcase action
      ("chat"
       (unless (require 'chat nil t)
         (user-error "gptel chat is unavailable"))
       (call-interactively #'ai/chat))
      ("zara"
       (unless (require 'zara nil t)
         (user-error "Zara Emacs package is unavailable; run doom sync"))
       (call-interactively #'zara-chat))
      ("agenda"
       (require 'org-agenda)
       (org-agenda nil "a"))
      ("ideas"
       (find-file ai/dashboard-ideas-file))
      ("project"
       (dired (expand-file-name argument)))
      ("file"
       (find-file (expand-file-name argument)))
      (_
       (user-error "Unsupported dashboard action: %s" action)))))

(defun ai/dashboard--insert-card (card)
  "Render CARD at point."
  (let ((entry card))
    (insert-text-button
     (alist-get 'title card)
     'face '(:weight bold :height 1.15)
     'follow-link t
     'help-echo (format "Run dashboard action: %s" (alist-get 'action card))
     'action (lambda (_button) (ai/dashboard--invoke-card entry)))
    (insert "\n")
    (insert (propertize (alist-get 'body card) 'face 'shadow))
    (insert "\n\n")))

(defvar ai-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'ai/dashboard-refresh)
    (define-key map (kbd "c")
                (lambda () (interactive)
                  (ai/dashboard--invoke-card (car ai/dashboard-default-cards))))
    (define-key map (kbd "z")
                (lambda () (interactive)
                  (ai/dashboard--invoke-card (cadr ai/dashboard-default-cards))))
    (define-key map (kbd "a")
                (lambda () (interactive)
                  (require 'org-agenda)
                  (org-agenda nil "a")))
    (define-key map (kbd "i")
                (lambda () (interactive)
                  (find-file ai/dashboard-ideas-file)))
    map)
  "Keymap for ai-dashboard-mode.")

(define-derived-mode ai-dashboard-mode special-mode "AI-Dashboard"
  "Major mode for the programmable AI startup dashboard."
  (setq-local truncate-lines nil))

(defun ai/dashboard-refresh ()
  "Refresh and return the dashboard buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*AI Dashboard*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-dashboard-mode)
        (ai-dashboard-mode))
      (let ((inhibit-read-only t)
            (root (ai/dashboard--project-root)))
        (erase-buffer)
        (insert (propertize "Command Center\n"
                            'face '(:height 1.8 :weight bold)))
        (insert (format "%s\n"
                        (format-time-string "%A · %Y-%m-%d · %H:%M")))
        (when root
          (insert (propertize (format "Project · %s\n" root) 'face 'shadow)))
        (insert
         (propertize
          "g refresh  ·  c tool chat  ·  z Zara  ·  a agenda  ·  i ideas\n\n"
          'face 'shadow))
        (dolist (card (ai/dashboard--merged-cards))
          (ai/dashboard--insert-card card))
        (goto-char (point-min))))
    buffer))

(defun ai/dashboard-buffer ()
  "Return the refreshed startup dashboard buffer."
  (ai/dashboard-refresh))

;;;###autoload
(defun ai/dashboard ()
  "Open the programmable startup dashboard."
  (interactive)
  (pop-to-buffer (ai/dashboard-buffer)))

(defun ai/dashboard-register-gptel-tools ()
  "Register typed dashboard mutation tools with gptel."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "gptel is unavailable"))
  (dolist (name '("DashboardListCards" "DashboardUpsertCard" "DashboardRemoveCard"))
    (when (fboundp 'gptel-get-tool)
      (ignore-errors (setf (gptel-get-tool name) nil))))
  (gptel-make-tool
   :name "DashboardListCards"
   :category "dashboard"
   :description "List the effective typed cards on the Emacs startup dashboard."
   :function #'ai/dashboard-list-cards)
  (gptel-make-tool
   :name "DashboardUpsertCard"
   :category "dashboard"
   :description
   "Create or replace a startup card. Actions are restricted to chat, zara, agenda, ideas, project, or file."
   :function #'ai/dashboard-upsert-card
   :args
   '((:name "id" :type string :description "Stable lowercase card id")
     (:name "title" :type string :description "Short visible title")
     (:name "body" :type string :description "Visible explanatory text")
     (:name "action" :type string
            :enum ["chat" "zara" "agenda" "ideas" "project" "file"]
            :description "Reviewed action type")
     (:name "argument" :type string :optional t
            :description "Path used only by project/file cards"))
   :confirm t)
  (gptel-make-tool
   :name "DashboardRemoveCard"
   :category "dashboard"
   :description "Remove a persistent dashboard card or override by id."
   :function #'ai/dashboard-remove-card
   :args '((:name "id" :type string :description "Card id to remove"))
   :confirm t)
  t)

(provide 'ai-dashboard)
;;; ai-dashboard.el ends here
