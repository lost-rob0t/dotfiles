;;; qtile-desktop.el --- Qtile popup helpers -*- lexical-binding: t; -*-

(require 'json)
(require 'org)
(require 'org-agenda)
(require 'subr-x)
(require 'url)
(require 'url-http)

(let ((qtile-ui-directory (expand-file-name "~/.dotfiles/lisp/qtile")))
  (when (file-directory-p qtile-ui-directory)
    (add-to-list 'load-path qtile-ui-directory)))
(require 'qtile-ui)
(require 'qtile-ui-org)

(defconst qtile-desktop-private-env
  (expand-file-name "~/.config/qtile/private.env"))
(defvar-local qtile-agent-zero-context-id nil)
(defvar-local qtile-agent-zero-prompt-start nil)

(defun qtile-desktop--load-private-env ()
  "Load KEY=VALUE overrides without ever echoing secret values."
  (when (file-readable-p qtile-desktop-private-env)
    (with-temp-buffer
      (insert-file-contents qtile-desktop-private-env)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (unless (or (string-empty-p line) (string-prefix-p "#" line))
            (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line)
              (let* ((key (string-trim (match-string 1 line)))
                     (value (string-trim (match-string 2 line)))
                     (value (string-trim value "['\"]" "['\"]")))
                (setenv key (substitute-in-file-name value))))))
        (forward-line 1)))))

(defun qtile-desktop--title (title)
  (set-frame-parameter nil 'name title)
  (set-frame-parameter nil 'title title))

(defun qtile-org-todos-open (&optional _params)
  "Show the global TODO list in the current Qtile scratch frame."
  (interactive)
  (qtile-desktop--title "qtile-org-todos")
  (org-agenda nil "t")
  (delete-other-windows)
  (qtile-ui-prepare-buffer)
  (qtile-ui-bind-dismiss))

(defun qtile-org-agenda-day (_params)
  "Show today's one-day Org agenda in a Qtile popup."
  (interactive)
  (qtile-desktop--title "qtile-org-agenda-day")
  (org-agenda-list nil (current-time) 1)
  (delete-other-windows)
  (qtile-ui-prepare-buffer)
  (qtile-ui-bind-dismiss))

(defun qtile-workflow-read (choices)
  "Select one Qtile workflow from CHOICES in a temporary GUI frame."
  (let ((frame (make-frame '((name . "qtile-workflow")
                             (title . "qtile-workflow")
                             (width . 58)
                             (height . 10)
                             (minibuffer . t)))))
    (unwind-protect
        (with-selected-frame frame
          (select-frame-set-input-focus frame)
          (completing-read "Qtile workflow: " choices nil t))
      (when (frame-live-p frame)
        (delete-frame frame)))))

(defvar qtile-agent-zero-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'qtile-agent-zero-send)
    (define-key map (kbd "C-c C-r") #'qtile-agent-zero-reset)
    map))

(define-derived-mode qtile-agent-zero-mode text-mode "Qtile-A0"
  "Tiny asynchronous Agent Zero client for a Qtile scratch frame."
  (setq-local header-line-format
              "Agent Zero  |  C-c C-c send  |  C-c C-r new context")
  (setq-local qtile-agent-zero-context-id nil)
  (setq-local qtile-agent-zero-prompt-start nil))

(defun qtile-agent-zero--api-key ()
  (qtile-desktop--load-private-env)
  (or (getenv "AGENT_ZERO_API_KEY")
      (let ((file (getenv "AGENT_ZERO_API_KEY_FILE")))
        (when (and file (file-readable-p file))
          (string-trim
           (with-temp-buffer
             (insert-file-contents file)
             (buffer-string)))))))

(defun qtile-agent-zero--host ()
  (qtile-desktop--load-private-env)
  (when-let ((host (getenv "AGENT_ZERO_HOST")))
    (replace-regexp-in-string "/+\\'" "" host)))

(defun qtile-agent-zero--insert-prompt ()
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (insert "> ")
  (setq qtile-agent-zero-prompt-start (point))
  (goto-char (point-max)))

(defun qtile-agent-zero-open (&optional _params)
  "Open or reuse the Qtile Agent Zero scratch buffer."
  (interactive)
  (qtile-desktop--title "qtile-agent-zero")
  (let ((buffer (get-buffer-create "*Qtile Agent Zero*")))
    (switch-to-buffer buffer)
    (unless (derived-mode-p 'qtile-agent-zero-mode)
      (qtile-agent-zero-mode))
    (qtile-ui-prepare-buffer)
    (when (= (buffer-size) 0)
      (insert "Agent Zero from Qtile\n\n")
      (qtile-agent-zero--insert-prompt))
    (goto-char (point-max))))

(defun qtile-agent-zero-reset ()
  "Forget local API conversation continuity and start a new context."
  (interactive)
  (setq qtile-agent-zero-context-id nil)
  (goto-char (point-max))
  (insert "\n[new Agent Zero context]\n")
  (qtile-agent-zero--insert-prompt))

(defun qtile-agent-zero--response-body ()
  (goto-char (point-min))
  (if (search-forward "\n\n" nil t)
      (buffer-substring-no-properties (point) (point-max))
    ""))

(defun qtile-agent-zero--finish (status target-buffer)
  "Handle Agent Zero URL callback STATUS into TARGET-BUFFER."
  (let ((body (qtile-agent-zero--response-body))
        (code (plist-get status :error)))
    (kill-buffer (current-buffer))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (goto-char (point-max))
        (condition-case err
            (if code
                (insert (format "\nA0 error: %s\n" code))
              (let* ((json-object-type 'alist)
                     (payload (json-read-from-string body))
                     (context (alist-get 'context_id payload))
                     (response (or (alist-get 'response payload)
                                   (alist-get 'message payload)
                                   body)))
                (when (stringp context)
                  (setq qtile-agent-zero-context-id context))
                (insert (format "\nA0: %s\n" response))))
          (error
           (insert (format "\nA0 parse error: %s\n" (error-message-string err)))))
        (qtile-agent-zero--insert-prompt)))))

(defun qtile-agent-zero-send ()
  "Send the current prompt to Agent Zero asynchronously."
  (interactive)
  (let* ((target-buffer (current-buffer))
         (start (or qtile-agent-zero-prompt-start (point-min)))
         (prompt (string-trim
                  (buffer-substring-no-properties start (point-max))))
         (host (qtile-agent-zero--host))
         (key (qtile-agent-zero--api-key)))
    (cond
     ((string-empty-p prompt)
      (message "Agent Zero prompt is empty"))
     ((not host)
      (goto-char (point-max))
      (insert "\nA0 config: set AGENT_ZERO_HOST in ~/.config/qtile/private.env\n")
      (qtile-agent-zero--insert-prompt))
     ((not key)
      (goto-char (point-max))
      (insert "\nA0 config: set AGENT_ZERO_API_KEY or AGENT_ZERO_API_KEY_FILE\n")
      (qtile-agent-zero--insert-prompt))
     (t
      (goto-char (point-max))
      (insert "\nA0: working…\n")
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("X-API-KEY" . ,key)))
             (payload `((message . ,prompt)))
             (payload (if qtile-agent-zero-context-id
                          (append payload `((context_id . ,qtile-agent-zero-context-id)))
                        payload))
             (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
        (url-retrieve
         (concat host "/api_message")
         #'qtile-agent-zero--finish
         (list target-buffer)
         t
         t))))))

(provide 'qtile-desktop)
;;; qtile-desktop.el ends here
