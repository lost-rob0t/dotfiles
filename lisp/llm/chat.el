;;; chat.el --- Org-native gptel agent chat -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)
(require 'org)
(require 'subr-x)
(require 'ai)
(require 'ai-agent)

(defgroup ai/chat nil
  "Org-native gptel chat sessions."
  :group 'ai/llm
  :prefix "ai/chat-")

(defcustom ai/chat-directory
  (expand-file-name "~/Documents/Notes/org/roam/llm/")
  "Directory where `ai/chat' stores Org chat files."
  :type 'directory
  :group 'ai/chat)

(define-obsolete-variable-alias
  'ai/chat-save-directory 'ai/chat-directory "2026-08-05")

(defcustom ai/chat-auto-save t
  "When non-nil, save chat buffers after each successful response."
  :type 'boolean
  :group 'ai/chat)

(defcustom ai/chat-auto-title t
  "When non-nil, generate a title after the first response."
  :type 'boolean
  :group 'ai/chat)

(defcustom ai/chat-system-prompt ai/agent-system-prompt
  "System message used for new agent chat buffers."
  :type 'string
  :group 'ai/chat)

(defvar-local ai/chat--titled nil
  "Non-nil after the current chat has received an automatic title.")

(defvar-local ai/chat--session-id nil
  "Stable timestamp identifier used for the current chat file.")

(defun ai/chat--slug (text)
  "Return a filesystem-safe slug for TEXT."
  (let* ((downcase (downcase (string-trim text)))
         (clean (replace-regexp-in-string "[^[:alnum:]]+" "-" downcase)))
    (string-trim clean "-+" "-+")))

(defun ai/chat--session-id ()
  "Return or create the current chat timestamp identifier."
  (or ai/chat--session-id
      (setq ai/chat--session-id (format-time-string "%Y-%m-%d_%H-%M-%S"))))

(defun ai/chat--file (&optional title)
  "Return the chat file path using TITLE as its slug."
  (let ((slug (ai/chat--slug (or title "chat"))))
    (when (string-empty-p slug)
      (setq slug "chat"))
    (expand-file-name
     (format "%s_%s.org" (ai/chat--session-id) slug)
     ai/chat-directory)))

(defun ai/chat--agent-tools ()
  "Return registered local agent tools plus globally active tools."
  (let ((tools (copy-sequence (default-value 'gptel-tools))))
    (dolist (name ai/agent-tools tools)
      (when-let ((tool (gptel-get-tool name)))
        (cl-pushnew tool tools :test #'eq)))))

(defun ai/chat--ensure-header (&optional title)
  "Ensure the current Org chat has persistent metadata for TITLE."
  (save-excursion
    (goto-char (point-min))
    (unless (looking-at-p "#\\+title:")
      (insert (format "#+title: %s\n" (or title "LLM Chat"))
              "#+category: llm\n"
              "#+filetags: :llm:gptel:\n\n"))))

(defun ai/chat--conversation-text ()
  "Return the current conversation without Org file metadata."
  (save-excursion
    (goto-char (point-min))
    (while (looking-at-p "#\\+")
      (forward-line 1))
    (string-trim (buffer-substring-no-properties (point) (point-max)))))

(defun ai/chat--summary-backend ()
  "Return a non-streaming backend matching `ai/llm-provider'."
  (pcase ai/llm-provider
    ('zai (ai/llm-zai-backend :stream nil :name "Z.AI Summary"))
    ('openai (ai/llm-openai-backend :stream nil :name "OpenAI Summary"))
    ('openrouter (ai/llm-openrouter-backend :stream nil :name "OpenRouter Summary"))))

(defun ai/chat--extract-field (field response)
  "Extract FIELD from RESPONSE formatted as FIELD: value."
  (when (string-match (format "^%s:[[:space:]]*\\(.+\\)$" (regexp-quote field))
                      response)
    (string-trim (match-string 1 response))))

(defun ai/chat--set-title (title summary)
  "Set Org TITLE and SUMMARY metadata without renaming the chat file."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:.*$" nil t)
        (replace-match (format "#+title: %s" title) t t)
      (insert (format "#+title: %s\n" title)))
    (goto-char (point-min))
    (if (re-search-forward "^#\\+description:.*$" nil t)
        (replace-match (format "#+description: %s" summary) t t)
      (forward-line 1)
      (insert (format "#+description: %s\n" summary))))
  (rename-buffer (format "*LLM: %s*" title) t)
  (ai/chat--save))

(defun ai/chat--title-async ()
  "Generate and apply a title and summary for the current chat."
  (let ((source-buffer (current-buffer))
        (conversation (ai/chat--conversation-text)))
    (unless (string-empty-p conversation)
      (gptel-request
       (format "Return exactly two single-line fields for this conversation:\nTITLE: a concrete title under 60 characters\nSUMMARY: one sentence describing the work\n\n%s"
               conversation)
       :backend (ai/chat--summary-backend)
       :model (ai/llm-resolve-model)
       :stream nil
       :callback
       (lambda (response info)
         (when (and response (buffer-live-p source-buffer))
           (let ((title (ai/chat--extract-field "TITLE" response))
                 (summary (ai/chat--extract-field "SUMMARY" response)))
             (when (and title summary)
               (with-current-buffer source-buffer
                 (setq ai/chat--titled t)
                 (ai/chat--set-title title summary)))))
         (unless response
           (message "Chat title generation failed: %s" (plist-get info :status))))))))

(defun ai/chat--save ()
  "Persist the current chat buffer."
  (when ai/chat-auto-save
    (make-directory ai/chat-directory t)
    (unless buffer-file-name
      (set-visited-file-name (ai/chat--file "chat") t))
    (save-buffer)))

(defun ai/chat--after-response (_start _end)
  "Persist the response and optionally generate a title."
  (when (derived-mode-p 'org-mode)
    (ai/chat--save)
    (when (and ai/chat-auto-title (not ai/chat--titled))
      (ai/chat--title-async))))

;;;###autoload
(defun ai/chat (&optional name)
  "Create and open a persistent Org agent chat named NAME.

The file is created immediately under `ai/chat-directory' as
YYYY-MM-DD_HH-MM-SS_slug.org.  The buffer enables the local Emacs agent
tools together with any globally active MCP tools."
  (interactive)
  (let* ((input (or name (read-string "Chat name: " "chat")))
         (name (if (string-empty-p (string-trim input)) "chat" input))
         (buffer (generate-new-buffer (format "*LLM: %s*" name))))
    (with-current-buffer buffer
      (org-mode)
      (setq-local ai/chat--session-id (format-time-string "%Y-%m-%d_%H-%M-%S"))
      (setq-local gptel-backend (ai/llm-backend))
      (setq-local gptel-model (ai/llm-resolve-model))
      (setq-local gptel-system-message ai/chat-system-prompt)
      (setq-local gptel-tools (ai/chat--agent-tools))
      (setq-local gptel-use-tools t)
      (setq-local gptel-use-context 'system)
      (setq-local gptel-track-media t)
      (setq-local gptel-include-reasoning t)
      (make-directory ai/chat-directory t)
      (set-visited-file-name (ai/chat--file name) t)
      (ai/chat--ensure-header name)
      (goto-char (point-max))
      (insert "* User\n")
      (add-hook 'gptel-post-response-functions #'ai/chat--after-response nil t)
      (gptel-mode 1)
      (ai/chat--save))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

;;;###autoload
(defun ai/chat-list-saved ()
  "Open a Dired buffer containing saved LLM chats."
  (interactive)
  (make-directory ai/chat-directory t)
  (dired ai/chat-directory))

;;;###autoload
(defun ai/chat-resume (file)
  "Resume saved gptel chat FILE."
  (interactive
   (list (read-file-name "Chat file: " ai/chat-directory nil t nil
                         (lambda (path)
                           (or (file-directory-p path)
                               (string-match-p "\\.org\\'" path))))))
  (find-file file)
  (org-mode)
  (setq-local gptel-backend (ai/llm-backend))
  (setq-local gptel-model (ai/llm-resolve-model))
  (setq-local gptel-system-message ai/chat-system-prompt)
  (setq-local gptel-tools (ai/chat--agent-tools))
  (setq-local gptel-use-tools t)
  (setq-local gptel-use-context 'system)
  (setq-local gptel-track-media t)
  (setq-local gptel-include-reasoning t)
  (add-hook 'gptel-post-response-functions #'ai/chat--after-response nil t)
  (gptel-mode 1))

(provide 'chat)
;;; chat.el ends here
