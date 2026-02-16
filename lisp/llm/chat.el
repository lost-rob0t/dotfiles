;;; chat.el --- Enhanced AI/Chat interface with automatic summarization -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 2.0
;; Package-Requires: ((emacs "27.1") (gptel "0.1"))
;; Keywords: ai, chat, llm

;;; Commentary:

;; Enhanced AI/Chat interface using gptel with org-mode.
;; Features:
;; - Auto-summarization using Claude Haiku after first exchange
;; - Automatic buffer title updates
;; - Clean token usage (removes wasteful insertions)
;; - Auto-save as .org.llm files
;; - Base tools integration
;; - Auto-insert USER prompt after AI responses

;;; Code:

(require 'gptel)
(require 'org)

(let ((ai-lib (expand-file-name "ai.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p ai-lib)
    (load ai-lib nil t)))

;;; ============================================================================
;;; Configuration Variables
;;; ============================================================================

(defcustom ai/chat-summary-backend nil
  "Backend to use for summarization. If nil, creates Claude Haiku backend."
  :type 'symbol
  :group 'ai-chat)

(defcustom ai/chat-save-directory (expand-file-name "~/Document/Notes/org/roam/llm/")
  "Directory to save chat files."
  :type 'directory
  :group 'ai-chat)

(defcustom ai/chat-auto-save t
  "Whether to auto-save chats after summarization."
  :type 'boolean
  :group 'ai-chat)

;;; ============================================================================
;;; Backend Setup
;;; ============================================================================

(defun ai/chat--get-summary-backend ()
  "Get or create the OpenRouter backend for summarization."
  (or ai/chat-summary-backend
      (setq ai/chat-summary-backend
            (ai/llm-openrouter-backend :stream nil :name "OpenRouterReWrite"))))

(defun ai/chat--get-main-backend ()
  "Get the main chat backend."
  (ai/llm-openrouter-backend :stream t :name "OpenRouter"))

;;; ============================================================================
;;; Summarization Logic
;;; ============================================================================

(defvar ai/chat--first-exchange-done nil
  "Buffer-local variable tracking if first exchange is complete.")
(make-variable-buffer-local 'ai/chat--first-exchange-done)

(defvar ai/chat--original-content nil
  "Buffer-local variable storing original content before summarization.")
(make-variable-buffer-local 'ai/chat--original-content)

(defun ai/chat--extract-conversation ()
  "Extract the conversation content from current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; Find first USER message
    (when (re-search-forward "^\\*+ USER:" nil t)
      (let ((start (match-beginning 0)))
        (buffer-substring-no-properties start (point-max))))))

(defun ai/chat--summarize-async (conversation buffer-name callback)
  "Summarize CONVERSATION using Claude Haiku.
BUFFER-NAME is the chat buffer name for context.
CALLBACK is called with (title summary) on success."
  (let ((backend (ai/chat--get-summary-backend))
        (prompt (format "Please analyze this conversation and provide:

1. A concise title (max 60 chars) that captures the main topic/request
2. A brief summary (2-3 sentences) of what was discussed and accomplished

Format your response as:
TITLE: [your title here]
SUMMARY: [your summary here]

Conversation:
%s" conversation)))
    
    (gptel-request prompt
      :backend backend
      :model (ai/llm-resolve-model)
      :callback
      (lambda (response info)
        (if response
            (let ((title (ai/chat--extract-title response))
                  (summary (ai/chat--extract-summary response)))
              (funcall callback title summary))
          (message "Failed to summarize: %s" (plist-get info :status)))))))

(defun ai/chat--extract-title (response)
  "Extract title from summarization RESPONSE."
  (when (string-match "TITLE: *\\(.+\\)" response)
    (string-trim (match-string 1 response))))

(defun ai/chat--extract-summary (response)
  "Extract summary from summarization RESPONSE."
  (when (string-match "SUMMARY: *\\(.+\\)" response)
    (string-trim (match-string 1 response))))

;;; ============================================================================
;;; Buffer Management
;;; ============================================================================

(defun ai/chat--update-buffer-header (title summary)
  "Update buffer header with TITLE and SUMMARY."
  (save-excursion
    (goto-char (point-min))
    ;; Replace or add the header
    (if (looking-at "^#\\+TITLE:")
        ;; Replace existing header
        (progn
          (kill-line)
          (kill-line) ; Remove description line if exists
          (when (looking-at "^#\\+DESCRIPTION:")
            (kill-line))
          (when (looking-at "^$")
            (kill-line))) ; Remove blank line
      ;; Insert new header
      (insert "\n")
      (goto-char (point-min)))
    
    ;; Insert new header
    (insert (format "#+TITLE: %s\n" title))
    (insert (format "#+DESCRIPTION: %s\n\n" summary))))

(defun ai/chat--save-buffer (buffer-name title)
  "Save buffer to file with proper name based on TITLE."
  (when ai/chat-auto-save
    (let* ((safe-title (replace-regexp-in-string "[^a-zA-Z0-9-_ ]" "" title))
           (filename (format "%s.org.llm" safe-title))
           (filepath (expand-file-name filename ai/chat-save-directory)))
      
      ;; Ensure directory exists
      (unless (file-exists-p ai/chat-save-directory)
        (make-directory ai/chat-save-directory t))
      
      ;; Save buffer
      (write-file filepath)
      (message "Saved chat: %s" filepath))))

;;; ============================================================================
;;; Hook Functions
;;; ============================================================================

(defun ai/chat--auto-insert-user-prompt (start end)
  "Auto-insert USER prompt after AI response.
Called as a hook function with START and END positions of the AI response."
  ;; Only run in LLM Chat buffers
  (when (string-match-p "\\*LLM Chat\\*" (buffer-name))
    (save-excursion
      (goto-char end)
      ;; Move past any trailing whitespace
      (skip-chars-forward " \t\n")
      ;; Check if we're not already at a USER prompt
      (unless (looking-at "\\*+ USER:")
        ;; Insert newline if not at beginning of line
        (unless (bolp)
          (insert "\n"))
        ;; Insert the USER prompt with consistent level
        (insert "\n*** USER: ")))))

(defun ai/chat--post-response-handler (start end)
  "Handle post-response tasks including summarization.
Called with START and END positions of the AI response."
  (when (string-match-p "\\*LLM Chat\\*" (buffer-name))
    ;; Insert user prompt first
    (ai/chat--auto-insert-user-prompt start end)
    
    ;; Check if this is the first exchange completion
    (unless ai/chat--first-exchange-done
      (setq ai/chat--first-exchange-done t)
      (setq ai/chat--original-content (buffer-string))
      
      ;; Start summarization process
      (let ((conversation (ai/chat--extract-conversation))
            (current-buffer (current-buffer))
            (buffer-name (buffer-name)))
        (when conversation
          (ai/chat--summarize-async
           conversation
           buffer-name
           (lambda (title summary)
             (when (buffer-live-p current-buffer)
               (with-current-buffer current-buffer
                 (ai/chat--update-buffer-header title summary)
                 (rename-buffer (format "*%s*" title))
                 (ai/chat--save-buffer buffer-name title)
                 (message "Chat summarized: %s" title))))))))))

;;; ============================================================================
;;; Main Interface
;;; ============================================================================

;;;###autoload
(defun ai/chat ()
  "Start an enhanced AI chat session with auto-summarization."
  (interactive)
  (let* ((buffer-name (read-string "Chat name: " "*LLM Chat*"))
         (buffer (get-buffer-create buffer-name))
         (backend (ai/chat--get-main-backend)))
    
    (with-current-buffer buffer
      ;; Initialize org-mode and buffer state
      (unless (eq major-mode 'org-mode)
        (org-mode))
      
      ;; Clear buffer and start fresh
      (erase-buffer)
      
      ;; Set up minimal initial content (no wasteful descriptions)
      (insert "*** USER: ")
      
       ;; Configure gptel
       (setq-local gptel-backend backend)
       (setq-local gptel-model (ai/llm-resolve-model))
       (setq-local gptel--system-message ai/todo-system-prompt)
      
      ;; Initialize buffer-local variables
      (setq-local ai/chat--first-exchange-done nil)
      (setq-local ai/chat--original-content nil)
      
      ;; Set up hooks
      (add-hook 'gptel-post-response-functions #'ai/chat--post-response-handler nil t)
      
      ;; Activate gptel-mode
      (gptel-mode 1))
    
    ;; Switch to buffer and position cursor
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (message "Enhanced AI Chat started. First exchange will be auto-summarized.")))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

;;;###autoload
(defun ai/chat-list-saved ()
  "List all saved chat files."
  (interactive)
  (if (file-exists-p ai/chat-save-directory)
      (let ((files (directory-files ai/chat-save-directory nil "\\.org\\.llm$")))
        (if files
            (progn
              (with-current-buffer (get-buffer-create "*Saved Chats*")
                (erase-buffer)
                (insert "# Saved AI Chats\n\n")
                (dolist (file files)
                  (insert (format "- [[file:%s][%s]]\n" 
                                  (expand-file-name file ai/chat-save-directory)
                                  (file-name-sans-extension file))))
                (org-mode)
                (goto-char (point-min)))
              (switch-to-buffer "*Saved Chats*"))
          (message "No saved chats found")))
    (message "Chat directory doesn't exist: %s" ai/chat-save-directory)))

;;;###autoload
(defun ai/chat-open-saved (filename)
  "Open a saved chat file."
  (interactive
   (list (completing-read "Open chat: "
                          (when (file-exists-p ai/chat-save-directory)
                            (directory-files ai/chat-save-directory nil "\\.org\\.llm$")))))
  (let ((filepath (expand-file-name filename ai/chat-save-directory)))
    (if (file-exists-p filepath)
        (find-file filepath)
      (error "Chat file not found: %s" filepath))))

;;;###autoload
(defun ai/chat-clean-directory ()
  "Clean up old chat files (interactive selection)."
  (interactive)
  (when (file-exists-p ai/chat-save-directory)
    (let ((files (directory-files ai/chat-save-directory nil "\\.org\\.llm$")))
      (when files
        (let ((to-delete (completing-read-multiple "Delete chats: " files)))
          (dolist (file to-delete)
            (when (y-or-n-p (format "Really delete %s? " file))
              (delete-file (expand-file-name file ai/chat-save-directory))
              (message "Deleted: %s" file))))))))

(provide 'chat)

;;; chat.el ends here
