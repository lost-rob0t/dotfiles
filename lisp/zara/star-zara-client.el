;;; star-zara-client.el --- Dotfiles-owned Zara client surface -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

(defgroup star-zara-client nil
  "Dotfiles-owned client/UI facade for the canonical Zara runtime."
  :group 'applications
  :prefix "star-zara-")

(defcustom star-zara-program "zara"
  "Canonical Zara CLI used by this client surface."
  :type 'string
  :group 'star-zara-client)

(defcustom star-zara-default-model "27b"
  "Expected default server-side model for operator status."
  :type 'string
  :group 'star-zara-client)

(defcustom star-zara-chat-buffer "*Mobile Zara*"
  "Fallback chat buffer used when the upstream native client is unavailable."
  :type 'string
  :group 'star-zara-client)

(defvar-local star-zara--busy nil)

(defun star-zara--program ()
  (or (executable-find star-zara-program)
      (user-error "Zara CLI is unavailable: %s" star-zara-program)))

(defun star-zara-status ()
  "Return the dotfiles Zara client status."
  (interactive)
  (let ((status
         (list :program star-zara-program
               :executable (executable-find star-zara-program)
               :model star-zara-default-model
               :provider "starintel"
               :endpoint "https://llm.starintel.actor")))
    (when (called-interactively-p 'interactive)
      (message "Zara: %s · StarIntel/%s"
               (or (plist-get status :executable) "unavailable")
               star-zara-default-model))
    status))

(defun star-zara-ask (prompt)
  "Synchronously ask Zara through its canonical CLI/runtime."
  (let ((program (star-zara--program)))
    (with-temp-buffer
      (let ((status (process-file program nil t nil prompt)))
        (if (and (integerp status) (zerop status))
            (string-trim (buffer-string))
          (error "Zara request failed: %s"
                 (string-trim (buffer-string))))))))

(defun star-zara-request (prompt callback)
  "Asynchronously ask Zara and call CALLBACK with response or error."
  (let* ((program (star-zara--program))
         (stdout (generate-new-buffer " *star-zara-out*"))
         (stderr (generate-new-buffer " *star-zara-err*")))
    (make-process
     :name "star-zara-request"
     :command (list program prompt)
     :buffer stdout
     :stderr stderr
     :noquery t
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((code (process-exit-status process))
               (out (with-current-buffer stdout
                      (string-trim (buffer-string))))
               (err (with-current-buffer stderr
                      (string-trim (buffer-string)))))
           (kill-buffer stdout)
           (kill-buffer stderr)
           (if (zerop code)
               (funcall callback out nil)
             (funcall callback nil
                      (if (string-empty-p err) out err)))))))))

(define-derived-mode star-zara-chat-mode special-mode "Mobile-Zara"
  "Fallback dotfiles Zara chat surface."
  (setq-local truncate-lines nil
              star-zara--busy nil)
  (setq-local header-line-format
              '(:eval
                (format " Zara · StarIntel/27b · %s · RET send"
                        (if star-zara--busy "working" "ready"))))
  (define-key star-zara-chat-mode-map (kbd "RET") #'star-zara-chat-send)
  (define-key star-zara-chat-mode-map (kbd "s") #'star-zara-chat-send))

(defun star-zara-chat-send (prompt)
  "Send PROMPT from the fallback Zara chat buffer."
  (interactive (list (read-string "Zara › ")))
  (when star-zara--busy
    (user-error "Zara is already handling a request"))
  (setq star-zara--busy t)
  (let ((target (current-buffer)))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize "You\n" 'face 'font-lock-keyword-face)
              prompt "\n\n"))
    (star-zara-request
     prompt
     (lambda (response error)
       (when (buffer-live-p target)
         (with-current-buffer target
           (setq star-zara--busy nil)
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (propertize
                      (if error "Zara · error\n" "Zara\n")
                      'face 'font-lock-keyword-face)
                     (or error response) "\n\n"))))))))

(defun star-zara-chat ()
  "Open the preferred Zara Emacs client.

When the reviewed upstream native package is available, reuse it. Otherwise
fall back to the dotfiles CLI client."
  (interactive)
  (if (and (require 'zara nil t) (fboundp 'zara-chat))
      (call-interactively #'zara-chat)
    (let ((buffer (get-buffer-create star-zara-chat-buffer)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'star-zara-chat-mode)
          (star-zara-chat-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Zara via dotfiles · StarIntel 27B\n\n"))))
      (pop-to-buffer buffer))))

(transient-define-prefix star-zara-menu ()
  "Zara, Mega Brain, and mobile operator commands."
  [["Zara"
    ("z" "Chat" star-zara-chat)
    ("s" "Status" star-zara-status)]
   ["Knowledge"
    ("f" "Food/recipes" org-food-menu)
    ("k" "Mega Brain status" star/mega-brain-status)
    ("r" "Reload Mega Brain" star/mega-brain-reset)
    ("u" "Unwind Prolog" star/mega-brain-unwind-all)]])

(provide 'star-zara-client)
;;; star-zara-client.el ends here
