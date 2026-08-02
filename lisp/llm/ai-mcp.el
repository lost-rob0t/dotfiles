;;; ai-mcp.el --- MCP runtime for gptel -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'subr-x)

(defgroup ai/mcp nil
  "MCP server integration for gptel."
  :group 'applications
  :prefix "ai/mcp-")

(defcustom ai/mcp-auto-connect t
  "When non-nil, start configured MCP servers after gptel loads."
  :type 'boolean
  :group 'ai/mcp)

(defcustom ai/mcp-proxmox-config-file
  (expand-file-name "~/.config/proxmox-mcp/config.json")
  "JSON configuration used by the Proxmox MCP server."
  :type 'file
  :group 'ai/mcp)

(defcustom ai/mcp-discord-auth-host "discord"
  "auth-source host used to look up the Discord bot token."
  :type 'string
  :group 'ai/mcp)

(defcustom ai/mcp-discord-auth-user "bot"
  "auth-source user used to look up the Discord bot token."
  :type 'string
  :group 'ai/mcp)

(defun ai/mcp--discord-token ()
  "Return the Discord bot token from auth-source, or nil."
  (when-let* ((entry (car (auth-source-search
                           :host ai/mcp-discord-auth-host
                           :user ai/mcp-discord-auth-user
                           :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun ai/mcp-register-proxmox (&optional noerror)
  "Register the local Proxmox MCP launcher.
With NOERROR, return nil instead of signaling when unavailable."
  (cond
   ((not (file-readable-p ai/mcp-proxmox-config-file))
    (unless noerror
      (user-error "Missing Proxmox MCP config: %s"
                  ai/mcp-proxmox-config-file)))
   ((not (executable-find "proxmox-mcp-launcher"))
    (unless noerror
      (user-error "proxmox-mcp-launcher is not on exec-path")))
   (t
    (setq mcp-hub-servers
          (cons '("proxmox" . (:command "proxmox-mcp-launcher"))
                (assoc-delete-all "proxmox" mcp-hub-servers)))
    t)))

(defun ai/mcp-register-discord (&optional noerror)
  "Register the local Discord MCP launcher.
With NOERROR, return nil instead of signaling when unavailable."
  (cond
   ((not (executable-find "discord-mcp-launcher"))
    (unless noerror
      (user-error "discord-mcp-launcher is not on exec-path")))
   ((let ((token (ai/mcp--discord-token)))
      (when (and token (not (string-empty-p token)))
        (setq mcp-hub-servers
              (cons `("discord" . (:command "discord-mcp-launcher"
                                    :env (:DISCORD_TOKEN ,token)))
                    (assoc-delete-all "discord" mcp-hub-servers)))
        t)))
   (noerror nil)
   (t
    (user-error "No Discord token in auth-source for host=%s user=%s"
                ai/mcp-discord-auth-host ai/mcp-discord-auth-user))))

(defun ai/mcp-connect-all (&optional noerror)
  "Start configured MCP servers and register their tools with gptel.
With NOERROR, report failures without aborting Emacs startup."
  (interactive)
  (condition-case err
      (progn
        (require 'gptel-integrations)
        (require 'mcp-hub)
        (ai/mcp-register-discord 'noerror)
        (ai/mcp-register-proxmox 'noerror)
        (gptel-mcp-connect)
        t)
    (error
     (if noerror
         (progn
           (message "MCP startup skipped: %s" (error-message-string err))
           nil)
       (signal (car err) (cdr err))))))

(defun ai/mcp-connect-proxmox ()
  "Register and connect the Proxmox MCP server."
  (interactive)
  (require 'gptel-integrations)
  (require 'mcp-hub)
  (ai/mcp-register-proxmox)
  (gptel-mcp-connect '("proxmox")))

(defun ai/mcp-connect-discord ()
  "Register and connect the Discord MCP server."
  (interactive)
  (require 'gptel-integrations)
  (require 'mcp-hub)
  (ai/mcp-register-discord)
  (gptel-mcp-connect '("discord")))

(defun ai/mcp-schedule-startup ()
  "Schedule MCP startup without invoking a Doom autoloaded function."
  (when ai/mcp-auto-connect
    (run-at-time 0.75 nil
                 (lambda ()
                   (ai/mcp-connect-all 'noerror)))))

(with-eval-after-load 'gptel
  (ai/mcp-schedule-startup))

(provide 'ai-mcp)
;;; ai-mcp.el ends here
