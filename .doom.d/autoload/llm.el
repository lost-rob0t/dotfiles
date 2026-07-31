;;; llm.el --- Doom autoloads for the local gptel stack -*- lexical-binding: t; -*-

(defcustom +llm/proxmox-config-file
  (expand-file-name "~/.config/proxmox-mcp/config.json")
  "JSON configuration used by the Emacs Proxmox MCP server."
  :type 'file
  :group 'applications)

(defun +llm/proxmox-register ()
  "Register the local Proxmox MCP launcher with mcp.el."
  (unless (file-readable-p +llm/proxmox-config-file)
    (user-error "Missing Proxmox MCP config: %s" +llm/proxmox-config-file))
  (unless (executable-find "proxmox-mcp-launcher")
    (user-error "proxmox-mcp-launcher is not on exec-path; apply Home Manager first"))
  (setq mcp-hub-servers
        (cons '("proxmox" . (:command "proxmox-mcp-launcher"))
              (assoc-delete-all "proxmox" mcp-hub-servers))))

(defun +llm/apply-final-defaults ()
  "Make the shared gptel configuration authoritative after package setup."
  (when (featurep 'gptel)
    (require 'ai)
    (ai/llm-apply-defaults)))

;;;###autoload
(defun +llm/proxmox-connect ()
  "Register and connect the Proxmox MCP tools to gptel."
  (interactive)
  (require 'gptel-integrations)
  (require 'mcp-hub)
  (+llm/proxmox-register)
  (gptel-mcp-connect '("proxmox")))

;;;###autoload
(with-eval-after-load 'gptel
  (require 'ai)
  (require 'ai-agent)
  ;; Run after every `eval-after-load' and `use-package!' callback for gptel.
  (run-at-time 0 nil #'+llm/apply-final-defaults))

;;;###autoload
(with-eval-after-load 'mcp-hub
  (when (and (file-readable-p +llm/proxmox-config-file)
             (executable-find "proxmox-mcp-launcher"))
    (+llm/proxmox-register)))

(defcustom +llm/discord-auth-host "discord"
  "auth-source host used to look up the Discord bot token."
  :type 'string
  :group 'applications)

(defcustom +llm/discord-auth-user "bot"
  "auth-source user used to look up the Discord bot token."
  :type 'string
  :group 'applications)

(defun +llm/discord-token ()
  "Return the Discord bot token from auth-source, or nil.
The token is read from the auth-source entry whose host matches
`+llm/discord-auth-host' and user `+llm/discord-auth-user'.  Add
the entry to `~/.authinfo.gpg' as:

    machine discord login bot password TOKEN"
  (when-let* ((entry (car (auth-source-search
                           :host +llm/discord-auth-host
                           :user +llm/discord-auth-user
                           :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

;;;###autoload
(defun +llm/discord-register (&optional noerror)
  "Register the local Discord MCP launcher with mcp.el.
The launcher is provided by Home Manager and runs the nix-built
discordmcp server.  The DISCORD_TOKEN env var is injected from
auth-source so the token never lives in the dotfiles repository.

With optional NOERROR non-nil, skip silently when the launcher or
token are unavailable instead of signaling a `user-error'."
  (interactive)
  (cond
   ((and (not noerror) (not (executable-find "discord-mcp-launcher")))
    (user-error "discord-mcp-launcher is not on exec-path; apply Home Manager first"))
   ((not (executable-find "discord-mcp-launcher"))
    (message "Discord MCP: launcher not found on exec-path; skipping"))
   (t
    (let ((token (+llm/discord-token)))
      (cond
       ((and token (not (string-empty-p token)))
        (setq mcp-hub-servers
              (cons `("discord" . (:command "discord-mcp-launcher"
                                    :env (:DISCORD_TOKEN ,token)))
                    (assoc-delete-all "discord" mcp-hub-servers)))
        (message "Discord MCP: registered discord-mcp-launcher"))
       (noerror
        (message "Discord MCP: no token in auth-source (host=%s user=%s); skipping"
                  +llm/discord-auth-host +llm/discord-auth-user))
       (t
        (user-error "No Discord token in auth-source (host=%s user=%s); add it to ~/.authinfo.gpg"
                    +llm/discord-auth-host +llm/discord-auth-user)))))))

;;;###autoload
(defun +llm/discord-connect ()
  "Register and connect the Discord MCP tools to gptel."
  (interactive)
  (require 'gptel-integrations)
  (require 'mcp-hub)
  (+llm/discord-register)
  (gptel-mcp-connect '("discord")))

;;;###autoload
(defun +llm/load ()
  "Load the complete local LLM configuration."
  (interactive)
  (require 'ai-init)
  (ai/llm-apply-defaults)
  (message "LLM stack loaded: %s / %s" ai/llm-provider ai/llm-model))

;;;###autoload
(defun +llm/chat ()
  "Open a persistent Org gptel agent chat."
  (interactive)
  (require 'chat)
  (call-interactively #'ai/chat))

;;;###autoload
(defun +llm/use-glm-5.2 (&optional local)
  "Switch to Z.AI GLM-5.2."
  (interactive "P")
  (require 'ai)
  (ai/llm-use-glm-5.2 local))

;;;###autoload
(defun +llm/use-gpt-5.6-sol (&optional local)
  "Switch to OpenAI API GPT-5.6 Sol."
  (interactive "P")
  (require 'ai)
  (ai/llm-use-gpt-5.6-sol local))

;;;###autoload
(defun +llm/use-openai-oauth (&optional local)
  "Switch to GPT-5.6 Sol through OpenAI subscription OAuth."
  (interactive "P")
  (require 'ai)
  (ai/llm-use-openai-oauth local))

;;;###autoload
(defun +llm/openai-login ()
  "Authenticate the OpenAI subscription backend."
  (interactive)
  (require 'ai)
  (ai/llm-openai-oauth-login))

;;;###autoload
(defun +llm/agent-context ()
  "Toggle project instruction files in gptel context."
  (interactive)
  (require 'ai-agent)
  (ai/agent-context-mode 'toggle))

(provide '+llm-autoloads)
;;; llm.el ends here
