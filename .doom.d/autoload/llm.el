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
  (require 'ai-agent))

;;;###autoload
(with-eval-after-load 'mcp-hub
  (when (and (file-readable-p +llm/proxmox-config-file)
             (executable-find "proxmox-mcp-launcher"))
    (+llm/proxmox-register)))

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
  "Switch to OpenAI GPT-5.6 Sol."
  (interactive "P")
  (require 'ai)
  (ai/llm-use-gpt-5.6-sol local))

;;;###autoload
(defun +llm/agent-context ()
  "Toggle project instruction files in gptel context."
  (interactive)
  (require 'ai-agent)
  (ai/agent-context-mode 'toggle))

(provide '+llm-autoloads)
;;; llm.el ends here
