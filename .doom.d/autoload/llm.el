;;; llm.el --- Doom autoload entrypoints for the local gptel stack -*- lexical-binding: t; -*-

;;;###autoload
(with-eval-after-load 'gptel
  ;; Load real implementations from `lisp/llm' instead of scheduling a
  ;; function whose autoload points back into this compiled autoload file.
  (run-at-time
   0 nil
   (lambda ()
     (require 'ai-init)
     (ai/llm-apply-defaults))))

;;;###autoload
(defun +llm/load ()
  "Load the complete local LLM configuration."
  (interactive)
  (require 'ai-init)
  (ai/llm-apply-defaults)
  (ai/mcp-connect-all 'noerror)
  (message "LLM stack loaded: %s / %s" ai/llm-provider ai/llm-model))

;;;###autoload
(defun +llm/mcp-connect-all (&optional noerror)
  "Start configured MCP servers and register their tools with gptel."
  (interactive)
  (require 'ai-mcp)
  (ai/mcp-connect-all noerror))

;;;###autoload
(defun +llm/proxmox-connect ()
  "Connect the Proxmox MCP server."
  (interactive)
  (require 'ai-mcp)
  (ai/mcp-connect-proxmox))

;;;###autoload
(defun +llm/discord-connect ()
  "Connect the Discord MCP server."
  (interactive)
  (require 'ai-mcp)
  (ai/mcp-connect-discord))

;;;###autoload
(defun +llm/chat ()
  "Open a persistent Org gptel agent chat."
  (interactive)
  (require 'chat)
  (call-interactively #'ai/chat))

;;;###autoload
(defun +llm/use-glm-5.2 (&optional local)
  "Switch to GLM-5.2 through OpenRouter."
  (interactive "P")
  (require 'ai)
  (ai/llm-use-glm-5.2 local))

;;;###autoload
(defun +llm/use-gpt-5.6-sol (&optional local)
  "Switch to GPT-5.6 Sol through OpenRouter."
  (interactive "P")
  (require 'ai)
  (ai/llm-use-gpt-5.6-sol local))

;;;###autoload
(defun +llm/use-gpt-luna (&optional local)
  "Switch to GPT-5.6 Luna through OpenRouter."
  (interactive "P")
  (require 'ai)
  (ai/llm-use-gpt-luna local))

;;;###autoload
(defun +llm/use-fable (&optional local)
  "Switch to Claude Fable 5 through OpenRouter."
  (interactive "P")
  (require 'ai)
  (ai/llm-use-fable local))

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
