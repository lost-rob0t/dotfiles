;;; llm.el --- Doom autoload entrypoints for the local gptel stack -*- lexical-binding: t; -*-

;;;###autoload
(autoload #'ai/chat "chat" "Create and open a persistent Org agent chat." t)

;;;###autoload
(autoload #'ai/meme-generate "meme" "Generate a meme from a region or prompt." t)

;;;###autoload
(autoload #'ai/youtube-context "youtube-context" "Copy yt-dlp transcript context." t)

;;;###autoload
(autoload #'ai/prompt-menu "ai-prompts" "Open reusable prompt and image commands." t)

;;;###autoload
(autoload #'ai/image-generate "ai-image" "Generate an image with OpenAI's image tool." t)

;;;###autoload
(autoload #'ai/image-generate-template "ai-image" "Generate an image from a reusable prompt template." t)

;;;###autoload
(autoload #'ai/image-edit "ai-image" "Edit an image with OpenAI's image tool." t)

;;;###autoload
(autoload #'mara "mara" "Open the local Mara runtime." t)

;;;###autoload
(defun +llm/bind-keys ()
  "Bind the local LLM entrypoints after Doom finishes loading."
  (define-key doom-leader-map (kbd "y i") #'+llm/image-generate)
  (define-key doom-leader-map (kbd "y m") #'ai/meme-generate)
  (define-key doom-leader-map (kbd "y p") #'+llm/prompt-menu)
  (define-key doom-leader-map (kbd "y v") #'+llm/youtube-context)
  (define-key doom-leader-map (kbd "y y") #'ai/chat))

;;;###autoload
(add-hook 'doom-after-init-hook #'+llm/bind-keys)

;;;###autoload
(with-eval-after-load 'gptel
  ;; Run after gptel's package callbacks so the local defaults remain final.
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
(defun +llm/proxmox-register (&optional noerror)
  "Register the Proxmox MCP server."
  (interactive)
  (require 'ai-mcp)
  (ai/mcp-register-proxmox noerror))

;;;###autoload
(defun +llm/discord-register (&optional noerror)
  "Register the Discord MCP server."
  (interactive)
  (require 'ai-mcp)
  (ai/mcp-register-discord noerror))

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
  (call-interactively #'ai/chat))

;;;###autoload
(defun +llm/youtube-context ()
  "Fetch a video transcript with yt-dlp and copy it as LLM context."
  (interactive)
  (call-interactively #'ai/youtube-context))

(defalias '+llm/yt-dlp-context #'+llm/youtube-context)

;;;###autoload
(defun +llm/prompt-menu ()
  "Open the reusable prompt-template and image-generation menu."
  (interactive)
  (require 'ai-prompts)
  (ai/prompt-menu))

;;;###autoload
(defun +llm/image-generate ()
  "Generate an image from the active region or a minibuffer prompt."
  (interactive)
  (require 'ai-image)
  (call-interactively #'ai/image-generate))

;;;###autoload
(defun +llm/image-generate-template ()
  "Generate an image from a reusable prompt template."
  (interactive)
  (require 'ai-image)
  (call-interactively #'ai/image-generate-template))

;;;###autoload
(defun +llm/image-edit ()
  "Edit an image using OpenAI's image-generation tool."
  (interactive)
  (require 'ai-image)
  (call-interactively #'ai/image-edit))

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
