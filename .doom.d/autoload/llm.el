;;; llm.el --- Doom autoloads for the local gptel stack -*- lexical-binding: t; -*-

;;;###autoload
(with-eval-after-load 'gptel
  (require 'ai)
  (require 'ai-agent))

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
