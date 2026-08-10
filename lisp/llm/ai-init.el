;;; ai-init.el --- Load the Emacs LLM stack -*- lexical-binding: t; -*-

(require 'ai)
(require 'ai-prompts)
(require 'ai-image)
(require 'meme)
(require 'ai-agent)
(require 'ai-image-tools)
(require 'ai-mcp)
(require 'chat)

(unless (assq 'anthropic/claude-opus-5:exacto ai/llm-openrouter-models)
  (push '(anthropic/claude-opus-5:exacto
          :description "Claude Opus 5 through OpenRouter Exacto"
          :capabilities (reasoning media tool-use json)
          :context-window 1000
          :request-params (:reasoning (:effort "max")))
        ai/llm-openrouter-models))

(setq ai/llm-provider 'openrouter
      ai/llm-model 'anthropic/claude-opus-5:exacto)
(ai/llm-backend 'openrouter t)
(ai/llm-apply-defaults)

(ai/image-register-gptel-tools)
(unless (string-match-p "Image and prompt-template rules:" ai/agent-system-prompt)
  (setq ai/agent-system-prompt
        (concat ai/agent-system-prompt ai/image-agent-instructions)))
(setq ai/chat-system-prompt ai/agent-system-prompt)

(gptel-make-preset 'claude-opus-5
  :description "Claude Opus 5 through OpenRouter Exacto at max reasoning effort."
  :backend (ai/llm-backend 'openrouter)
  :model 'anthropic/claude-opus-5:exacto
  :stream t
  :request-params '(:reasoning (:effort "max"))
  :include-reasoning 'ignore)

(gptel-make-preset 'agent
  :description "Claude Opus 5 project agent through OpenRouter Exacto."
  :backend (ai/llm-backend 'openrouter)
  :model 'anthropic/claude-opus-5:exacto
  :system ai/agent-system-prompt
  :tools ai/agent-tools
  :stream t
  :temperature nil
  :request-params '(:reasoning (:effort "max"))
  :use-context 'system
  :track-media t
  :include-reasoning t)

(gptel-make-preset 'agent-gpt-5.6-sol
  :parents '(agent)
  :description "GPT-5.6 Sol project agent through OpenRouter."
  :backend (ai/llm-backend 'openrouter)
  :model 'openai/gpt-5.6-sol
  :request-params nil)

(with-eval-after-load 'org-ql
  (require 'todo nil t))

(provide 'ai-init)
;;; ai-init.el ends here
