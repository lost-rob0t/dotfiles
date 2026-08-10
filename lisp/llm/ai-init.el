;;; ai-init.el --- Load the Emacs LLM stack -*- lexical-binding: t; -*-

(require 'ai)
(require 'ai-prompts)
(require 'ai-image)
(require 'meme)
(require 'ai-agent)
(require 'ai-image-tools)
(require 'ai-mcp)
(require 'chat)

(unless (assq 'openai/gpt-5.6-sol-pro:exacto ai/llm-openrouter-models)
  (push '(openai/gpt-5.6-sol-pro:exacto
          :description "GPT-5.6 Sol Pro through OpenRouter Exacto"
          :capabilities (reasoning media tool-use json url)
          :context-window 1050)
        ai/llm-openrouter-models))

(setq ai/llm-provider 'openrouter
      ai/llm-model 'openai/gpt-5.6-sol-pro:exacto)
(ai/llm-backend 'openrouter t)
(ai/llm-apply-defaults)

(ai/image-register-gptel-tools)
(unless (string-match-p "Image and prompt-template rules:" ai/agent-system-prompt)
  (setq ai/agent-system-prompt
        (concat ai/agent-system-prompt ai/image-agent-instructions)))

(gptel-make-preset 'gpt-5.6-sol-pro
  :description "GPT-5.6 Sol Pro through OpenRouter Exacto."
  :backend (ai/llm-backend 'openrouter)
  :model 'openai/gpt-5.6-sol-pro:exacto
  :stream t
  :include-reasoning 'ignore)

(gptel-make-preset 'agent
  :description "GPT-5.6 Sol Pro project agent through OpenRouter Exacto."
  :backend (ai/llm-backend 'openrouter)
  :model 'openai/gpt-5.6-sol-pro:exacto
  :system ai/agent-system-prompt
  :tools ai/agent-tools
  :stream t
  :temperature nil
  :use-context 'system
  :track-media t
  :include-reasoning t)

(gptel-make-preset 'agent-gpt-5.6-sol
  :parents '(agent)
  :description "GPT-5.6 Sol project agent through OpenRouter."
  :backend (ai/llm-backend 'openrouter)
  :model 'openai/gpt-5.6-sol)

(with-eval-after-load 'org-ql
  (require 'todo nil t))

(provide 'ai-init)
;;; ai-init.el ends here
