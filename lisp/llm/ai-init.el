;;; ai-init.el --- Load the Emacs LLM stack -*- lexical-binding: t; -*-

(require 'ai)
(require 'ai-prompts)
(require 'ai-image)
(require 'meme)
(require 'ai-agent)
(require 'ai-image-tools)
(require 'ai-mcp)
(require 'chat)
(require 'fren-loader)

(defconst ai/default-openrouter-image-model
  (or (getenv "OPENROUTER_IMAGE_MODEL")
      "bytedance-seed/seedream-4.5")
  "OpenRouter image model used by image and meme generation.")

(setq ai/image-model ai/default-openrouter-image-model
      ai/meme-model ai/default-openrouter-image-model)

(unless (assq 'openrouter/auto ai/llm-openrouter-models)
  (push '(openrouter/auto
          :description "OpenRouter Auto Router in pure-quality mode"
          :capabilities (reasoning media tool-use json url)
          :context-window 2000
          :request-params (:plugins [(:id "auto-router"
                                      :cost_quality_tradeoff 0)]))
        ai/llm-openrouter-models))

(unless (assq 'anthropic/claude-opus-5:exacto ai/llm-openrouter-models)
  (push '(anthropic/claude-opus-5:exacto
          :description "Claude Opus 5 through OpenRouter Exacto"
          :capabilities (reasoning media tool-use json)
          :context-window 1000
          :request-params (:reasoning (:effort "max")))
        ai/llm-openrouter-models))

(setq ai/llm-provider 'openrouter
      ai/llm-model 'openrouter/auto)
(ai/llm-backend 'openrouter t)
(ai/llm-apply-defaults)

(ai/image-register-gptel-tools)
(unless (string-match-p "Image and prompt-template rules:" ai/agent-system-prompt)
  (setq ai/agent-system-prompt
        (concat ai/agent-system-prompt ai/image-agent-instructions)))
(setq ai/chat-system-prompt ai/agent-system-prompt)

(gptel-make-preset 'best
  :description "OpenRouter Auto Router with cost-quality tradeoff pinned to pure quality."
  :backend (ai/llm-backend 'openrouter)
  :model 'openrouter/auto
  :stream t
  :request-params '(:plugins [(:id "auto-router"
                               :cost_quality_tradeoff 0)])
  :include-reasoning 'ignore)

(gptel-make-preset 'claude-opus-5
  :description "Claude Opus 5 through OpenRouter Exacto at max reasoning effort."
  :backend (ai/llm-backend 'openrouter)
  :model 'anthropic/claude-opus-5:exacto
  :stream t
  :request-params '(:reasoning (:effort "max"))
  :include-reasoning 'ignore)

(gptel-make-preset 'agent
  :description "Pure-quality OpenRouter Auto Router project agent."
  :backend (ai/llm-backend 'openrouter)
  :model 'openrouter/auto
  :system ai/agent-system-prompt
  :tools ai/agent-tools
  :stream t
  :temperature nil
  :request-params '(:plugins [(:id "auto-router"
                               :cost_quality_tradeoff 0)])
  :use-context 'system
  :track-media t
  :include-reasoning t)

(gptel-make-preset 'agent-claude-opus-5
  :parents '(agent)
  :description "Claude Opus 5 project agent through OpenRouter Exacto."
  :model 'anthropic/claude-opus-5:exacto
  :request-params '(:reasoning (:effort "max")))

(gptel-make-preset 'agent-gpt-5.6-sol
  :parents '(agent)
  :description "GPT-5.6 Sol project agent through OpenRouter."
  :model 'openai/gpt-5.6-sol
  :request-params nil)

(with-eval-after-load 'org-ql
  (require 'todo nil t))

(provide 'ai-init)
;;; ai-init.el ends here
