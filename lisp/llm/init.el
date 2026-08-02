;;; init.el --- Load the Emacs LLM stack -*- lexical-binding: t; -*-

(require 'ai)
(require 'ai-agent)
(require 'ai-mcp)
(require 'chat)

;; `agent.el' historically used direct provider backends.  Re-register the
;; public presets after it loads so all normal agent traffic stays on OpenRouter.
(gptel-make-preset 'agent
  :description "GLM-5.2 project agent through OpenRouter."
  :backend (ai/llm-backend 'openrouter)
  :model 'z-ai/glm-5.2
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
;;; init.el ends here
