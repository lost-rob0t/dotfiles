;;; ai.el --- Shared LLM backend configuration -*- lexical-binding: t; -*-

(require 'gptel)
(require 'cl-lib)

(defgroup ai/llm nil
  "Shared LLM configuration for custom workflows."
  :group 'applications)

(defcustom ai/llm-model 'moonshotai/kimi-k2.5
  "Default model used by custom gptel workflows."
  :type 'symbol
  :group 'ai/llm)

(defcustom ai/llm-openrouter-models
  '(moonshotai/kimi-k2.5
    openai/gpt-5-mini
    openai/gpt-5.2-codex)
  "Model list advertised to the OpenRouter backend."
  :type '(repeat symbol)
  :group 'ai/llm)

(cl-defun ai/llm-openrouter-backend (&key (stream t) (name "OpenRouter"))
  "Return an OpenRouter backend.
STREAM controls response streaming. NAME sets backend display name."
  (gptel-make-openai name
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream stream
    :key #'(lambda () (nsa/auth-source-get :host "openrouter.ai"))
    :models ai/llm-openrouter-models))

(defun ai/llm-resolve-model (&optional model)
  "Return MODEL when provided, otherwise `ai/llm-model'."
  (or model ai/llm-model))

(provide 'ai)

;;; ai.el ends here
