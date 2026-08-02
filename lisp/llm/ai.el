;;; ai.el --- Shared modern gptel backend configuration -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-anthropic)
(require 'gptel-openai-extras)
(require 'subr-x)

(defgroup ai/llm nil
  "Shared gptel configuration for interactive and agent workflows."
  :group 'applications
  :prefix "ai/llm-")

(defcustom ai/llm-provider 'openrouter
  "Default LLM provider.
OpenRouter is authoritative.  Direct provider backends remain available only
for explicit manual selection."
  :type '(choice (const :tag "OpenRouter" openrouter)
                 (const :tag "OpenAI subscription OAuth" openai-oauth)
                 (const :tag "Z.AI API" zai)
                 (const :tag "OpenAI API" openai)
                 (const :tag "Anthropic API" anthropic))
  :group 'ai/llm)

(defcustom ai/llm-model 'z-ai/glm-5.2
  "Default OpenRouter model used by gptel and custom workflows."
  :type 'symbol
  :group 'ai/llm)

(defcustom ai/llm-zai-host "api.z.ai"
  "Z.AI API host for explicit direct-provider use."
  :type 'string
  :group 'ai/llm)

(defcustom ai/llm-zai-endpoint "/api/paas/v4/chat/completions"
  "Z.AI chat-completions endpoint for explicit direct-provider use."
  :type 'string
  :group 'ai/llm)

(defcustom ai/llm-zai-models
  '((glm-5.2
     :description "Z.AI flagship model for long-horizon coding and agent work"
     :capabilities (reasoning tool-use json)
     :context-window 1000
     :request-params (:thinking (:type "enabled")))
    (|glm-5.2[1m]|
     :description "GLM-5.2 Coding Plan model with one-million-token context"
     :capabilities (reasoning tool-use json)
     :context-window 1000
     :request-params (:thinking (:type "enabled")))
    (glm-5.1
     :capabilities (reasoning tool-use json)
     :context-window 200
     :request-params (:thinking (:type "enabled")))
    (glm-5
     :capabilities (reasoning tool-use json)
     :context-window 200
     :request-params (:thinking (:type "enabled"))))
  "Models advertised by the optional direct Z.AI backend."
  :type '(repeat sexp)
  :group 'ai/llm)

(defcustom ai/llm-openai-models
  '(gpt-5.6-sol gpt-5.6-terra gpt-5.6-luna)
  "OpenAI model IDs exposed for explicit direct-provider use."
  :type '(repeat symbol)
  :group 'ai/llm)

(defcustom ai/llm-anthropic-models
  '(claude-fable-5 claude-sonnet-5 claude-opus-4-8)
  "Anthropic model IDs exposed for explicit direct-provider use."
  :type '(repeat symbol)
  :group 'ai/llm)

(defcustom ai/llm-openrouter-models
  '((z-ai/glm-5.2
     :description "GLM-5.2 through OpenRouter"
     :capabilities (reasoning tool-use json)
     :context-window 1000)
    (openai/gpt-5.6-sol
     :description "GPT-5.6 Sol through OpenRouter"
     :capabilities (reasoning media tool-use json url)
     :context-window 1050)
    (openai/gpt-5.6-terra
     :description "GPT-5.6 Terra through OpenRouter"
     :capabilities (reasoning media tool-use json url)
     :context-window 1050)
    (openai/gpt-5.6-luna
     :description "GPT-5.6 Luna through OpenRouter"
     :capabilities (media tool-use json url)
     :context-window 1050)
    (anthropic/claude-fable-5
     :description "Claude Fable 5 through OpenRouter"
     :capabilities (media tool-use json)
     :context-window 1000)
    (openai/gpt-5.2-codex
     :capabilities (reasoning media tool-use json url))
    (openai/gpt-5-mini
     :capabilities (reasoning media tool-use json url))
    (moonshotai/kimi-k2.5
     :capabilities (reasoning tool-use json)))
  "Provider-qualified model IDs advertised by the OpenRouter backend."
  :type '(repeat sexp)
  :group 'ai/llm)

(defvar ai/llm--backends (make-hash-table :test #'eq)
  "Cached gptel backend objects keyed by provider symbol.")

(defun ai/llm--auth-source-secret (host)
  "Return a secret from auth-source for HOST, or nil."
  (when-let* ((match (car (auth-source-search :host host :max 1
                                               :require '(:secret))))
              (secret (plist-get match :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun ai/llm--api-key (provider)
  "Return the API key for PROVIDER.
Environment variables are preferred, then auth-source is consulted."
  (pcase provider
    ('zai
     (or (getenv "ZAI_API_KEY")
         (getenv "ZHIPUAI_API_KEY")
         (and (fboundp 'nsa/auth-source-get)
              (ignore-errors (nsa/auth-source-get :host ai/llm-zai-host)))
         (ai/llm--auth-source-secret ai/llm-zai-host)))
    ('openai
     (or (getenv "OPENAI_API_KEY")
         (and (fboundp 'nsa/auth-source-get)
              (ignore-errors (nsa/auth-source-get :host "api.openai.com")))
         (ai/llm--auth-source-secret "api.openai.com")))
    ('anthropic
     (or (getenv "ANTHROPIC_API_KEY")
         (and (fboundp 'nsa/auth-source-get)
              (ignore-errors (nsa/auth-source-get :host "api.anthropic.com")))
         (ai/llm--auth-source-secret "api.anthropic.com")))
    ('openrouter
     (or (getenv "OPENROUTER_API_KEY")
         (and (fboundp 'nsa/auth-source-get)
              (ignore-errors (nsa/auth-source-get :host "openrouter.ai")))
         (ai/llm--auth-source-secret "openrouter.ai")))
    (_ (error "Unsupported API-key LLM provider: %S" provider))))

(defun ai/llm--require-api-key (provider)
  "Return PROVIDER's API key or signal a useful error."
  (or (ai/llm--api-key provider)
      (user-error
       "No API key for %s; configure auth-source or its environment variable"
       provider)))

(cl-defun ai/llm-zai-backend (&key (stream t) (name "Z.AI"))
  "Return the optional direct Z.AI backend."
  (gptel-make-deepseek name
    :host ai/llm-zai-host
    :endpoint (or (getenv "ZAI_API_ENDPOINT") ai/llm-zai-endpoint)
    :stream stream
    :key (lambda () (ai/llm--require-api-key 'zai))
    :models ai/llm-zai-models))

(cl-defun ai/llm-openai-backend (&key (stream t) (name "OpenAI"))
  "Return the optional direct OpenAI Responses API backend."
  (gptel-make-openai name
    :stream stream
    :key (lambda () (ai/llm--require-api-key 'openai))))

(cl-defun ai/llm-openai-oauth-backend
    (&key (stream t) (name "OpenAI Subscription"))
  "Return gptel's OpenAI subscription OAuth backend."
  (require 'gptel-openai-oauth)
  (gptel-make-openai-oauth name :stream stream))

(cl-defun ai/llm-anthropic-backend (&key (stream t) (name "Anthropic"))
  "Return the optional direct Anthropic Messages API backend."
  (gptel-make-anthropic name
    :stream stream
    :key (lambda () (ai/llm--require-api-key 'anthropic))))

(cl-defun ai/llm-openrouter-backend (&key (stream t) (name "OpenRouter"))
  "Return the authoritative OpenRouter chat-completions backend."
  (gptel-make-openai name
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream stream
    :key (lambda () (ai/llm--require-api-key 'openrouter))
    :models ai/llm-openrouter-models))

(defun ai/llm-backend (&optional provider refresh)
  "Return the backend for PROVIDER.
PROVIDER defaults to `ai/llm-provider'.  When REFRESH is non-nil, rebuild it."
  (let ((provider (or provider ai/llm-provider)))
    (when refresh
      (remhash provider ai/llm--backends))
    (or (gethash provider ai/llm--backends)
        (puthash provider
                 (pcase provider
                   ('openrouter (ai/llm-openrouter-backend))
                   ('openai-oauth (ai/llm-openai-oauth-backend))
                   ('zai (ai/llm-zai-backend))
                   ('openai (ai/llm-openai-backend))
                   ('anthropic (ai/llm-anthropic-backend))
                   (_ (error "Unsupported LLM provider: %S" provider)))
                 ai/llm--backends))))

(defun ai/llm-resolve-model (&optional model)
  "Return MODEL when provided, otherwise `ai/llm-model'."
  (or model ai/llm-model))

(defun ai/llm--model-name (spec)
  "Return the model symbol represented by SPEC."
  (if (consp spec) (car spec) spec))

(defun ai/llm-models-for-provider (&optional provider)
  "Return configured model symbols for PROVIDER."
  (pcase (or provider ai/llm-provider)
    ('openrouter (mapcar #'ai/llm--model-name ai/llm-openrouter-models))
    ('zai (mapcar #'ai/llm--model-name ai/llm-zai-models))
    ((or 'openai 'openai-oauth) ai/llm-openai-models)
    ('anthropic ai/llm-anthropic-models)
    (_ nil)))

(defun ai/llm-use (provider model &optional local)
  "Switch gptel to PROVIDER and MODEL.
With LOCAL non-nil, only change the current buffer."
  (interactive
   (let* ((provider
           (intern
            (completing-read
             "Provider: "
             '("openrouter" "openai-oauth" "zai" "openai" "anthropic")
             nil t nil nil (symbol-name ai/llm-provider))))
          (available (ai/llm-models-for-provider provider))
          (default (if (memq ai/llm-model available)
                       ai/llm-model
                     (car available)))
          (model
           (intern
            (completing-read "Model: " (mapcar #'symbol-name available)
                             nil t nil nil (symbol-name default)))))
     (list provider model current-prefix-arg)))
  (let ((backend (ai/llm-backend provider)))
    (if local
        (progn
          (setq-local gptel-backend backend)
          (setq-local gptel-model model))
      (setq ai/llm-provider provider
            ai/llm-model model
            gptel-backend backend
            gptel-model model)))
  (message "gptel: %s / %s%s"
           provider model (if local " (buffer-local)" "")))

(defun ai/llm-use-glm-5.2 (&optional local)
  "Use GLM-5.2 through OpenRouter."
  (interactive "P")
  (ai/llm-use 'openrouter 'z-ai/glm-5.2 local))

(defun ai/llm-use-gpt-5.6-sol (&optional local)
  "Use GPT-5.6 Sol through OpenRouter."
  (interactive "P")
  (ai/llm-use 'openrouter 'openai/gpt-5.6-sol local))

(defun ai/llm-use-gpt-luna (&optional local)
  "Use GPT-5.6 Luna through OpenRouter."
  (interactive "P")
  (ai/llm-use 'openrouter 'openai/gpt-5.6-luna local))

(defun ai/llm-use-fable (&optional local)
  "Use Claude Fable 5 through OpenRouter."
  (interactive "P")
  (ai/llm-use 'openrouter 'anthropic/claude-fable-5 local))

(defun ai/llm-use-openai-oauth (&optional local)
  "Use GPT-5.6 Sol through an OpenAI subscription OAuth session."
  (interactive "P")
  (ai/llm-use 'openai-oauth 'gpt-5.6-sol local))

(defun ai/llm-openai-oauth-login ()
  "Authenticate gptel's OpenAI subscription backend."
  (interactive)
  (require 'gptel-openai-oauth)
  (gptel-openai-oauth-login (ai/llm-backend 'openai-oauth)))

(defun ai/llm-apply-defaults ()
  "Apply shared defaults for current gptel."
  (setq gptel-backend (ai/llm-backend ai/llm-provider)
        gptel-model ai/llm-model
        gptel-default-mode 'org-mode
        gptel-stream t
        gptel-temperature nil
        gptel-use-curl t
        gptel-use-tools t
        gptel-confirm-tool-calls 'auto
        gptel-include-tool-results 'auto
        gptel-use-context 'system
        gptel-context-restrict-to-project-files t
        gptel-include-reasoning 'ignore
        gptel-track-media t
        gptel-track-response t
        gptel-cache '(system tool)
        gptel-org-convert-response t
        gptel-org-branching-context t
        gptel-use-header-line t))

(ai/llm-apply-defaults)

(gptel-make-preset 'glm-5.2
  :description "GLM-5.2 through OpenRouter with tool use."
  :backend (ai/llm-backend 'openrouter)
  :model 'z-ai/glm-5.2
  :stream t
  :include-reasoning 'ignore)

(gptel-make-preset 'gpt-5.6-sol
  :description "GPT-5.6 Sol through OpenRouter."
  :backend (ai/llm-backend 'openrouter)
  :model 'openai/gpt-5.6-sol
  :stream t
  :include-reasoning 'ignore)

(gptel-make-preset 'gpt-5.6-luna
  :description "GPT-5.6 Luna through OpenRouter."
  :backend (ai/llm-backend 'openrouter)
  :model 'openai/gpt-5.6-luna
  :stream t
  :include-reasoning 'ignore)

(gptel-make-preset 'claude-fable-5
  :description "Claude Fable 5 through OpenRouter."
  :backend (ai/llm-backend 'openrouter)
  :model 'anthropic/claude-fable-5
  :stream t
  :include-reasoning 'ignore)

(gptel-make-preset 'gpt-5.6-sol-oauth
  :description "GPT-5.6 Sol through OpenAI subscription OAuth."
  :backend (ai/llm-backend 'openai-oauth)
  :model 'gpt-5.6-sol
  :stream t
  :request-params '(:reasoning (:effort "high" :summary "auto"))
  :include-reasoning 'ignore)

(provide 'ai)
;;; ai.el ends here
