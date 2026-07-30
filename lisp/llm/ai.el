;;; ai.el --- Shared modern gptel backend configuration -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'cl-lib)
(require 'gptel)
(require 'subr-x)

(defgroup ai/llm nil
  "Shared gptel configuration for interactive and agent workflows."
  :group 'applications
  :prefix "ai/llm-")

(defcustom ai/llm-provider 'zai
  "Default LLM provider.
Supported values are `zai', `openai', `openai-oauth', and `openrouter'."
  :type '(choice (const :tag "Z.AI API" zai)
                 (const :tag "OpenAI API" openai)
                 (const :tag "OpenAI subscription OAuth" openai-oauth)
                 (const :tag "OpenRouter" openrouter))
  :group 'ai/llm)

(defcustom ai/llm-model 'glm-5.2
  "Default model used by gptel and custom workflows."
  :type 'symbol
  :group 'ai/llm)

(defcustom ai/llm-zai-host "api.z.ai"
  "Z.AI API host."
  :type 'string
  :group 'ai/llm)

(defcustom ai/llm-zai-endpoint "/api/paas/v4/chat/completions"
  "OpenAI-compatible Z.AI chat-completions endpoint."
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
  "Models advertised by the Z.AI backend."
  :type '(repeat sexp)
  :group 'ai/llm)

(defcustom ai/llm-openai-models
  '(gpt-5.6-sol gpt-5.6-terra gpt-5.6-luna)
  "OpenAI model IDs exposed by the local model switcher.
Metadata is supplied by current gptel instead of being duplicated here."
  :type '(repeat symbol)
  :group 'ai/llm)

(defcustom ai/llm-openrouter-models
  '((z-ai/glm-5.2
     :capabilities (reasoning tool-use json)
     :context-window 1000)
    (openai/gpt-5.6-sol
     :capabilities (reasoning media tool-use json url)
     :context-window 1050)
    (openai/gpt-5.6-terra
     :capabilities (reasoning media tool-use json url)
     :context-window 1050)
    (openai/gpt-5.6-luna
     :capabilities (reasoning media tool-use json url)
     :context-window 1050))
  "Models advertised by the OpenRouter backend."
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
  "Return a Z.AI OpenAI-compatible backend.
STREAM controls response streaming and NAME is the backend display name."
  (gptel-make-openai name
    :host ai/llm-zai-host
    :endpoint (or (getenv "ZAI_API_ENDPOINT") ai/llm-zai-endpoint)
    :stream stream
    :key (lambda () (ai/llm--require-api-key 'zai))
    :models ai/llm-zai-models))

(cl-defun ai/llm-openai-backend (&key (stream t) (name "OpenAI"))
  "Return the current gptel OpenAI Responses API backend."
  (gptel-make-openai name
    :stream stream
    :key (lambda () (ai/llm--require-api-key 'openai))))

(cl-defun ai/llm-openai-oauth-backend
    (&key (stream t) (name "OpenAI Subscription"))
  "Return gptel's OpenAI subscription OAuth backend."
  (require 'gptel-openai-oauth)
  (gptel-make-openai-oauth name :stream stream))

(cl-defun ai/llm-openrouter-backend (&key (stream t) (name "OpenRouter"))
  "Return an OpenRouter chat-completions backend."
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
                   ('zai (ai/llm-zai-backend))
                   ('openai (ai/llm-openai-backend))
                   ('openai-oauth (ai/llm-openai-oauth-backend))
                   ('openrouter (ai/llm-openrouter-backend))
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
    ('zai (mapcar #'ai/llm--model-name ai/llm-zai-models))
    ((or 'openai 'openai-oauth) ai/llm-openai-models)
    ('openrouter (mapcar #'ai/llm--model-name ai/llm-openrouter-models))
    (_ nil)))

(defun ai/llm-use (provider model &optional local)
  "Switch gptel to PROVIDER and MODEL.
With LOCAL non-nil, only change the current buffer."
  (interactive
   (let* ((provider
           (intern
            (completing-read
             "Provider: " '("zai" "openai" "openai-oauth" "openrouter")
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
  "Use GLM-5.2 through Z.AI.
With prefix argument LOCAL, apply only in the current buffer."
  (interactive "P")
  (ai/llm-use 'zai 'glm-5.2 local))

(defun ai/llm-use-gpt-5.6-sol (&optional local)
  "Use GPT-5.6 Sol through the OpenAI API.
With prefix argument LOCAL, apply only in the current buffer."
  (interactive "P")
  (ai/llm-use 'openai 'gpt-5.6-sol local))

(defun ai/llm-use-openai-oauth (&optional local)
  "Use GPT-5.6 Sol through an OpenAI subscription OAuth session.
With prefix argument LOCAL, apply only in the current buffer."
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
        gptel-use-curl t
        gptel-use-tools t
        gptel-confirm-tool-calls 'auto
        gptel-include-tool-results 'auto
        gptel-use-context 'system
        gptel-include-reasoning 'ignore
        gptel-track-response t
        gptel-org-convert-response t
        gptel-org-branching-context t
        gptel-use-header-line t))

(ai/llm-apply-defaults)

(gptel-make-preset 'glm-5.2
  :description "GLM-5.2 through Z.AI with thinking and tool use."
  :backend (ai/llm-backend 'zai)
  :model 'glm-5.2
  :stream t
  :include-reasoning 'ignore)

(gptel-make-preset 'gpt-5.6-sol
  :description "GPT-5.6 Sol through the OpenAI Responses API."
  :backend (ai/llm-backend 'openai)
  :model 'gpt-5.6-sol
  :stream t
  :request-params '(:reasoning (:effort "high" :summary "auto"))
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
