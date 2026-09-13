;;; ai-starintel.el --- StarIntel gateway backend for gptel -*- lexical-binding: t; -*-

(require 'ai)
(require 'cl-lib)
(require 'gptel)

(defcustom ai/llm-starintel-host "llm.starintel.actor"
  "Host name of the StarIntel OpenAI-compatible LLM gateway."
  :type 'string
  :group 'ai/llm)

(defcustom ai/llm-starintel-endpoint "/v1/chat/completions"
  "Chat-completions endpoint exposed by the StarIntel LLM gateway."
  :type 'string
  :group 'ai/llm)

(defcustom ai/llm-starintel-models
  '((qwen38-27b
     :description "Qwen 3.8 27B through llm.starintel.actor"
     :capabilities (reasoning tool-use json)))
  "Models advertised by the StarIntel LLM gateway backend.
Add gateway model IDs here as they are deployed."
  :type '(repeat sexp)
  :group 'ai/llm)

(defvar ai/llm-starintel--backend nil
  "Cached gptel backend for llm.starintel.actor.")

(defun ai/llm-starintel--api-key ()
  "Return the StarIntel gateway API key without persisting it in dotfiles."
  (or (getenv "STARINTEL_LLM_API_KEY")
      (getenv "LLM_STARINTEL_API_KEY")
      (and (fboundp 'nsa/auth-source-get)
           (ignore-errors
             (nsa/auth-source-get :host ai/llm-starintel-host)))
      (ai/llm--auth-source-secret ai/llm-starintel-host)))

(defun ai/llm-starintel--require-api-key ()
  "Return the StarIntel gateway API key or signal a useful error."
  (or (ai/llm-starintel--api-key)
      (user-error
       "No StarIntel LLM key; set STARINTEL_LLM_API_KEY or auth-source host %s"
       ai/llm-starintel-host)))

(cl-defun ai/llm-starintel-backend (&key (stream t) (name "StarIntel LLM"))
  "Return the OpenAI-compatible gptel backend for llm.starintel.actor."
  (gptel-make-openai name
    :host (or (getenv "STARINTEL_LLM_HOST") ai/llm-starintel-host)
    :endpoint (or (getenv "STARINTEL_LLM_ENDPOINT")
                  ai/llm-starintel-endpoint)
    :protocol "https"
    :stream stream
    :key #'ai/llm-starintel--require-api-key
    :models ai/llm-starintel-models))

(defun ai/llm-starintel--backend-object (&optional refresh)
  "Return the cached StarIntel backend, rebuilding it when REFRESH is non-nil."
  (when refresh
    (setq ai/llm-starintel--backend nil))
  (or ai/llm-starintel--backend
      (setq ai/llm-starintel--backend (ai/llm-starintel-backend))))

(defun ai/llm-starintel--backend-around (original provider &optional refresh)
  "Extend ORIGINAL `ai/llm-backend' with the StarIntel PROVIDER."
  (if (eq provider 'starintel)
      (ai/llm-starintel--backend-object refresh)
    (funcall original provider refresh)))

(defun ai/llm-starintel--models-around (original &optional provider)
  "Extend ORIGINAL provider model lookup with StarIntel models."
  (if (eq (or provider ai/llm-provider) 'starintel)
      (mapcar #'ai/llm--model-name ai/llm-starintel-models)
    (funcall original provider)))

(unless (advice-member-p #'ai/llm-starintel--backend-around 'ai/llm-backend)
  (advice-add 'ai/llm-backend :around #'ai/llm-starintel--backend-around))

(unless (advice-member-p #'ai/llm-starintel--models-around
                         'ai/llm-models-for-provider)
  (advice-add 'ai/llm-models-for-provider
              :around #'ai/llm-starintel--models-around))

(defun ai/llm-use-starintel (&optional model local)
  "Use MODEL through llm.starintel.actor.
With LOCAL non-nil, only change the current buffer."
  (interactive
   (let* ((models (ai/llm-models-for-provider 'starintel))
          (default (or (and (memq ai/llm-model models) ai/llm-model)
                       (car models)))
          (model (intern
                  (completing-read "StarIntel model: "
                                   (mapcar #'symbol-name models)
                                   nil t nil nil
                                   (and default (symbol-name default))))))
          (local current-prefix-arg))
     (list model local)))
  (ai/llm-use 'starintel
              (or model (car (ai/llm-models-for-provider 'starintel)))
              local))

(gptel-make-preset 'starintel
  :description "StarIntel OpenAI-compatible gateway."
  :backend (ai/llm-starintel--backend-object)
  :model 'qwen38-27b
  :stream t
  :include-reasoning 'ignore)

(provide 'ai-starintel)
;;; ai-starintel.el ends here
