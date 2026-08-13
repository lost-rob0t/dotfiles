;;; ai-prolog-rlm.el --- Async Prolog RLM tool for gptel -*- lexical-binding: t; -*-

(require 'ai)
(require 'ai-agent-core)
(require 'cl-lib)
(require 'gptel)
(require 'json)
(require 'subr-x)

(defgroup ai/prolog-rlm nil
  "gptel integration for the local prolog-rlm runtime."
  :group 'ai/agent
  :prefix "ai/prolog-rlm-")

(defcustom ai/prolog-rlm-root
  (expand-file-name "~/Documents/Projects/prolog-rlm")
  "Checkout containing prolog-rlm's public `prolog/rlm.pl' entrypoint."
  :type 'directory
  :group 'ai/prolog-rlm)

(defcustom ai/prolog-rlm-model "openrouter/auto"
  "OpenRouter model used by the inner Prolog RLM runtime."
  :type 'string
  :group 'ai/prolog-rlm)

(defcustom ai/prolog-rlm-max-context-bytes (* 8 1024 1024)
  "Maximum UTF-8 file payload accepted by one PrologRLM tool call."
  :type 'integer
  :group 'ai/prolog-rlm)

(defcustom ai/prolog-rlm-max-total-tokens 8192
  "Requested aggregate token ceiling for one Prolog RLM completion.
The trusted Prolog bridge applies its own hard upper bound as well."
  :type 'integer
  :group 'ai/prolog-rlm)

(defcustom ai/prolog-rlm-max-cost-usd 0.25
  "Requested aggregate dollar ceiling for one Prolog RLM completion.
The trusted Prolog bridge applies its own hard upper bound as well."
  :type 'number
  :group 'ai/prolog-rlm)

(defcustom ai/prolog-rlm-time-limit 60.0
  "Requested wall-time ceiling in seconds for one Prolog RLM completion."
  :type 'number
  :group 'ai/prolog-rlm)

(defcustom ai/prolog-rlm-host-timeout-slack 15.0
  "Seconds the Emacs host allows beyond the Prolog runtime time limit."
  :type 'number
  :group 'ai/prolog-rlm)

(defcustom ai/prolog-rlm-max-result-bytes (* 512 1024)
  "Maximum JSON response accepted from the Prolog bridge."
  :type 'integer
  :group 'ai/prolog-rlm)

(defconst ai/prolog-rlm--module-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the Prolog RLM Emacs integration files.")

(defconst ai/prolog-rlm--bridge-file
  (expand-file-name "prolog-rlm-bridge.pl" ai/prolog-rlm--module-directory)
  "Trusted JSON/stdin bridge between Emacs and prolog-rlm.")

(defconst ai/prolog-rlm-agent-instructions
  "\n\nProlog RLM rules:\n- Use PrologRLM for large-context analysis or tasks where bounded search, slicing, decomposition, or recursive subcalls are useful.\n- Do not use PrologRLM for trivial questions that the current model can answer directly.\n- When the source is a local project file, pass its path directly to PrologRLM instead of calling Read first and copying the full file into the conversation.\n- Supply exactly one of path or context. Use context only when the relevant text is already available without another file read.\n- Treat PrologRLM failures as structured tool failures; do not invent a result when it reports an error."
  "System-prompt fragment describing appropriate PrologRLM use.")

(defun ai/prolog-rlm--error-result (kind message &optional details)
  "Return a canonical gptel tool result for KIND, MESSAGE and DETAILS."
  (ai/agent--tool-result
   (ai/agent--object
    "ok" :json-false
    "kind" kind
    "error" message
    "details" details)))

(defun ai/prolog-rlm--root ()
  "Return the validated prolog-rlm checkout root."
  (let* ((root (file-name-as-directory (expand-file-name ai/prolog-rlm-root)))
         (entrypoint (expand-file-name "prolog/rlm.pl" root)))
    (unless (file-readable-p entrypoint)
      (error "prolog-rlm entrypoint is not readable: %s" entrypoint))
    root))

(defun ai/prolog-rlm--read-context-file (path)
  "Read PATH as bounded text using the agent's filesystem policy."
  (let* ((file (ai/agent--resolve-path path))
         (attributes (file-attributes file 'string)))
    (unless attributes
      (error "Context file does not exist: %s" path))
    (unless (file-regular-p file)
      (error "Context path is not a regular file: %s" path))
    (unless (file-readable-p file)
      (error "Context file is not readable: %s" path))
    (let ((bytes (file-attribute-size attributes)))
      (when (> bytes ai/prolog-rlm-max-context-bytes)
        (error "Context file is %d bytes; limit is %d"
               bytes ai/prolog-rlm-max-context-bytes)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (when (string-match-p "\0" text)
          (error "Context file appears to be binary: %s" path))
        text))))

(defun ai/prolog-rlm--context (path context)
  "Resolve exactly one of PATH or inline CONTEXT to text."
  (let ((has-path (and (stringp path) (not (string-empty-p path))))
        (has-context (and (stringp context) (not (string-empty-p context)))))
    (cond
     ((and has-path has-context)
      (error "PrologRLM accepts exactly one of path or context"))
     (has-path (ai/prolog-rlm--read-context-file path))
     (has-context
      (when (> (string-bytes context) ai/prolog-rlm-max-context-bytes)
        (error "Inline context exceeds %d bytes" ai/prolog-rlm-max-context-bytes))
      context)
     (t (error "PrologRLM requires exactly one of path or context")))))

(defun ai/prolog-rlm--budget ()
  "Return the host-requested RLM budget as JSON-compatible data."
  (ai/agent--object
   "max_iterations" 32
   "max_recursion_depth" 1
   "max_concurrent_subcalls" 2
   "max_model_calls" 4
   "max_tool_calls" 0
   "max_context_ops" 8
   "max_total_tokens" (max 256 (min 32768 ai/prolog-rlm-max-total-tokens))
   "max_cost_usd" (max 0.0 (min 1.0 ai/prolog-rlm-max-cost-usd))
   "max_output_bytes" 65536
   "time_limit" (max 1.0 (min 120.0 ai/prolog-rlm-time-limit))))

(defun ai/prolog-rlm--request (query context)
  "Build the trusted bridge request for QUERY over CONTEXT."
  (unless (and (stringp query) (not (string-empty-p (string-trim query))))
    (error "PrologRLM query must be non-empty"))
  (unless (and (stringp ai/prolog-rlm-model)
               (not (string-empty-p ai/prolog-rlm-model)))
    (error "ai/prolog-rlm-model must be non-empty"))
  (ai/agent--object
   "operation" "completion"
   "root" (ai/prolog-rlm--root)
   "query" query
   "context" context
   "model" ai/prolog-rlm-model
   "budget" (ai/prolog-rlm--budget)))

(defun ai/prolog-rlm--stderr-text (buffer)
  "Return bounded stderr text from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (car (ai/agent--truncate
            (buffer-substring-no-properties (point-min) (point-max))
            16384)))))

(defun ai/prolog-rlm--bridge-output (raw)
  "Validate RAW bridge JSON and return canonical gptel result text."
  (when (> (string-bytes raw) ai/prolog-rlm-max-result-bytes)
    (error "Prolog RLM response exceeded %d bytes"
           ai/prolog-rlm-max-result-bytes))
  (let ((parsed
         (json-parse-string
          (string-trim raw)
          :object-type 'alist
          :array-type 'array
          :null-object nil
          :false-object :json-false)))
    (ai/agent--tool-result parsed)))

(defun ai/prolog-rlm--start (callback request)
  "Run REQUEST through the trusted SWI bridge and invoke CALLBACK once."
  (unless (executable-find "swipl")
    (funcall callback
             (ai/prolog-rlm--error-result
              "configuration_error" "swipl is not on exec-path"))
    (cl-return-from ai/prolog-rlm--start nil))
  (unless (file-readable-p ai/prolog-rlm--bridge-file)
    (funcall callback
             (ai/prolog-rlm--error-result
              "configuration_error"
              (format "Prolog RLM bridge is not readable: %s"
                      ai/prolog-rlm--bridge-file)))
    (cl-return-from ai/prolog-rlm--start nil))
  (let ((key (ai/llm--api-key 'openrouter)))
    (unless (and (stringp key) (not (string-empty-p key)))
      (funcall callback
               (ai/prolog-rlm--error-result
                "configuration_error"
                "No OpenRouter credential is available to PrologRLM"))
      (cl-return-from ai/prolog-rlm--start nil))
    (let* ((stdout (generate-new-buffer " *prolog-rlm-out*"))
           (stderr (generate-new-buffer " *prolog-rlm-err*"))
           (request-text (ai/agent--tool-result request))
           (finished nil)
           (timer nil)
           (process nil)
           (host-timeout (+ (max 1.0 (min 120.0 ai/prolog-rlm-time-limit))
                            (max 0.0 ai/prolog-rlm-host-timeout-slack))))
      (cl-labels
          ((cleanup ()
             (when (timerp timer)
               (cancel-timer timer))
             (when (buffer-live-p stdout)
               (kill-buffer stdout))
             (when (buffer-live-p stderr)
               (kill-buffer stderr)))
           (finish (result)
             (unless finished
               (setq finished t)
               (unwind-protect
                   (funcall callback result)
                 (cleanup))))
           (fail (kind message &optional details)
             (finish (ai/prolog-rlm--error-result kind message details)))
           (sentinel (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((status (process-exit-status proc))
                     (errtext (ai/prolog-rlm--stderr-text stderr)))
                 (if (not (zerop status))
                     (fail "process_error"
                           (format "Prolog RLM bridge exited with status %d" status)
                           errtext)
                   (condition-case err
                       (let ((raw
                              (when (buffer-live-p stdout)
                                (with-current-buffer stdout
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))
                         (if (string-empty-p (or (string-trim (or raw "")) ""))
                             (fail "protocol_error"
                                   "Prolog RLM bridge returned no JSON"
                                   errtext)
                           (finish (ai/prolog-rlm--bridge-output raw))))
                     (error
                      (fail "protocol_error"
                            (error-message-string err)
                            errtext))))))))
        (let ((process-environment (copy-sequence process-environment)))
          (setenv "OPENROUTER_API_KEY" key)
          (setq process
                (make-process
                 :name (generate-new-buffer-name "prolog-rlm")
                 :buffer stdout
                 :stderr stderr
                 :command (list (executable-find "swipl")
                                "-q" "-s" ai/prolog-rlm--bridge-file)
                 :coding 'utf-8-unix
                 :connection-type 'pipe
                 :noquery t
                 :sentinel #'sentinel)))
        (setq timer
              (run-at-time
               host-timeout nil
               (lambda ()
                 (unless finished
                   (setq finished t)
                   (when (process-live-p process)
                     (delete-process process))
                   (unwind-protect
                       (funcall callback
                                (ai/prolog-rlm--error-result
                                 "host_timeout"
                                 (format "PrologRLM exceeded %.1f seconds"
                                         host-timeout)))
                     (cleanup))))))
        (process-send-string process request-text)
        (process-send-eof process)
        process))))

(defun ai/prolog-rlm-chat (callback query &optional path context)
  "Run QUERY through prolog-rlm over PATH or inline CONTEXT.
CALLBACK is supplied first by gptel because this is an asynchronous tool."
  (condition-case err
      (let* ((resolved-context (ai/prolog-rlm--context path context))
             (request (ai/prolog-rlm--request query resolved-context)))
        (ai/prolog-rlm--start callback request))
    (error
     (funcall callback
              (ai/prolog-rlm--error-result
               "input_error" (error-message-string err))))))

(defun ai/prolog-rlm-register-gptel-tool ()
  "Register the asynchronous PrologRLM tool and add it to agent presets."
  (when (fboundp 'gptel-get-tool)
    (ignore-errors (setf (gptel-get-tool "PrologRLM") nil)))
  (gptel-make-tool
   :name "PrologRLM"
   :function #'ai/prolog-rlm-chat
   :category "reasoning"
   :description
   "Run a bounded Recursive Language Model in SWI-Prolog over a large local file or inline text. The RLM can inspect/search/slice/partition opaque context and make bounded OpenRouter subcalls without exposing the full context to the outer model."
   :args '((:name "query"
            :type string
            :description "Question or task the Prolog RLM should solve")
           (:name "path"
            :type string
            :optional t
            :description "Local context file. Prefer this for large project files; do not Read it first")
           (:name "context"
            :type string
            :optional t
            :description "Inline context text; mutually exclusive with path"))
   :async t
   :include t)
  (when (boundp 'ai/agent-tools)
    (cl-pushnew "PrologRLM" ai/agent-tools :test #'equal))
  "PrologRLM")

(provide 'ai-prolog-rlm)
;;; ai-prolog-rlm.el ends here
