;;; ai-agent-tool-result-test.el --- Tool-result serialization tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'cl-lib)
(require 'gptel-request)
(require 'gptel-openai)

;; The agent core only needs these features to exist while its functions are
;; defined.  The tests exercise gptel-request and gptel-openai directly so they
;; do not need the full interactive gptel UI or the normal ai backend setup.
(unless (featurep 'gptel)
  (provide 'gptel))
(unless (featurep 'ai)
  (provide 'ai))

(require 'ai-agent-core)

(defun ai/agent-tool-result-test--parse (text)
  "Parse JSON TEXT using the same false/null conventions as the agent tools."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(defun ai/agent-tool-result-test--backend ()
  "Return an OpenRouter-shaped gptel OpenAI-compatible backend without I/O."
  (gptel-make-openai "Tool Result Test"
    :host "openrouter.invalid"
    :endpoint "/api/v1/chat/completions"
    :protocol "https"
    :header nil
    :key nil
    :models '(tool-result-test-model)
    :stream nil))

(defun ai/agent-tool-result-test--provider-cycle (result)
  "Feed tool RESULT through gptel's OpenAI request path and parse a reply.

Return a plist containing the provider DATA, encoded wire request, decoded tool
CONTENT and the parsed simulated continuation response."
  (let* ((backend (ai/agent-tool-result-test--backend))
         (gptel-backend backend)
         (gptel-model 'tool-result-test-model)
         (gptel-stream nil)
         (gptel-system-prompt nil)
         (gptel-use-tools nil)
         (gptel--request-params nil)
         (data (gptel--request-data
                backend
                (list (list :role "user" :content "Read README.md"))))
         (tool-call
          (list :id "call_read_1"
                :name "Read"
                :args '(:path "README.md")
                :result result)))
    (gptel--inject-prompt
     backend data
     '(:role "assistant"
       :content :null
       :tool_calls
       [(:id "call_read_1"
         :type "function"
         :function (:name "Read"
                    :arguments "{\"path\":\"README.md\"}"))]))
    (gptel--inject-prompt
     backend data
     (gptel--parse-tool-results backend (list tool-call)))
    (let* ((info (list :backend backend
                       :model 'tool-result-test-model
                       :stream nil
                       :data data))
           ;; This is the exact function named in the original crash.  It must
           ;; be able to serialize the second request without json-value-p.
           (curl-config (gptel-curl--get-config info "tool-result-test-uuid"))
           (wire-json
            (decode-coding-string (gptel--json-encode data) 'utf-8 t))
           (wire-value
            (json-parse-string wire-json
                               :object-type 'plist
                               :array-type 'array
                               :null-object :null
                               :false-object :json-false))
           (messages (plist-get wire-value :messages))
           (tool-message (aref messages (1- (length messages))))
           (content (plist-get tool-message :content))
           (continued
            (gptel--parse-response
             backend
             '(:choices [(:message (:content "continuation ok"))]
               :usage (:prompt_tokens 3 :completion_tokens 2))
             info)))
      (list :data data
            :curl-config curl-config
            :wire-json wire-json
            :content content
            :continued continued))))

(ert-deftest ai/agent-tool-result-reproduces-emacs30-unibyte-json-failure ()
  "Document the old double-serialization boundary that triggered the crash."
  (skip-unless (>= emacs-major-version 30))
  (let* ((legacy
          (json-serialize
           (ai/agent--object "ok" t "value" "Unicode — λ")
           :null-object nil
           :false-object :json-false))
         (outer (list :role "tool" :content legacy)))
    (should (stringp legacy))
    (should-not (multibyte-string-p legacy))
    (should-error
     (json-serialize outer :null-object :null :false-object :json-false)
     :type 'wrong-type-argument)))

(ert-deftest ai/agent-tool-result-simple ()
  (let* ((result (ai/agent--json "ok" t "value" "hello"))
         (parsed (ai/agent-tool-result-test--parse result)))
    (should (stringp result))
    (should (multibyte-string-p result))
    (should (eq (alist-get 'ok parsed) t))
    (should (equal (alist-get 'value parsed) "hello"))))

(ert-deftest ai/agent-tool-result-file-read-preserves-text ()
  (let* ((directory (make-temp-file "ai-agent-tool-result-" t))
         (file (expand-file-name "README.md" directory))
         (body (concat
                "# Markdown\n\n"
                "A \"quoted\" value and \\backslashes.\n"
                "```elisp\n(message \"hello\")\n```\n"
                "multiple\nlines\n"
                "Unicode: λ 日本語 — ready.\n")))
    (unwind-protect
        (progn
          (with-temp-file file (insert body))
          (let* ((default-directory directory)
                 (ai/agent-restrict-to-project nil)
                 (result (ai/agent-read-file file 1 300))
                 (parsed (ai/agent-tool-result-test--parse result)))
            (should (multibyte-string-p result))
            (should (eq (alist-get 'ok parsed) t))
            (should (equal (alist-get 'content parsed) body))))
      (delete-directory directory t))))

(ert-deftest ai/agent-tool-result-nested-structured-value ()
  (let* ((result
          (ai/agent--json
           "ok" t
           "meta" (ai/agent--object "start_line" 1 "end_line" 220)
           "items" ["a" "b" "c"]
           "empty" nil))
         (parsed (ai/agent-tool-result-test--parse result))
         (meta (alist-get 'meta parsed)))
    (should (eq (alist-get 'ok parsed) t))
    (should (= (alist-get 'start_line meta) 1))
    (should (= (alist-get 'end_line meta) 220))
    (should (equal (alist-get 'items parsed) '("a" "b" "c")))
    (should (null (alist-get 'empty parsed)))))

(ert-deftest ai/agent-tool-result-unicode ()
  (let* ((text "Zażółć gęślą jaźń — λ 日本語 🚀")
         (result (ai/agent--json "ok" t "value" text))
         (parsed (ai/agent-tool-result-test--parse result)))
    (should (multibyte-string-p result))
    (should (equal (alist-get 'value parsed) text))))

(ert-deftest ai/agent-tool-result-error-survives-provider-path ()
  (let* ((result (ai/agent--json-error "boom" "file not found — nope"))
         (cycle (ai/agent-tool-result-test--provider-cycle result))
         (content (plist-get cycle :content))
         (parsed (ai/agent-tool-result-test--parse content)))
    (should (stringp (plist-get cycle :curl-config)))
    (should (eq (alist-get 'ok parsed) :json-false))
    (should (equal (alist-get 'error parsed) "boom"))
    (should (equal (plist-get cycle :continued) "continuation ok"))))

(ert-deftest ai/agent-tool-result-no-double-encoding ()
  (let* ((result
          (ai/agent--json
           "ok" t
           "meta" (ai/agent--object "start_line" 1 "end_line" 220)
           "items" ["a" "b" "c"]
           "value" "Unicode — λ"))
         (cycle (ai/agent-tool-result-test--provider-cycle result))
         (content (plist-get cycle :content))
         (parsed (ai/agent-tool-result-test--parse content)))
    ;; OpenAI-compatible chat completions defines tool result `content' as text.
    ;; The outer request therefore quotes it exactly once.  Parsing the outer
    ;; request must recover the original JSON text, not a quoted JSON string.
    (should (equal content result))
    (should (string-prefix-p "{" content))
    (should-not (string-prefix-p "\"{" content))
    (should (eq (alist-get 'ok parsed) t))
    (should (equal (alist-get 'items parsed) '("a" "b" "c")))))

(ert-deftest ai/agent-tool-result-model-continuation-end-to-end ()
  (let* ((directory (make-temp-file "ai-agent-tool-continuation-" t))
         (file (expand-file-name "README.md" directory)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# prolog-rlm\n\nModel → tool → model — continue.\n"))
          (let* ((default-directory directory)
                 (ai/agent-restrict-to-project nil)
                 ;; Execute the real Read implementation, then drive its result
                 ;; through gptel's tool-result/provider continuation path.
                 (result (ai/agent-read-file file 1 300))
                 (cycle (ai/agent-tool-result-test--provider-cycle result)))
            (should (stringp (plist-get cycle :curl-config)))
            (should (equal (plist-get cycle :content) result))
            (should (equal (plist-get cycle :continued) "continuation ok"))))
      (delete-directory directory t))))

(ert-deftest ai/agent-tool-result-large-content ()
  (let* ((body (concat (make-string 100000 ?x) "\nUnicode tail — λ 日本語\n"))
         (result (ai/agent--json "ok" t "content" body))
         (cycle (ai/agent-tool-result-test--provider-cycle result))
         (parsed (ai/agent-tool-result-test--parse (plist-get cycle :content))))
    (should (equal (alist-get 'content parsed) body))
    (should (equal (plist-get cycle :continued) "continuation ok"))))

(provide 'ai-agent-tool-result-test)
;;; ai-agent-tool-result-test.el ends here
