;;; ai-prolog-rlm-test.el --- Tests for PrologRLM gptel tool -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'ai-prolog-rlm)

(defun ai/prolog-rlm-test--parse (text)
  "Parse tool-result JSON TEXT."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object :json-false))

(ert-deftest ai/prolog-rlm-context-requires-exactly-one-source ()
  (should-error (ai/prolog-rlm--context nil nil))
  (should-error (ai/prolog-rlm--context "README.md" "inline")))

(ert-deftest ai/prolog-rlm-inline-context-preserves-unicode ()
  (let ((text "RLM → λ 日本語 🚀\nsecond line"))
    (should (equal (ai/prolog-rlm--context nil text) text))))

(ert-deftest ai/prolog-rlm-file-context-is-bounded-and-lossless ()
  (let* ((directory (make-temp-file "prolog-rlm-context-" t))
         (file (expand-file-name "context.md" directory))
         (body "# Context\n\n\"quotes\" \\slashes — λ\n"))
    (unwind-protect
        (progn
          (with-temp-file file (insert body))
          (let ((ai/agent-restrict-to-project nil)
                (ai/prolog-rlm-max-context-bytes 4096))
            (should (equal (ai/prolog-rlm--context file nil) body)))
          (let ((ai/agent-restrict-to-project nil)
                (ai/prolog-rlm-max-context-bytes 2))
            (should-error (ai/prolog-rlm--context file nil))))
      (delete-directory directory t))))

(ert-deftest ai/prolog-rlm-budget-is-host-bounded ()
  (let* ((ai/prolog-rlm-max-total-tokens 999999)
         (ai/prolog-rlm-max-cost-usd 99.0)
         (ai/prolog-rlm-time-limit 999.0)
         (budget (ai/prolog-rlm--budget)))
    (should (= (alist-get 'max_recursion_depth budget) 1))
    (should (= (alist-get 'max_tool_calls budget) 0))
    (should (= (alist-get 'max_total_tokens budget) 32768))
    (should (= (alist-get 'max_cost_usd budget) 1.0))
    (should (= (alist-get 'time_limit budget) 120.0))))

(ert-deftest ai/prolog-rlm-request-serializes-as-gptel-safe-text ()
  (let* ((root (make-temp-file "prolog-rlm-root-" t))
         (entry-dir (expand-file-name "prolog" root))
         (entry (expand-file-name "rlm.pl" entry-dir))
         (ai/prolog-rlm-root root)
         (ai/prolog-rlm-model "openrouter/auto"))
    (unwind-protect
        (progn
          (make-directory entry-dir t)
          (with-temp-file entry (insert "% fixture\n"))
          (let* ((request (ai/prolog-rlm--request "Find → λ" "opaque 日本語"))
                 (encoded (ai/agent--tool-result request))
                 (parsed (ai/prolog-rlm-test--parse encoded)))
            (should (multibyte-string-p encoded))
            (should (equal (alist-get 'operation parsed) "completion"))
            (should (equal (alist-get 'model parsed) "openrouter/auto"))
            (should (equal (alist-get 'query parsed) "Find → λ"))
            (should (equal (alist-get 'context parsed) "opaque 日本語"))))
      (delete-directory root t))))

(ert-deftest ai/prolog-rlm-bridge-output-normalizes-once ()
  (let* ((raw "{\"ok\":true,\"kind\":\"rlm_result\",\"result\":{\"value\":\"λ — ok\"}}")
         (result (ai/prolog-rlm--bridge-output raw))
         (parsed (ai/prolog-rlm-test--parse result)))
    (should (multibyte-string-p result))
    (should (eq (alist-get 'ok parsed) t))
    (should (equal (alist-get 'kind parsed) "rlm_result"))
    (should-not (string-prefix-p "\"{" result))))

(ert-deftest ai/prolog-rlm-configuration-failure-calls-back-once ()
  (let ((calls 0)
        result)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) nil)))
      (ai/prolog-rlm--start
       (lambda (value)
         (setq calls (1+ calls)
               result value))
       '((operation . "completion"))))
    (should (= calls 1))
    (should (eq (alist-get 'ok (ai/prolog-rlm-test--parse result))
                :json-false))))

(ert-deftest ai/prolog-rlm-registers-async-gptel-tool ()
  (ai/prolog-rlm-register-gptel-tool)
  (let ((tool (gptel-get-tool "PrologRLM")))
    (should tool)
    (should (gptel-tool-async tool))
    (should (member "PrologRLM" ai/agent-tools))))

(provide 'ai-prolog-rlm-test)
;;; ai-prolog-rlm-test.el ends here
