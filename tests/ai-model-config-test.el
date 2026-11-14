;;; ai-model-config-test.el --- LLM model retirement regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst ai-model-config-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun ai-model-config-test--read (path)
  (with-temp-buffer
    (insert-file-contents (expand-file-name path ai-model-config-test--root))
    (buffer-string)))

(ert-deftest ai-model-config-uses-luna-and-drops-retiring-mini ()
  (let ((ai-source (ai-model-config-test--read "lisp/llm/ai.el"))
        (init-source (ai-model-config-test--read "lisp/llm/ai-init.el")))
    (should (string-match-p "(openai/gpt-5\\.6-luna" ai-source))
    (should (string-match-p "ai/llm-model 'openai/gpt-5\\.6-luna" ai-source))
    (should (string-match-p "ai/llm-model 'openai/gpt-5\\.6-luna" init-source))
    (should-not (string-match-p "openai/gpt-5-mini" ai-source))
    (should (string-match-p "(gptel-make-preset 'gpt-5\\.6-luna" ai-source))
    ;; The direct OpenAI model registry stays on the unaffected GPT-5.6 IDs.
    (should (string-match-p "'(gpt-5\\.6-sol gpt-5\\.6-terra gpt-5\\.6-luna)" ai-source))))

(provide 'ai-model-config-test)
;;; ai-model-config-test.el ends here
