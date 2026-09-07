;;; prompt-lib-test.el --- Tests for prompt-lib -*- lexical-binding: t; -*-

(require 'ert)
(require 'prompt-lib)

(defmacro ai/prompt-lib-test--with-library (&rest body)
  "Run BODY with an isolated temporary prompt library."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "prompt-lib-test-" t))
          (ai/prompt-lib-directory (file-name-as-directory root))
          (ai/prompt-template-directory (expand-file-name "prompts/" root))
          (ai/prompt-lib--records-cache nil)
          (ai/prompt-lib--active-directory nil)
          (ai/prompt-lib-formats '(org markdown lisp legacy)))
     (unwind-protect
         (progn
           (make-directory ai/prompt-template-directory t)
           ,@body)
       (delete-directory root t))))

(ert-deftest ai/prompt-lib-parses-org-metadata-and-body ()
  (ai/prompt-lib-test--with-library
    (write-region
     "#+title: Hello prompt\n#+prompt_id: test.hello\n#+description: Test prompt\n#+filetags: :test:org:\n#+prompt_aliases: hi | hello\n\n* Prompt\nHello {{NAME}}\n\n* Notes\nNot prompt text.\n"
     nil (expand-file-name "prompts/hello.org" ai/prompt-lib-directory) nil 'silent)
    (let ((record (car (ai/prompt-lib-records 'refresh))))
      (should (equal (plist-get record :id) "test.hello"))
      (should (equal (plist-get record :title) "Hello prompt"))
      (should (equal (plist-get record :description) "Test prompt"))
      (should (equal (plist-get record :tags) '("test" "org")))
      (should (equal (plist-get record :aliases) '("hi" "hello")))
      (should (equal (plist-get record :body) "Hello {{NAME}}"))
      (should (eq (plist-get record :format) 'org)))))

(ert-deftest ai/prompt-lib-parses-markdown-adapter ()
  (ai/prompt-lib-test--with-library
    (write-region
     "---\nprompt_id: test.markdown\ntitle: Markdown prompt\ndescription: Markdown adapter\ntags: test, markdown\naliases: md | markdown test\n---\n\n# Prompt\nHello {{NAME}}\n\n# Notes\nNot prompt text.\n"
     nil (expand-file-name "prompts/markdown.md" ai/prompt-lib-directory) nil 'silent)
    (let ((record (car (ai/prompt-lib-records 'refresh))))
      (should (equal (plist-get record :id) "test.markdown"))
      (should (equal (plist-get record :tags) '("test" "markdown")))
      (should (equal (plist-get record :aliases) '("md" "markdown test")))
      (should (equal (plist-get record :body) "Hello {{NAME}}"))
      (should (eq (plist-get record :format) 'markdown)))))

(ert-deftest ai/prompt-lib-parses-lisp-as-data-without-eval ()
  (ai/prompt-lib-test--with-library
    (let ((ai/prompt-lib-test--evaluated nil))
      (write-region
       "(:id \"test.lisp\" :title \"Lisp prompt\" :description \"Lisp adapter\" :tags (\"test\" \"lisp\") :aliases (\"el\") :prompt \"Hello {{NAME}}\")\n(setq ai/prompt-lib-test--evaluated t)\n"
       nil (expand-file-name "prompts/lisp.el" ai/prompt-lib-directory) nil 'silent)
      (let ((record (car (ai/prompt-lib-records 'refresh))))
        (should-not ai/prompt-lib-test--evaluated)
        (should (equal (plist-get record :id) "test.lisp"))
        (should (equal (plist-get record :tags) '("test" "lisp")))
        (should (equal (plist-get record :body) "Hello {{NAME}}"))
        (should (eq (plist-get record :format) 'lisp))))))

(ert-deftest ai/prompt-lib-keeps-legacy-raw-prompt-fallback ()
  (ai/prompt-lib-test--with-library
    (write-region "Plain {{PROMPT}}" nil
                  (expand-file-name "prompts/plain.prompt" ai/prompt-lib-directory)
                  nil 'silent)
    (let ((record (car (ai/prompt-lib-records 'refresh))))
      (should (equal (plist-get record :id) "plain"))
      (should (equal (plist-get record :body) "Plain {{PROMPT}}"))
      (should (eq (plist-get record :format) 'legacy)))))

(ert-deftest ai/prompt-lib-activate-selects-external-prompts-directory ()
  (ai/prompt-lib-test--with-library
    (let ((fallback (make-temp-file "prompt-lib-fallback-" t)))
      (unwind-protect
          (progn
            (setq ai/prompt-template-directory fallback)
            (ai/prompt-lib-activate)
            (should (equal ai/prompt-lib--active-directory
                           (expand-file-name "prompts/" ai/prompt-lib-directory)))
            (should (equal ai/prompt-template-directory
                           (expand-file-name "prompts/" ai/prompt-lib-directory))))
        (delete-directory fallback t)))))

(ert-deftest ai/prompt-lib-new-defaults-to-org-mode ()
  (ai/prompt-lib-test--with-library
    (let ((ai/prompt-lib-default-format 'org)
          buffer)
      (unwind-protect
          (progn
            (ai/prompt-lib-activate)
            (setq buffer (ai/prompt-lib-new "Hello Prompt" 'org))
            (should (string-suffix-p ".org" buffer-file-name))
            (should (eq major-mode 'org-mode))
            (goto-char (point-min))
            (should (search-forward "#+prompt_id: hello-prompt" nil t))
            (should (search-forward "* Prompt" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ai/prompt-lib-template-compatibility-uses-normalized-record ()
  (ai/prompt-lib-test--with-library
    (write-region
     "#+title: Render test\n#+prompt_id: test.render\n\n* Prompt\nRepo {{REPO}} task {{TASK|test}}\n"
     nil (expand-file-name "prompts/render.org" ai/prompt-lib-directory) nil 'silent)
    (ai/prompt-lib-activate)
    (should (member "test.render" (ai/prompt-template-names)))
    (should (equal (ai/prompt-template--read "test.render")
                   "Repo {{REPO}} task {{TASK|test}}"))
    (should (equal
             (ai/prompt-template-render-string
              (ai/prompt-template--read "test.render")
              '(("REPO" . "lost-rob0t/test")))
             "Repo lost-rob0t/test task test"))))

(provide 'prompt-lib-test)
;;; prompt-lib-test.el ends here
