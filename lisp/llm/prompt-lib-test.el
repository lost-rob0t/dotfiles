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
          (ai/prompt-lib--active-directory nil))
     (unwind-protect
         (progn
           (make-directory ai/prompt-template-directory t)
           ,@body)
       (delete-directory root t))))

(ert-deftest ai/prompt-lib-catalog-loads-metadata ()
  (ai/prompt-lib-test--with-library
    (let ((prompt (expand-file-name "prompts/test.prompt" ai/prompt-lib-directory)))
      (write-region "Hello {{NAME}}" nil prompt nil 'silent)
      (write-region
       "{\"schema\":\"prompts-lib.catalog.v1\",\"prompts\":[{\"id\":\"test.hello\",\"title\":\"Hello\",\"description\":\"Test\",\"path\":\"prompts/test.prompt\",\"tags\":[\"test\"],\"aliases\":[\"hi\"]}]}"
       nil (ai/prompt-lib--catalog-path) nil 'silent)
      (let ((record (car (ai/prompt-lib-records 'refresh))))
        (should (equal (plist-get record :id) "test.hello"))
        (should (equal (plist-get record :title) "Hello"))
        (should (equal (plist-get record :tags) '("test")))
        (should (equal (plist-get record :aliases) '("hi")))))))

(ert-deftest ai/prompt-lib-discovers-uncataloged-prompts ()
  (ai/prompt-lib-test--with-library
    (write-region "Plain prompt" nil
                  (expand-file-name "prompts/plain.prompt" ai/prompt-lib-directory)
                  nil 'silent)
    (let ((record (car (ai/prompt-lib-records 'refresh))))
      (should (equal (plist-get record :id) "plain"))
      (should (equal (plist-get record :source) 'scan)))))

(ert-deftest ai/prompt-lib-rejects-catalog-path-escape ()
  (ai/prompt-lib-test--with-library
    (write-region
     "{\"schema\":\"prompts-lib.catalog.v1\",\"prompts\":[{\"id\":\"bad\",\"path\":\"../escape.prompt\"}]}"
     nil (ai/prompt-lib--catalog-path) nil 'silent)
    (should-error (ai/prompt-lib-records 'refresh) :type 'user-error)))

(ert-deftest ai/prompt-lib-activate-selects-external-prompts-directory ()
  (ai/prompt-lib-test--with-library
    (let ((fallback (make-temp-file "prompt-lib-fallback-" t)))
      (unwind-protect
          (progn
            (setq ai/prompt-template-directory fallback)
            (ai/prompt-lib-activate)
            (should (equal ai/prompt-template-directory
                           (expand-file-name "prompts/" ai/prompt-lib-directory))))
        (delete-directory fallback t)))))

(ert-deftest ai/prompt-lib-render-uses-existing-template-renderer ()
  (ai/prompt-lib-test--with-library
    (let ((template "Repo {{REPO}} task {{TASK|test}}"))
      (should (equal
               (ai/prompt-template-render-string
                template '(("REPO" . "lost-rob0t/test")))
               "Repo lost-rob0t/test task test")))))

(provide 'prompt-lib-test)
;;; prompt-lib-test.el ends here
