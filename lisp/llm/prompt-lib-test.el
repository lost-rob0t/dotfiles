;;; prompt-lib-test.el --- Tests for prompt-lib -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
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

(ert-deftest ai/prompt-lib-parses-org-metadata-and-body ()
  (ai/prompt-lib-test--with-library
    (write-region
     "#+title: State worker\n#+prompt_id: engineering.state-worker\n#+description: Durable worker\n#+filetags: :engineering:worker:\n#+prompt_aliases: state worker | loop\n\n* Prompt\nMode {{MODE|REVIEW}}\n\n* Notes\nNot prompt text.\n"
     nil (expand-file-name "prompts/state-worker.org" ai/prompt-lib-directory)
     nil 'silent)
    (ai/prompt-lib-activate)
    (let ((record (car (ai/prompt-lib-records 'refresh))))
      (should (equal (plist-get record :id) "engineering.state-worker"))
      (should (equal (plist-get record :tags) '("engineering" "worker")))
      (should (equal (plist-get record :aliases) '("state worker" "loop")))
      (should (equal (plist-get record :body) "Mode {{MODE|REVIEW}}")))))

(ert-deftest ai/prompt-lib-external-directory-wins-over-legacy-fallback ()
  (ai/prompt-lib-test--with-library
    (let ((fallback (make-temp-file "prompt-lib-fallback-" t)))
      (unwind-protect
          (progn
            (setq ai/prompt-template-directory fallback)
            (ai/prompt-lib-activate)
            (should (equal ai/prompt-lib--active-directory
                           (expand-file-name "prompts/" ai/prompt-lib-directory))))
        (delete-directory fallback t)))))

(ert-deftest ai/prompt-lib-copy-updates-kill-ring-and-system-selection ()
  (let (selection)
    (cl-letf (((symbol-function 'gui-set-selection)
               (lambda (type text)
                 (setq selection (cons type text)))))
      (ai/prompt-lib--system-copy "hello")
      (should (equal (current-kill 0) "hello"))
      (should (equal selection '(CLIPBOARD . "hello"))))))

(ert-deftest ai/prompt-lib-template-compatibility-routes-through-records ()
  (ai/prompt-lib-test--with-library
    (write-region
     "#+title: Render test\n#+prompt_id: test.render\n\n* Prompt\nRepo {{REPO}}\n"
     nil (expand-file-name "prompts/render.org" ai/prompt-lib-directory)
     nil 'silent)
    (ai/prompt-lib-activate)
    (should (member "test.render" (ai/prompt-template-names)))
    (should (equal (ai/prompt-template--read "test.render") "Repo {{REPO}}"))))

(provide 'prompt-lib-test)
;;; prompt-lib-test.el ends here
