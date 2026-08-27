;;; research-approval-test.el --- Tests for research approval  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'research-approval)

(defun nsa/research-approval-test--git (directory &rest arguments)
  (let ((default-directory (file-name-as-directory directory)))
    (should (zerop (apply #'process-file "git" nil nil nil arguments)))))

(defmacro nsa/research-approval-test--with-repository (&rest body)
  (declare (indent 0) (debug t))
  `(let ((directory (make-temp-file "research-approval-test-" t)))
     (unwind-protect
         (let ((record (expand-file-name "research/example.org" directory)))
           (make-directory (file-name-directory record) t)
           (with-temp-file record
             (insert "#+title: Example\n#+status: RESEARCHED\n"))
           (nsa/research-approval-test--git directory "init" "-q" "-b" "topic")
           (nsa/research-approval-test--git
            directory "config" "user.name" "Research Test")
           (nsa/research-approval-test--git
            directory "config" "user.email" "test@example.invalid")
           (nsa/research-approval-test--git directory "add" "research/example.org")
           (nsa/research-approval-test--git directory "commit" "-q" "-m" "fixture")
           (nsa/research-approval-test--git
            directory "remote" "add" "origin"
            "git@github.com:lost-rob0t/prolog-rlm.git")
           (nsa/research-approval-test--git
            directory "update-ref" "refs/remotes/origin/topic" "HEAD")
           (nsa/research-approval-test--git
            directory "config" "branch.topic.remote" "origin")
           (nsa/research-approval-test--git
            directory "config" "branch.topic.merge" "refs/heads/topic")
           ,@body)
       (delete-directory directory t))))

(ert-deftest nsa/research-approval-adds-explicit-keywords ()
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: example\n:END:\n"
            "#+title: Example research\n"
            "#+status: RESEARCHED\n\n"
            "* Finding\n")
    (nsa/research-approval--insert-marker "[2026-08-27 Thu 12:00 -0400]")
    (should
     (equal (buffer-string)
            (concat ":PROPERTIES:\n:ID: example\n:END:\n"
                    "#+title: Example research\n"
                    "#+status: RESEARCHED\n"
                    "#+approval: APPROVED\n"
                    "#+approved_at: [2026-08-27 Thu 12:00 -0400]\n\n"
                    "* Finding\n")))))

(ert-deftest nsa/research-approval-refuses-existing-marker ()
  (with-temp-buffer
    (org-mode)
    (insert "#+title: Example\n#+approval: APPROVED\n")
    (should-error
     (nsa/research-approval--insert-marker "[2026-08-27 Thu 12:00 -0400]")
     :type 'user-error)))

(ert-deftest nsa/research-approval-limits-paths-to-research-records ()
  (should (nsa/research-approval--eligible-path-p
           "research/RLM-RESEARCH-010-example.org"))
  (should (nsa/research-approval--eligible-path-p
           "rage/219-retrieval-expert.org"))
  (should-not (nsa/research-approval--eligible-path-p "README.org"))
  (should-not (nsa/research-approval--eligible-path-p "research/notes.txt"))
  (should-not (nsa/research-approval--eligible-path-p "../research/example.org")))

(ert-deftest nsa/research-approval-recognizes-only-github-push-urls ()
  (dolist (url '("git@github.com:lost-rob0t/prolog-rlm.git"
                 "ssh://git@github.com/lost-rob0t/prolog-rlm.git"
                 "https://github.com/lost-rob0t/prolog-rlm.git"))
    (should (nsa/research-approval--github-url-p url)))
  (dolist (url '("https://gitlab.com/lost-rob0t/prolog-rlm.git"
                 "file:///tmp/prolog-rlm.git"
                 "github.com/lost-rob0t/prolog-rlm"))
    (should-not (nsa/research-approval--github-url-p url))))

(ert-deftest nsa/research-approval-verifies-checkout-and-upstream ()
  (nsa/research-approval-test--with-repository
    (let ((buffer (find-file-noselect record)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((context (nsa/research-approval--context t)))
              (should (equal (plist-get context :path) "research/example.org"))
              (should (equal (plist-get context :branch) "topic"))
              (should (equal (plist-get context :upstream) "origin/topic"))
              (should (equal (plist-get context :merge-ref) "refs/heads/topic"))))
        (kill-buffer buffer)))))

(ert-deftest nsa/research-approval-refuses-unrelated-dirt-and-detached-head ()
  (nsa/research-approval-test--with-repository
    (let ((buffer (find-file-noselect record))
          (untracked (expand-file-name ".env" directory)))
      (unwind-protect
          (with-current-buffer buffer
            (with-temp-file untracked (insert "SECRET=test\n"))
            (should-error (nsa/research-approval--context t) :type 'user-error)
            (delete-file untracked)
            (nsa/research-approval-test--git directory "checkout" "--detach" "-q")
            (should-error (nsa/research-approval--context t) :type 'user-error))
        (kill-buffer buffer)))))

(provide 'research-approval-test)
;;; research-approval-test.el ends here
