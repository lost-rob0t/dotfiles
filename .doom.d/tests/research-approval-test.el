;;; research-approval-test.el --- Tests for canonical research approval -*- lexical-binding: t; -*-

(require 'ert)
(require 'research-approval)

(defconst nsa/research-approval-test--pending
  "#+title: Example research\n#+status: RESEARCHED\n#+approval_schema: prolog-rlm.research-approval.v1\n#+approval_state: PENDING\n#+approval_actor: NONE\n#+approval_evidence: NONE\n#+approval_base_commit: NONE\n#+approval_base_blob: NONE\n#+approval_decided_at: NONE\n")

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
             (insert nsa/research-approval-test--pending))
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

(ert-deftest nsa/research-approval-canonical-pending-is-accepted ()
  (with-temp-buffer
    (insert nsa/research-approval-test--pending)
    (should-not
     (nsa/research-approval--validate-record "PENDING"))))

(ert-deftest nsa/research-approval-requires-canonical-order ()
  (with-temp-buffer
    (insert (string-replace
             "#+approval_schema: prolog-rlm.research-approval.v1\n"
             "#+approval_state: PENDING\n#+approval_schema: prolog-rlm.research-approval.v1\n"
             nsa/research-approval-test--pending))
    (should-error (nsa/research-approval--validate-record "PENDING")
                  :type 'user-error)))

(ert-deftest nsa/research-approval-rejects-legacy-and-checked-layouts ()
  (with-temp-buffer
    (insert (concat nsa/research-approval-test--pending
                    "#+approval: APPROVED\n"
                    "- [X] APPROVE\n"))
    (should-error (nsa/research-approval--validate-record "PENDING")
                  :type 'user-error)))

(ert-deftest nsa/research-approval-keeps-lifecycle-separate ()
  (with-temp-buffer
    (insert (string-replace
             "#+status: RESEARCHED" "#+status: DONE"
             nsa/research-approval-test--pending))
    (should-not (nsa/research-approval--validate-record "PENDING")))
  (with-temp-buffer
    (insert (string-replace
             "#+status: RESEARCHED" "#+status: APPROVED"
             nsa/research-approval-test--pending))
    (should-error (nsa/research-approval--validate-record "PENDING")
                  :type 'user-error)))

(ert-deftest nsa/research-approval-rejects-duplicate-fields ()
  (with-temp-buffer
    (insert (concat nsa/research-approval-test--pending
                    "#+approval_state: PENDING\n"))
    (should-error (nsa/research-approval--validate-record "PENDING")
                  :type 'user-error)))

(ert-deftest nsa/research-approval-limits-paths-to-research-records ()
  (should (nsa/research-approval--eligible-path-p
           "research/RLM-RESEARCH-010-example.org"))
  (should-not (nsa/research-approval--eligible-path-p "rage/219-example.org"))
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

(ert-deftest nsa/research-approval-verifies-clean-checkout-and-upstream ()
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

(ert-deftest nsa/research-approval-commits-and-pushes-only-canonical-fields ()
  (nsa/research-approval-test--with-repository
    (let ((bare (make-temp-file "research-approval-remote-" t))
          (buffer (find-file-noselect record))
          answers)
      (unwind-protect
          (progn
            (nsa/research-approval-test--git bare "init" "-q" "--bare")
            (nsa/research-approval-test--git
             directory "config"
             (format "url.file://%s.insteadOf" bare)
             "git@github.com:lost-rob0t/prolog-rlm.git")
            (nsa/research-approval-test--git
             directory "push" "-q" "origin" "HEAD:refs/heads/topic")
            (nsa/research-approval-test--git
             directory "update-ref" "refs/remotes/origin/topic" "HEAD")
            (setq answers '("Alice Example" "https://github.com/lost-rob0t/prolog-rlm/issues/1"))
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'read-string)
                         (lambda (prompt &rest _)
                           (ignore prompt)
                           (pop answers)))
                        ((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'display-buffer)
                         (lambda (displayed &rest _) displayed))
                        ;; The fixture uses a local bare remote; URL policy is
                        ;; covered independently by the GitHub URL test.
                        ((symbol-function 'nsa/research-approval--github-url-p)
                         (lambda (_url) t)))
                (nsa/research-approve-and-push))
              (should (string-match-p "^#\\+approval_state: APPROVED\n"
                                      (buffer-string)))
              (should (string-match-p
                       "^#\\+approval_base_commit: [0-9a-f][0-9a-f]*\n"
                       (buffer-string)))
              (let ((status (nsa/research-approval--git
                             directory "status" "--porcelain=v1")))
                (should (string-empty-p status)))
              (should (equal
                       (nsa/research-approval--git-lines
                        directory "diff-tree" "--no-commit-id" "--name-only" "-r" "HEAD")
                       (list "research/example.org"))))
            (should (zerop
                     (let ((default-directory bare))
                       (process-file "git" nil nil nil
                                    "show-ref" "--verify" "refs/heads/topic")))))
        (kill-buffer buffer)
        (delete-directory bare t)))))

(ert-deftest nsa/research-approval-refuses-dirty-and-detached-checkouts ()
  (nsa/research-approval-test--with-repository
    (let ((buffer (find-file-noselect record))
          (untracked (expand-file-name ".local-note" directory)))
      (unwind-protect
          (with-current-buffer buffer
            (with-temp-file untracked (insert "local\n"))
            (should-error (nsa/research-approval--context t) :type 'user-error)
            (delete-file untracked)
            (nsa/research-approval-test--git directory "checkout" "--detach" "-q")
            (should-error (nsa/research-approval--context t) :type 'user-error))
        (kill-buffer buffer)))))

(provide 'research-approval-test)
;;; research-approval-test.el ends here
