;;; research-dashboard-test.el --- Tests for research dashboard -*- lexical-binding: t; -*-

(require 'ert)
(require 'research-dashboard)

(defconst nsa/research-dashboard-test--canonical
  "#+title: Canonical research\n#+status: RESEARCHED\n#+approval_schema: prolog-rlm.research-approval.v1\n#+approval_state: PENDING\n#+approval_actor: NONE\n#+approval_evidence: NONE\n#+approval_base_commit: NONE\n#+approval_base_blob: NONE\n#+approval_decided_at: NONE\n\n* Findings\nBody\n")

(defconst nsa/research-dashboard-test--legacy-review
  "#+title: Legacy StarIntel research\n#+status: REVIEW\n#+filetags: :research:\n\n* Findings\nBody\n")

(defun nsa/research-dashboard-test--legacy-with-status (status)
  (string-replace "#+status: REVIEW"
                  (format "#+status: %s" status)
                  nsa/research-dashboard-test--legacy-review))

(ert-deftest nsa/research-dashboard-recognizes-only-review-artifact-paths ()
  (dolist (path '("research/example.org"
                  "roam/research/star-server/example.org"
                  "nested/research/topic/example.org"))
    (should (nsa/research-dashboard--candidate-path-p path)))
  (dolist (path '("roam/indexes/research.org"
                  "docs/research-notes.org"
                  "research/example.md"
                  "../research/example.org"
                  "books/prolog/research/index.org"
                  "books/prolog/research/sources.org"
                  "books/prolog/research/search-log.org"))
    (should-not (nsa/research-dashboard--candidate-path-p path))))

(ert-deftest nsa/research-dashboard-canonical-pending-is-open-even-when-lifecycle-done ()
  (let ((item (nsa/research-dashboard--item
               "lost-rob0t/prolog-rlm" "main" "research/example.org"
               "deadbeef"
               (string-replace "#+status: RESEARCHED" "#+status: DONE"
                               nsa/research-dashboard-test--canonical))))
    (should item)
    (should (equal (nsa/research-item-approval item) "PENDING"))
    (should (equal (nsa/research-item-lifecycle item) "DONE"))))

(ert-deftest nsa/research-dashboard-unmigrated-review-ready-is-retained ()
  (let ((item (nsa/research-dashboard--item
               "lost-rob0t/starintel-auto-research" "main"
               "roam/research/star-server/example.org" "deadbeef"
               nsa/research-dashboard-test--legacy-review)))
    (should item)
    (should (equal (nsa/research-item-approval item) "LEGACY"))))

(ert-deftest nsa/research-dashboard-unmigrated-nonreview-states-are-not-queued ()
  (dolist (status '("DRAFT" "RESEARCHING" "APPROVED" "DONE" "REJECTED"
                    "SUPERSEDED" "implemented-prototype" "accepted-for-realization"))
    (should-not
     (nsa/research-dashboard--item
      "lost-rob0t/starintel-auto-research" "main"
      "roam/research/star-server/example.org" "deadbeef"
      (nsa/research-dashboard-test--legacy-with-status status))))
  (should-not
   (nsa/research-dashboard--item
    "lost-rob0t/chatgpt-books" "main"
    "research/publishing-pipeline.org" "deadbeef"
    "#+title: Publishing Pipeline Decision\n* Findings\nBody\n")))

(ert-deftest nsa/research-dashboard-default-view-hides-unmigrated ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (let ((canonical (nsa/research-dashboard--item
                      "lost-rob0t/prolog-rlm" "main" "research/a.org"
                      "a" nsa/research-dashboard-test--canonical))
          (legacy (nsa/research-dashboard--item
                   "lost-rob0t/starintel-auto-research" "main"
                   "roam/research/star-server/b.org" "b"
                   nsa/research-dashboard-test--legacy-review)))
      (setq nsa/research-dashboard--items (list canonical legacy))
      (should-not nsa/research-dashboard--show-legacy)
      (should (equal (nsa/research-dashboard--visible-items)
                     (list canonical)))
      (setq nsa/research-dashboard--show-legacy t)
      (should (= (length (nsa/research-dashboard--visible-items)) 2)))))

(ert-deftest nsa/research-dashboard-search-and-filters-are-local ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (let ((a (nsa/research-dashboard--item
              "lost-rob0t/prolog-rlm" "main"
              "research/RLM-RESEARCH-020-ui.org" "a"
              (string-replace "Canonical research" "Emacs UI contract"
                              nsa/research-dashboard-test--canonical)))
          (b (nsa/research-dashboard--item
              "lost-rob0t/prolog-rlm" "main"
              "research/RLM-RESEARCH-010-prompt.org" "b"
              (string-replace "Canonical research" "Prompt compiler"
                              nsa/research-dashboard-test--canonical))))
      (setq nsa/research-dashboard--items (list a b)
            nsa/research-dashboard--query "emacs")
      (should (equal (nsa/research-dashboard--visible-items) (list a)))
      (setq nsa/research-dashboard--query ""
            nsa/research-dashboard--lifecycle-filter "RESEARCHED")
      (should (= (length (nsa/research-dashboard--visible-items)) 2))
      (setq nsa/research-dashboard--repo-filter "lost-rob0t/other")
      (should-not (nsa/research-dashboard--visible-items)))))

(ert-deftest nsa/research-dashboard-major-mode-has-review-navigation-keys ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (should (derived-mode-p 'nsa/research-dashboard-mode))
    (should (eq (lookup-key nsa/research-dashboard-mode-map (kbd "/"))
                #'nsa/research-dashboard-search))
    (should (eq (lookup-key nsa/research-dashboard-mode-map (kbd "L"))
                #'nsa/research-dashboard-toggle-legacy))
    (should (keymapp (lookup-key nsa/research-dashboard-mode-map (kbd "f"))))))

(ert-deftest nsa/research-dashboard-migrates-review-ready-unmigrated-without-changing-lifecycle ()
  (let ((updated (nsa/research-dashboard--decision-content
                  nsa/research-dashboard-test--legacy-review "APPROVED"
                  "lost-rob0t" "human:test" "aaaa" "bbbb"
                  "2026-08-27T04:00:00-04:00")))
    (should (string-match-p "^#\\+status: REVIEW$" updated))
    (should (string-match-p "^#\\+approval_state: APPROVED$" updated))
    (should (string-match-p "^#\\+approval_base_commit: aaaa$" updated))
    (should (string-match-p "^#\\+approval_base_blob: bbbb$" updated))))

(ert-deftest nsa/research-dashboard-updates-canonical-fields-only ()
  (let ((updated (nsa/research-dashboard--decision-content
                  nsa/research-dashboard-test--canonical "APPROVED"
                  "lost-rob0t" "human:test" "aaaa" "bbbb"
                  "2026-08-27T04:00:00-04:00")))
    (should (string-match-p "^#\\+status: RESEARCHED$" updated))
    (should (string-match-p "^#\\+approval_state: APPROVED$" updated))
    (should (string-match-p "^\\* Findings$" updated))
    (should (string-match-p "^Body$" updated))))

(ert-deftest nsa/research-dashboard-rejects-partial-approval-metadata ()
  (should-error
   (nsa/research-dashboard--decision-content
    (concat nsa/research-dashboard-test--legacy-review
            "#+approval_actor: somebody\n")
    "APPROVED" "lost-rob0t" "human:test" "aaaa" "bbbb"
    "2026-08-27T04:00:00-04:00")
   :type 'user-error))

(ert-deftest nsa/research-dashboard-gh-uses-async-processes ()
  (let (spec)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "/usr/bin/gh"))
              ((symbol-function 'make-process)
               (lambda (&rest process-spec)
                 (setq spec process-spec)
                 'fake-process)))
      (with-temp-buffer
        (nsa/research-dashboard--gh
         (current-buffer) (lambda (&rest _ignored)) '("api" "user"))
        (should (equal (plist-get spec :command) '("gh" "api" "user")))
        (should (eq (plist-get spec :connection-type) 'pipe))
        (should (functionp (plist-get spec :sentinel)))))))

(ert-deftest nsa/research-dashboard-source-forbids-blocking-process-apis ()
  (let ((source-file (symbol-file 'nsa/research-dashboard-refresh 'defun)))
    (should source-file)
    (with-temp-buffer
      (insert-file-contents source-file)
      (let ((source (buffer-string)))
        (dolist (forbidden '("(process-file" "(process-lines" "(call-process"
                             "(call-process-region" "(shell-command"
                             "(shell-command-to-string" "(start-process-shell-command"
                             "(make-thread" "(accept-process-output" "(sleep-for"))
          (should-not (string-match-p (regexp-quote forbidden) source)))
        (should (string-match-p (regexp-quote "(make-process") source))))))

(ert-deftest nsa/research-dashboard-refresh-schedules-and-returns ()
  (let (called)
    (with-temp-buffer
      (nsa/research-dashboard-mode)
      (cl-letf (((symbol-function 'nsa/research-dashboard--discover)
                 (lambda (generation)
                   (setq called generation))))
        (nsa/research-dashboard-refresh)
        (should called)
        (should nsa/research-dashboard--scanning)
        (should (= called nsa/research-dashboard--generation))))))

(provide 'research-dashboard-test)
;;; research-dashboard-test.el ends here
