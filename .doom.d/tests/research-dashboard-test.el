;;; research-dashboard-test.el --- Tests for research dashboard -*- lexical-binding: t; -*-

(require 'ert)
(require 'research-dashboard)

(defconst nsa/research-dashboard-test--canonical
  "#+title: Canonical research\n#+status: RESEARCHED\n#+approval_schema: prolog-rlm.research-approval.v1\n#+approval_state: PENDING\n#+approval_actor: NONE\n#+approval_evidence: NONE\n#+approval_base_commit: NONE\n#+approval_base_blob: NONE\n#+approval_decided_at: NONE\n\n* Findings\nBody\n")

(defconst nsa/research-dashboard-test--legacy
  "#+title: Legacy StarIntel research\n#+status: REVIEW\n#+filetags: :research:\n\n* Findings\nBody\n")

(defun nsa/research-dashboard-test--item
    (&optional approval lifecycle title repo path)
  (nsa/research-item-create
   :repo (or repo "lost-rob0t/prolog-rlm")
   :branch "main"
   :path (or path "research/example.org")
   :blob "deadbeef"
   :title (or title "Example research")
   :lifecycle (or lifecycle "RESEARCHED")
   :approval (or approval "PENDING")
   :content nsa/research-dashboard-test--canonical))

(ert-deftest nsa/research-dashboard-recognizes-research-paths ()
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

(ert-deftest nsa/research-dashboard-canonical-pending-is-open ()
  (let ((item
         (nsa/research-dashboard--item
          "lost-rob0t/prolog-rlm" "main" "research/example.org"
          "deadbeef" nsa/research-dashboard-test--canonical)))
    (should item)
    (should (equal (nsa/research-item-approval item) "PENDING"))
    (should (equal (nsa/research-item-lifecycle item) "RESEARCHED"))))

(ert-deftest nsa/research-dashboard-legacy-active-record-can-be-loaded ()
  (let ((item
         (nsa/research-dashboard--item
          "lost-rob0t/starintel-auto-research" "main"
          "roam/research/star-server/example.org" "deadbeef"
          nsa/research-dashboard-test--legacy)))
    (should item)
    (should (equal (nsa/research-item-approval item) "LEGACY"))))

(ert-deftest nsa/research-dashboard-unmigrated-review-ready-is-retained ()
  (dolist (state '("REVIEW" "RESEARCHED" "VERIFIED"))
    (should
     (nsa/research-dashboard--item
      "lost-rob0t/starintel-auto-research" "main"
      "roam/research/star-server/example.org" "deadbeef"
      (string-replace "#+status: REVIEW"
                      (format "#+status: %s" state)
                      nsa/research-dashboard-test--legacy)))))

(ert-deftest nsa/research-dashboard-unmigrated-nonreview-states-are-not-queued ()
  (dolist (state '("DRAFT" "RESEARCHING" "APPROVED" "DONE" "REJECTED"
                   "SUPERSEDED" "implemented-prototype"
                   "accepted-for-realization" "MISSING"))
    (should-not
     (nsa/research-dashboard--item
      "lost-rob0t/starintel-auto-research" "main"
      "roam/research/star-server/example.org" "deadbeef"
      (if (string= state "MISSING")
          "#+title: Missing lifecycle\n* Findings\nBody\n"
        (string-replace "#+status: REVIEW"
                        (format "#+status: %s" state)
                        nsa/research-dashboard-test--legacy))))))

(ert-deftest nsa/research-dashboard-hides-legacy-by-default ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (let ((canonical (nsa/research-dashboard-test--item))
          (legacy (nsa/research-dashboard-test--item "LEGACY" "REVIEW")))
      (setq nsa/research-dashboard--items (list canonical legacy)
            nsa/research-dashboard--show-legacy nil
            nsa/research-dashboard--search-text ""
            nsa/research-dashboard--field-filters nil)
      (should (equal (nsa/research-dashboard--visible-items)
                     (list canonical)))
      (setq nsa/research-dashboard--show-legacy t)
      (should (= (length (nsa/research-dashboard--visible-items)) 2)))))

(ert-deftest nsa/research-dashboard-searches-all-important-fields ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (let ((item
           (nsa/research-dashboard-test--item
            "PENDING" "RESEARCHED"
            "Managed Context Tool Discovery"
            "lost-rob0t/prolog-rlm"
            "research/RLM-RESEARCH-011-managed-context-tool-discovery.org")))
      (setq nsa/research-dashboard--items (list item)
            nsa/research-dashboard--show-legacy nil
            nsa/research-dashboard--field-filters nil
            nsa/research-dashboard--search-text "managed context")
      (should (equal (nsa/research-dashboard--visible-items) (list item)))
      (setq nsa/research-dashboard--search-text "starintel")
      (should-not (nsa/research-dashboard--visible-items)))))

(ert-deftest nsa/research-dashboard-field-filter-is-exact ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (let ((rlm (nsa/research-dashboard-test--item
                "PENDING" "RESEARCHED" "RLM"
                "lost-rob0t/prolog-rlm"))
          (other (nsa/research-dashboard-test--item
                  "PENDING" "REVIEW" "Other"
                  "lost-rob0t/other-repo")))
      (setq nsa/research-dashboard--items (list rlm other)
            nsa/research-dashboard--show-legacy nil
            nsa/research-dashboard--search-text ""
            nsa/research-dashboard--field-filters
            '((repository . "lost-rob0t/prolog-rlm")))
      (should (equal (nsa/research-dashboard--visible-items) (list rlm))))))

(ert-deftest nsa/research-dashboard-major-mode-has-review-controls ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (should (derived-mode-p 'nsa/research-dashboard-mode))
    (should (eq (lookup-key nsa/research-dashboard-mode-map (kbd "/"))
                #'nsa/research-dashboard-search))
    (should (eq (lookup-key nsa/research-dashboard-mode-map (kbd "f"))
                #'nsa/research-dashboard-filter))
    (should (eq (lookup-key nsa/research-dashboard-mode-map (kbd "L"))
                #'nsa/research-dashboard-toggle-legacy))
    (should (eq (lookup-key nsa/research-dashboard-mode-map (kbd "a"))
                #'nsa/research-dashboard-approve))))

(ert-deftest nsa/research-dashboard-migrates-legacy-without-changing-lifecycle ()
  (let ((updated
         (nsa/research-dashboard--decision-content
          nsa/research-dashboard-test--legacy "APPROVED"
          "lost-rob0t" "human:test" "aaaa" "bbbb"
          "2026-08-27T04:00:00-04:00")))
    (should (string-match-p "^#\\+status: REVIEW$" updated))
    (should (string-match-p "^#\\+approval_state: APPROVED$" updated))
    (should (string-match-p "^#\\+approval_base_commit: aaaa$" updated))
    (should (string-match-p "^#\\+approval_base_blob: bbbb$" updated))))

(ert-deftest nsa/research-dashboard-updates-canonical-fields-only ()
  (let ((updated
         (nsa/research-dashboard--decision-content
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
    (concat nsa/research-dashboard-test--legacy
            "#+approval_actor: somebody\n")
    "APPROVED" "lost-rob0t" "human:test" "aaaa" "bbbb"
    "2026-08-27T04:00:00-04:00")
   :type 'user-error))

(ert-deftest nsa/research-dashboard-approval-commit-skips-ci ()
  (let* ((item (nsa/research-dashboard-test--item))
         (decision
          (nsa/research-decision-create
           :item item :state "APPROVED")))
    (should
     (string-match-p
      (regexp-quote "[skip ci]")
      (nsa/research-dashboard--approval-commit-message decision)))))

(ert-deftest nsa/research-dashboard-approval-merge-is-immediate-rebase ()
  (let* ((item (nsa/research-dashboard-test--item))
         (decision
          (nsa/research-decision-create
           :dashboard (current-buffer)
           :item item
           :state "APPROVED"
           :approval-commit "abc123"
           :pr-number 42))
         captured-args captured-input)
    (cl-letf
        (((symbol-function 'nsa/research-dashboard--gh-json)
          (lambda (_dashboard _callback args &optional input)
            (setq captured-args args
                  captured-input input))))
      (nsa/research-dashboard--merge-approval-pr decision))
    (should
     (equal captured-args
            '("api" "--method" "PUT"
              "repos/lost-rob0t/prolog-rlm/pulls/42/merge"
              "--input" "-")))
    (let ((payload (nsa/research-dashboard--json captured-input)))
      (should (equal (nsa/research-dashboard--get "merge_method" payload)
                     "rebase"))
      (should (equal (nsa/research-dashboard--get "sha" payload)
                     "abc123")))))

(ert-deftest nsa/research-dashboard-approval-pr-body-has-machine-marker ()
  (let* ((item (nsa/research-dashboard-test--item))
         (decision
          (nsa/research-decision-create
           :dashboard (current-buffer)
           :item item
           :state "APPROVED"
           :actor "operator"
           :evidence "operator decision"
           :approval-branch "research-approval/example"
           :approval-commit "abc123"))
        payload)
    (cl-letf
        (((symbol-function 'nsa/research-dashboard--gh-json)
          (lambda (_dashboard callback args &optional input)
            (if (seq-some (lambda (arg) (string-suffix-p "/merge" arg)) args)
                (funcall callback t '((merged . t)))
              (setq payload (nsa/research-dashboard--json input))
              (funcall callback t '((number . 42) (html_url . "https://github.com/example/pr/42")))))))
      (nsa/research-dashboard--create-approval-pr decision))
    (should
     (string-match-p
      (regexp-quote "<!-- starintel-research-approval:v1 -->")
      (nsa/research-dashboard--get "body" payload)))))

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
         (current-buffer)
         (lambda (&rest _ignored))
         '("api" "user"))
        (should (equal (plist-get spec :command)
                       '("gh" "api" "user")))
        (should (eq (plist-get spec :connection-type) 'pipe))
        (should (functionp (plist-get spec :sentinel)))))))

(ert-deftest nsa/research-dashboard-gh-sends-keyword-input-asynchronously ()
  (let (sent-input eof-process)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "/usr/bin/gh"))
              ((symbol-function 'make-process)
               (lambda (&rest _process-spec) 'fake-process))
              ((symbol-function 'process-send-string)
               (lambda (process input)
                 (setq sent-input (list process input))))
              ((symbol-function 'process-send-eof)
               (lambda (process)
                 (setq eof-process process))))
      (with-temp-buffer
        (nsa/research-dashboard--gh
         (current-buffer)
         (lambda (&rest _ignored))
         '("api" "--method" "POST" "repos/example/project/pulls"
           "--input" "-")
         :input "{\"title\":\"Approval\"}")
        (should (equal sent-input
                       '(fake-process "{\"title\":\"Approval\"}")))
        (should (eq eof-process 'fake-process))))))

(ert-deftest nsa/research-dashboard-discover-passes-gh-argument-list ()
  (let (call)
    (with-temp-buffer
      (nsa/research-dashboard-mode)
      (setq nsa/research-dashboard--generation 1)
      (cl-letf (((symbol-function 'nsa/research-dashboard--gh)
                 (lambda (&rest arguments)
                   (setq call arguments))))
        (nsa/research-dashboard--discover 1))
      (should (= (length call) 3))
      (should (eq (car call) (current-buffer)))
      (should (functionp (cadr call)))
      (should (equal (caddr call) '("api" "user" "--jq" ".login"))))))

(ert-deftest nsa/research-dashboard-source-forbids-blocking-process-apis ()
  (let ((source-file
         (symbol-file 'nsa/research-dashboard-refresh 'defun)))
    (should source-file)
    (with-temp-buffer
      (insert-file-contents source-file)
      (let ((source (buffer-string)))
        (dolist
            (forbidden
             '("(process-file" "(process-lines" "(call-process"
               "(call-process-region" "(shell-command"
               "(shell-command-to-string" "(start-process-shell-command"
               "(make-thread" "(accept-process-output" "(sleep-for"))
          (should-not
           (string-match-p (regexp-quote forbidden) source)))
        (should (string-match-p (regexp-quote "(make-process") source))
        (should (string-match-p (regexp-quote "[skip ci]") source))
        (should (string-match-p
                 (regexp-quote "research-approval/") source))
        (should (string-match-p
                 (regexp-quote "pulls/%s/merge") source))
        (should-not
         (string-match-p
          (regexp-quote "(nsa/research-dashboard--write decision)")
          source))))))

(ert-deftest nsa/research-dashboard-refresh-schedules-and-returns ()
  (let (called)
    (with-temp-buffer
      (nsa/research-dashboard-mode)
      (cl-letf
          (((symbol-function 'nsa/research-dashboard--discover)
            (lambda (generation)
              (setq called generation))))
        (nsa/research-dashboard-refresh)
        (should called)
        (should nsa/research-dashboard--scanning)
        (should (= called nsa/research-dashboard--generation))))))

(provide 'research-dashboard-test)
;;; research-dashboard-test.el ends here
