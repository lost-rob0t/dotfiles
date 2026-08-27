;;; research-dashboard-test.el --- Tests for research dashboard -*- lexical-binding: t; -*-

(require 'ert)
(require 'research-dashboard)

(defconst nsa/research-dashboard-test--canonical
  "#+title: Canonical research\n#+status: RESEARCHED\n#+approval_schema: prolog-rlm.research-approval.v1\n#+approval_state: PENDING\n#+approval_actor: NONE\n#+approval_evidence: NONE\n#+approval_base_commit: NONE\n#+approval_base_blob: NONE\n#+approval_decided_at: NONE\n\n* Findings\nBody\n")

(defconst nsa/research-dashboard-test--legacy
  "#+title: Legacy StarIntel research\n#+status: REVIEW\n#+filetags: :research:\n\n* Findings\nBody\n")

(ert-deftest nsa/research-dashboard-recognizes-research-paths ()
  (dolist (path '("research/example.org"
                  "roam/research/star-server/example.org"
                  "nested/research/topic/example.org"))
    (should (nsa/research-dashboard--candidate-path-p path)))
  (dolist (path '("roam/indexes/research.org"
                  "docs/research-notes.org"
                  "research/example.md"
                  "../research/example.org"))
    (should-not (nsa/research-dashboard--candidate-path-p path))))

(ert-deftest nsa/research-dashboard-canonical-pending-is-open ()
  (let ((item (nsa/research-dashboard--item
               "lost-rob0t/prolog-rlm" "main" "research/example.org"
               "deadbeef" nsa/research-dashboard-test--canonical)))
    (should item)
    (should (equal (nsa/research-item-approval item) "PENDING"))
    (should (equal (nsa/research-item-lifecycle item) "RESEARCHED"))))

(ert-deftest nsa/research-dashboard-starintel-auto-research-adapter ()
  (should (nsa/research-dashboard--item
           "lost-rob0t/starintel-auto-research" "main"
           "roam/research/star-server/example.org" "deadbeef"
           nsa/research-dashboard-test--legacy))
  (should-not
   (nsa/research-dashboard--item
    "lost-rob0t/starintel-auto-research" "main"
    "roam/research/star-server/example.org" "deadbeef"
    (string-replace "#+status: REVIEW" "#+status: DONE"
                    nsa/research-dashboard-test--legacy))))

(ert-deftest nsa/research-dashboard-migrates-legacy-without-changing-lifecycle ()
  (let ((updated (nsa/research-dashboard--decision-content
                  nsa/research-dashboard-test--legacy "APPROVED"
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
    (concat nsa/research-dashboard-test--legacy "#+approval_actor: somebody\n")
    "APPROVED" "lost-rob0t" "human:test" "aaaa" "bbbb"
    "2026-08-27T04:00:00-04:00")
   :type 'user-error))

(provide 'research-dashboard-test)
;;; research-dashboard-test.el ends here
