;;; research-dashboard-local-test.el --- Tests for local research sources -*- lexical-binding: t; -*-

(require 'ert)
(require 'research-dashboard)
(require 'research-dashboard-local)

(defconst nsa/research-dashboard-local-test--canonical
  "#+title: Local research\n#+status: RESEARCHED\n#+approval_schema: prolog-rlm.research-approval.v1\n#+approval_state: PENDING\n#+approval_actor: NONE\n#+approval_evidence: NONE\n#+approval_base_commit: NONE\n#+approval_base_blob: NONE\n#+approval_decided_at: NONE\n\n* Findings\nLocal body\n")

(defun nsa/research-dashboard-local-test--read (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest nsa/research-dashboard-local-parses-github-remotes ()
  (should (equal (nsa/research-dashboard-local--github-slug
                  "git@github.com:lost-rob0t/prolog-rlm.git")
                 "lost-rob0t/prolog-rlm"))
  (should (equal (nsa/research-dashboard-local--github-slug
                  "https://github.com/lost-rob0t/starintel-auto-research.git")
                 "lost-rob0t/starintel-auto-research")))

(ert-deftest nsa/research-dashboard-local-scans-explicit-arbitrary-directory ()
  (let* ((root (make-temp-file "research-dashboard-local-" t))
         (notes (expand-file-name "notes" root))
         (file (expand-file-name "example.org" notes))
         (nsa/research-dashboard-local-checkouts (list root)))
    (unwind-protect
        (progn
          (make-directory notes t)
          (with-temp-file file
            (insert nsa/research-dashboard-local-test--canonical))
          (with-temp-buffer
            (nsa/research-dashboard-mode)
            (setq nsa/research-dashboard--items nil
                  nsa/research-dashboard--errors nil
                  nsa/research-dashboard--seen (make-hash-table :test #'equal))
            (nsa/research-dashboard-local--scan-current-buffer)
            (should (= (length nsa/research-dashboard--items) 1))
            (let* ((item (car nsa/research-dashboard--items))
                   (id (nsa/research-dashboard-local--id item)))
              (should (string-prefix-p "local:" (nsa/research-item-repo item)))
              (should (equal (nsa/research-item-path item) "notes/example.org"))
              (should (equal (gethash id nsa/research-dashboard-local--files)
                             file)))))
      (delete-directory root t))))

(ert-deftest nsa/research-dashboard-local-register-shadows-remote-item ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (setq nsa/research-dashboard--seen (make-hash-table :test #'equal))
    (let* ((remote (nsa/research-item-create
                    :repo "lost-rob0t/prolog-rlm"
                    :branch "main"
                    :path "research/example.org"
                    :blob "remote"
                    :title "Remote"
                    :lifecycle "RESEARCHED"
                    :approval "PENDING"
                    :content "remote"))
           (local (nsa/research-item-create
                   :repo "lost-rob0t/prolog-rlm"
                   :branch "main"
                   :path "research/example.org"
                   :blob "local"
                   :title "Local"
                   :lifecycle "RESEARCHED"
                   :approval "PENDING"
                   :content "local")))
      (setq nsa/research-dashboard--items (list remote))
      (nsa/research-dashboard-local--register local "/tmp/example.org")
      (should (equal nsa/research-dashboard--items (list local)))
      (should (gethash "lost-rob0t/prolog-rlm:research/example.org"
                       nsa/research-dashboard--seen)))))

(ert-deftest nsa/research-dashboard-local-write-updates-checkout-file ()
  (let ((file (make-temp-file "research-dashboard-local-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert nsa/research-dashboard-local-test--canonical))
          (let* ((before (nsa/research-dashboard-local-test--read file))
                 (updated
                  (nsa/research-dashboard--decision-content
                   before "APPROVED" "operator" "human:test"
                   "local-head" "local-blob"
                   "2026-09-10T23:59:00-04:00")))
            (nsa/research-dashboard-local--write file before updated)
            (let ((after (nsa/research-dashboard-local-test--read file)))
              (should (string-match-p "^#\\+approval_state: APPROVED$" after))
              (should (string-match-p "^#\\+approval_base_commit: local-head$" after)))))
      (delete-file file))))

(ert-deftest nsa/research-dashboard-local-write-refuses-stale-content ()
  (let ((file (make-temp-file "research-dashboard-local-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "new content\n"))
          (should-error
           (nsa/research-dashboard-local--write
            file "old content\n" "replacement\n")
           :type 'user-error))
      (delete-file file))))

(ert-deftest nsa/research-dashboard-local-installs-pane-controls ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (should (eq (lookup-key nsa/research-dashboard-mode-map (kbd "d"))
                #'nsa/research-dashboard-local-add-directory))
    (should (eq (lookup-key nsa/research-dashboard-mode-map (kbd "D"))
                #'nsa/research-dashboard-local-remove-directory))))

(provide 'research-dashboard-local-test)
;;; research-dashboard-local-test.el ends here
