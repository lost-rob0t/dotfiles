;;; star-android-reader-test.el --- Android reader tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'star-android-reader)

(defun star/android-test--write-file (file content)
  "Write CONTENT to FILE, creating its parent directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert content)))

(ert-deftest star/android-metadata-prefers-org-dates ()
  (let ((file (make-temp-file "star-android-" nil ".org")))
    (unwind-protect
        (progn
          (star/android-test--write-file
           file
           "#+title: Example\n#+status: review\n#+created: [2026-08-01 Sat]\n#+last_modified: [2026-08-13 Thu]\n")
          (let ((metadata (star/android-document-metadata file)))
            (should (equal (plist-get metadata :title) "Example"))
            (should (equal (plist-get metadata :status) "REVIEW"))
            (should (equal (plist-get metadata :date) "[2026-08-13 Thu]"))
            (should (> (plist-get metadata :timestamp) 0))))
      (delete-file file))))

(ert-deftest star/android-reviewed-status-filter ()
  (let ((star/android-reviewed-statuses '("APPROVED" "IMPLEMENTED")))
    (should (star/android-reviewed-p '(:status "APPROVED")))
    (should-not (star/android-reviewed-p '(:status "REVIEW")))))

(ert-deftest star/android-newest-first-does-not-mutate-input ()
  (let* ((old '(:title "old" :timestamp 1.0))
         (new '(:title "new" :timestamp 2.0))
         (input (list old new))
         (sorted (star/android-newest-first input)))
    (should (equal (mapcar (lambda (item) (plist-get item :title)) input)
                   '("old" "new")))
    (should (equal (mapcar (lambda (item) (plist-get item :title)) sorted)
                   '("new" "old")))))

(ert-deftest star/android-repository-discovery-requires-git-and-roam ()
  (let ((root (make-temp-file "star-android-repos-" t)))
    (unwind-protect
        (let ((star/android-research-root root))
          (make-directory (expand-file-name "good/.git" root) t)
          (make-directory (expand-file-name "good/roam" root) t)
          (make-directory (expand-file-name "not-a-repo/roam" root) t)
          (make-directory (expand-file-name "no-roam/.git" root) t)
          (should
           (equal (mapcar #'file-name-nondirectory
                          (mapcar #'directory-file-name
                                  (star/android-repositories)))
                  '("good"))))
      (delete-directory root t))))

(ert-deftest star/android-index-detection-is-path-based ()
  (should
   (star/android-index-p
    '(:file "/tmp/repo/roam/indexes/star-lang/STAR-INDEX-000.org")))
  (should-not
   (star/android-index-p
    '(:file "/tmp/repo/roam/research/star-lang/STAR-RESEARCH-000.org"))))

(provide 'star-android-reader-test)
;;; star-android-reader-test.el ends here
