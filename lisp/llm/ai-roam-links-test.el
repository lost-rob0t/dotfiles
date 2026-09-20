;;; ai-roam-links-test.el --- Tests for ai-roam-links -*- lexical-binding: t; -*-

(require 'ert)
(require 'ai-roam-links)

(defmacro ai-roam-links-test-with-dir (&rest body)
  "Run BODY with a fresh temp roam directory bound to `ai/roam-directory'."
  (declare (indent 0))
  `(let ((ai/roam-directory (file-name-as-directory
                             (make-temp-file "ai-roam-links-test" t)))
         (ai/full-editor-rights nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory ai/roam-directory t))))

(defun ai-roam-links-test-write (name content)
  "Write CONTENT to NAME inside the test roam directory; return its path."
  (let ((file (expand-file-name name ai/roam-directory)))
    (with-temp-file file
      (insert content))
    file))

(ert-deftest ai-roam-links-scan-finds-only-real-idless-files ()
  "Lock and backup files are skipped; only real ID-less org files are listed."
  (ai-roam-links-test-with-dir
    (let ((with-id (ai-roam-links-test-write
                    "has-id.org"
                    ":PROPERTIES:\n:ID: abc-123\n:END:\n* Heading\n"))
          (locked (ai-roam-links-test-write ".#locked.org" "unseen.host:42\n"))
          (backup (ai-roam-links-test-write "backup.org~" "* old stuff\n"))
          (missing (ai-roam-links-test-write
                    "no-id.org"
                    "#+TITLE: No ID\n\n* Heading\n")))
      (should (equal (ai/roam-links-scan-directory) (list missing)))
      (should (equal (ai/roam-links-scan-directory ai/roam-directory)
                     (list missing)))
      (should-not (member with-id (ai/roam-links-scan-directory)))
      (should-not (member locked (ai/roam-links-scan-directory)))
      (should-not (member backup (ai/roam-links-scan-directory))))))

(ert-deftest ai-roam-links-scan-covers-subdirectories-and-sorts ()
  "The scan is recursive and returns absolute paths sorted by name."
  (ai-roam-links-test-with-dir
    (mkdir (expand-file-name "hacking" ai/roam-directory) t)
    (let ((b (ai-roam-links-test-write "b.org" "* b\n"))
          (a (ai-roam-links-test-write "a.org" "* a\n"))
          (deep (ai-roam-links-test-write "hacking/deep.org" "* deep\n")))
      (let ((result (ai/roam-links-scan-directory)))
        (should (equal result (sort (copy-sequence result) #'string<)))
        (should (equal result (list a b deep)))
        (dolist (file result)
          (should (file-name-absolute-p file)))))))

(ert-deftest ai-roam-links-scan-empty-when-all-have-ids ()
  "Files carrying an :ID: in their top drawer are not reported."
  (ai-roam-links-test-with-dir
    (ai-roam-links-test-write
     "one.org" ":PROPERTIES:\n:ID: id-one\n:END:\n* One\n")
    (ai-roam-links-test-write
     "two.org" "#+TITLE: Two\n:PROPERTIES:\n:ID: id-two\n:END:\n* Two\n")
    (should (null (ai/roam-links-scan-directory)))))

(ert-deftest ai-roam-links-drawer-check-crlf-tolerant ()
  "CRLF line endings do not hide an :ID: inside the top drawer."
  (ai-roam-links-test-with-dir
    (let ((file (ai-roam-links-test-write
                 "crlf.org"
                 "#+TITLE: CRLF\r\n:PROPERTIES:\r\n:ID: crlf-id\r\n:END:\r\n* H\r\n")))
      (should (equal (ai/roam-links--top-drawer-id file) "crlf-id"))
      (should (null (ai/roam-links-scan-directory))))))

(ert-deftest ai-roam-links-ensure-id-creates-drawer-after-title ()
  "Without a drawer, one is inserted after the leading #+KEYWORD lines."
  (ai-roam-links-test-with-dir
    (let ((file (ai-roam-links-test-write
                 "new.org" "#+TITLE: Fresh\n\n* Heading\n")))
      (let ((id (ai/roam-links--ensure-id file)))
        (should (stringp id))
        (should (> (length id) 0))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (should (looking-at-p "#\\+TITLE: Fresh"))
          (should (re-search-forward "^:PROPERTIES:[ \t]*$" nil t))
          (should (re-search-forward
                   (concat "^:ID: " (regexp-quote id) "[ \t]*$") nil t))
          (should (re-search-forward "^:END:[ \t]*$" nil t))
          (should (re-search-forward "^\\* Heading[ \t]*$" nil t)))))))

(ert-deftest ai-roam-links-ensure-id-extends-existing-drawer ()
  "An existing top drawer gains the :ID: line without reordering."
  (ai-roam-links-test-with-dir
    (let ((file (ai-roam-links-test-write
                 "drawer.org"
                 "#+TITLE: Drawer\n:PROPERTIES:\n:CREATED: 2026-01-01\n:END:\n* H\n")))
      (let ((id (ai/roam-links--ensure-id file)))
        (should (stringp id))
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p
                   (concat ":PROPERTIES:\n:ID: " (regexp-quote id)
                           "\n:CREATED: 2026-01-01\n:END:\n")
                   (buffer-string))))))))

(ert-deftest ai-roam-links-ensure-id-idempotent ()
  "A second call returns the same ID and leaves the file untouched."
  (ai-roam-links-test-with-dir
    (let ((file (ai-roam-links-test-write "again.org" "* Twice\n")))
      (let ((id (ai/roam-links--ensure-id file))
            (content (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
        (should (equal (ai/roam-links--ensure-id file) id))
        (should (equal (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))
                       content))
        ;; Exactly one drawer, exactly one ID line.
        (with-temp-buffer
          (insert-file-contents file)
          (should (= (how-many ":PROPERTIES:") 1))
          (should (= (how-many "^:ID:") 1)))))))

(ert-deftest ai-roam-links-tool-fails-closed-without-rights ()
  "Without full rights the tool returns a rights error and edits nothing."
  (ai-roam-links-test-with-dir
    (let* ((file (ai-roam-links-test-write "locked.org" "#+TITLE: Locked\n"))
           (before (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
           (result (ai/roam-links--tool-create-id-link file "Title")))
      (should (stringp result))
      (should (string-match-p "rights" result))
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     before))
      (should (null (ai/roam-links--top-drawer-id file))))))

(ert-deftest ai-roam-links-tool-creates-link-with-rights ()
  "With full rights the tool writes an ID and returns the roam link."
  (ai-roam-links-test-with-dir
    (let ((ai/full-editor-rights t)
          (file (ai-roam-links-test-write "target.org" "#+TITLE: Target\n")))
      (let ((result (ai/roam-links--tool-create-id-link file "Title")))
        (should (stringp result))
        (should (string-match-p "\\[\\[roam:" result))
        (should (string-match-p "\\[\\[roam:[^]]*\\]\\[Title\\]\\]" result))
        (should (string-match-p (regexp-quote file) result))
        (let ((id (ai/roam-links--top-drawer-id file)))
          (should id)
          (should (string-match-p (regexp-quote id) result)))))))

(ert-deftest ai-roam-links-tool-resolves-relative-targets ()
  "Relative targets resolve against `ai/roam-directory'."
  (ai-roam-links-test-with-dir
    (let ((ai/full-editor-rights t)
          (file (ai-roam-links-test-write "rel.org" "* Rel\n")))
      (let ((result (ai/roam-links--tool-create-id-link "rel.org" "Rel")))
        (should (string-match-p "\\[\\[roam:" result))
        (should (ai/roam-links--top-drawer-id file))))))

(ert-deftest ai-roam-links-repair-missing-repairs-all ()
  "The repair loop fixes ID-less files and reports the counts."
  (ai-roam-links-test-with-dir
    (let ((a (ai-roam-links-test-write "a.org" "* A\n"))
          (b (ai-roam-links-test-write
              "b.org" ":PROPERTIES:\n:ID: keep-me\n:END:\n* B\n")))
      (let ((result (ai/roam-links-repair-missing)))
        (should (equal result '(1 0)))
        (should (ai/roam-links--top-drawer-id a))
        (should (equal (ai/roam-links--top-drawer-id b) "keep-me"))
        (should (null (ai/roam-links-scan-directory)))))))

(ert-deftest ai-roam-links-clear-tool-without-gptel ()
  "The clear helper is a no-op when gptel is not loaded."
  (should (null (ai/roam-links--clear-gptel-tool "create_roam_id_link"))))

(provide 'ai-roam-links-test)
;;; ai-roam-links-test.el ends here
