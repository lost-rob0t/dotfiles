;;; ai-roam-memory-test.el --- Tests for ai-roam-memory -*- lexical-binding: t; -*-

(require 'ert)
(require 'ai-roam-memory)

(defmacro ai-roam-memory-test-with-dir (&rest body)
  "Run BODY with a fresh temp roam directory bound to `ai/roam-directory'."
  (declare (indent 0))
  `(let ((ai/roam-directory (file-name-as-directory
                             (make-temp-file "ai-roam-memory-test" t)))
         (ai/full-editor-rights nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory ai/roam-directory t))))

(defun ai-roam-memory-test-read (file)
  "Return FILE's contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest ai-roam-memory-contract-defaults ()
  "The defcustom defaults match the documented contract."
  (should (equal ai/roam-memory-dir "llm/memory/"))
  (should (equal ai/roam-memory-kb-file "llm/memory/kb-facts.pl")))

(ert-deftest ai-roam-memory-file-slug-deterministic-and-sanitized ()
  "Memory file paths are deterministic, lowercased, and sanitized."
  (ai-roam-memory-test-with-dir
    (let ((path (ai/roam-memory-file "My Fancy Subject!")))
      (should (string= path
                       (expand-file-name
                        "my-fancy-subject.org"
                        (file-name-as-directory
                         (expand-file-name "llm/memory" ai/roam-directory)))))
      ;; Deterministic: same subject, same path; distinct subjects differ.
      (should (string= path (ai/roam-memory-file "My Fancy Subject!")))
      (should-not (string= path (ai/roam-memory-file "other subject")))
      ;; Uppercase, spaces, and punctuation never reach the slug.
      (should (string= (file-name-nondirectory
                        (ai/roam-memory-file "UPPER Case!!"))
                       "upper-case.org")))))

(ert-deftest ai-roam-memory-read-absent-returns-nil ()
  "Reading a subject with no note returns nil, never an error."
  (ai-roam-memory-test-with-dir
    (should (null (ai/roam-memory-read "ghost")))))

(ert-deftest ai-roam-memory-write-read-round-trips ()
  "With full rights a write creates the note; read returns its content."
  (ai-roam-memory-test-with-dir
    (let ((ai/full-editor-rights t))
      (let ((path (ai/roam-memory-write
                   "Graph Ingest" "Facts about graph ingest pipelines."
                   "agent run 42")))
        (should (file-exists-p path))
        (let ((text (ai-roam-memory-test-read path)))
          (should (string-match-p "\\`#\\+TITLE: Graph Ingest" text))
          (should (string-match-p ":PROPERTIES:" text))
          (should (string-match-p ":ID: " text))
          (should (string-match-p ":END:" text))
          (should (string-match-p "^\\* Memory$" text))
          (should (string-match-p
                   (regexp-quote "Facts about graph ingest pipelines.") text))
          (should (string-match-p "^- Source: agent run 42$" text)))
        (should (string-match-p
                 (regexp-quote "Facts about graph ingest pipelines.")
                 (ai/roam-memory-read "Graph Ingest")))))))

(ert-deftest ai-roam-memory-write-without-rights-fails-closed ()
  "Without full rights the write user-errors and creates nothing."
  (ai-roam-memory-test-with-dir
    (let ((err (condition-case e
                   (progn (ai/roam-memory-write "Secret" "nope") nil)
                 (user-error e))))
      (should err)
      (should (string-match-p "rights" (error-message-string err))))
    (should-not (file-exists-p (ai/roam-memory-file "Secret")))))

(ert-deftest ai-roam-memory-list-sorted ()
  "Subjects derive from the note files and come back sorted."
  (ai-roam-memory-test-with-dir
    (should (null (ai/roam-memory-list)))
    (let ((ai/full-editor-rights t))
      (ai/roam-memory-write "bravo" "b")
      (ai/roam-memory-write "alpha" "a")
      (ai/roam-memory-write "charlie" "c"))
    (should (equal (ai/roam-memory-list) '("alpha" "bravo" "charlie")))))

(ert-deftest ai-roam-memory-write-idempotent ()
  "Rewriting a subject keeps one file, updates content, keeps its ID."
  (ai-roam-memory-test-with-dir
    (let ((ai/full-editor-rights t))
      (let* ((path (ai/roam-memory-write "topic" "first draft"))
             (first-id (with-temp-buffer
                         (insert-file-contents path)
                         (goto-char (point-min))
                         (re-search-forward "^:ID: \\(.*\\)$")
                         (match-string 1)))
             (path2 (ai/roam-memory-write "topic" "second draft v2")))
        (should (string= path path2))
        (should (= (length (ai/roam-memory-list)) 1))
        (let ((text (ai-roam-memory-test-read path)))
          (should (string-match-p "second draft v2" text))
          (should-not (string-match-p "first draft" text))
          (should (string-match-p
                   (concat ":ID: " (regexp-quote first-id) "$") text)))))))

(ert-deftest ai-roam-memory-assert-fact-appends-and-escapes ()
  "Each assertion appends exactly one escaped world_fact line."
  (ai-roam-memory-test-with-dir
    (let ((ai/full-editor-rights t))
      (should (= (ai/roam-memory-fact-count) 0))
      (let ((line (ai/roam-memory-assert-fact
                   "it's a subject" "depends on" "the 'grid'")))
        (should (string-match-p "\\`world_fact(" line))
        (should (string-match-p "'it''s a subject'" line))
        (should (string-match-p "'the ''grid'''" line))
        (should (= (ai/roam-memory-fact-count) 1)))
      (ai/roam-memory-assert-fact "other" "relates to" "thing")
      (should (= (ai/roam-memory-fact-count) 2))
      (with-temp-buffer
        (insert-file-contents (ai/roam-memory--kb-file))
        (goto-char (point-min))
        (should (= (how-many "^world_fact(") 2))
        (should (string-suffix-p ")\n" (buffer-string)))))))

(ert-deftest ai-roam-memory-assert-fact-rejects-newlines ()
  "Values containing newlines are rejected with user-error."
  (ai-roam-memory-test-with-dir
    (let ((ai/full-editor-rights t))
      (should-error (ai/roam-memory-assert-fact "bad\nsubject" "rel" "x")
                    :type 'user-error)
      (should-error (ai/roam-memory-assert-fact "s" "re\nl" "x")
                    :type 'user-error)
      (should-error (ai/roam-memory-assert-fact "s" "rel" "multi\nline")
                    :type 'user-error)
      (should (= (ai/roam-memory-fact-count) 0))
      (should-not (file-exists-p (ai/roam-memory--kb-file))))))

(ert-deftest ai-roam-memory-assert-fact-without-rights-fails-closed ()
  "Without full rights no fact line is written."
  (ai-roam-memory-test-with-dir
    (let ((err (should-error (ai/roam-memory-assert-fact "s" "rel" "o")
                             :type 'user-error)))
      (should (string-match-p "rights" (error-message-string err))))
    (should (= (ai/roam-memory-fact-count) 0))
    (should-not (file-exists-p (ai/roam-memory--kb-file)))))

(ert-deftest ai-roam-memory-gptel-tool-names ()
  "The registered gptel tool names match the documented contract."
  (should (equal ai/roam-memory-gptel-tool-remember "remember_fact"))
  (should (equal ai/roam-memory-gptel-tool-recall "recall_memory"))
  (should (equal ai/roam-memory-gptel-tool-assert-fact "assert_world_fact")))

(ert-deftest ai-roam-memory-tool-remember-fails-soft-without-rights ()
  "The remember tool returns a rights error string, not a signal."
  (ai-roam-memory-test-with-dir
    (let ((result (ai/roam-memory--tool-remember "locked" "content")))
      (should (stringp result))
      (should (string-match-p "rights" result)))
    (should-not (file-exists-p (ai/roam-memory-file "locked")))))

(ert-deftest ai-roam-memory-tool-remember-and-recall-round-trip ()
  "remember_fact confirms with the path; recall_memory returns content."
  (ai-roam-memory-test-with-dir
    (let ((ai/full-editor-rights t))
      (let ((result (ai/roam-memory--tool-remember "grid" "grid facts")))
        (should (stringp result))
        (should (string-match-p (regexp-quote (ai/roam-memory-file "grid"))
                                result)))
      (should (string-match-p "grid facts" (ai/roam-memory--tool-recall "grid")))
      (should (string-match-p "No memory for"
                              (ai/roam-memory--tool-recall "absent"))))))

(ert-deftest ai-roam-memory-tool-assert-fact-reports-count ()
  "assert_world_fact confirms with the fact count after the write."
  (ai-roam-memory-test-with-dir
    (let ((ai/full-editor-rights t))
      (should (string-match-p
               "\\b1\\b" (ai/roam-memory--tool-assert-fact "grid" "feeds" "plc")))
      (should (string-match-p
               "\\b2\\b" (ai/roam-memory--tool-assert-fact "sensor" "reads" "value"))))
    (let ((ai/full-editor-rights nil))
      (let ((result (ai/roam-memory--tool-assert-fact "x" "y" "z")))
        (should (stringp result))
        (should (string-match-p "rights" result))))))

(ert-deftest ai-roam-memory-register-without-gptel-matches-siblings ()
  "Register either registers the tools or user-errors about gptel.
The clear helper stays a no-op without gptel, like the siblings."
  (should (null (ai/roam-memory--clear-gptel-tool
                 ai/roam-memory-gptel-tool-remember)))
  (let ((result (condition-case err
                    (ai/roam-memory-register-gptel-tools)
                  (user-error
                   (if (string-match-p "gptel" (error-message-string err))
                       'no-gptel
                     (signal (car err) (cdr err)))))))
    (unless (eq result 'no-gptel)
      (should (equal result (list ai/roam-memory-gptel-tool-remember
                                  ai/roam-memory-gptel-tool-recall
                                  ai/roam-memory-gptel-tool-assert-fact))))))

(provide 'ai-roam-memory-test)
;;; ai-roam-memory-test.el ends here
