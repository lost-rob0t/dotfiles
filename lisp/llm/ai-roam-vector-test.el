;;; ai-roam-vector-test.el --- Tests for ai-roam-vector -*- lexical-binding: t; -*-

(require 'ert)
(require 'ai-roam-vector)

(defconst ai-roam-vector-test-search-json
  "{\"results\": [{\"filepath\": \"/notes/roam/hacking/tool.org\", \"snippet\": \"vector search hit\"}]}"
  "Canned JSON the fixture script echoes for the search subcommand.")

(defconst ai-roam-vector-test-embed-line
  "Embed sync complete: indexed=2 unchanged=1 removed=0 parse_failed=0 embed_failed=0"
  "Canned summary the fixture script echoes for the embed subcommand.")

(defun ai-roam-vector-test-write-fixture ()
  "Write an executable canned org-vector fixture script; return its path.
When ORG_VECTOR_ARGS_LOG is set in the environment the fixture also
records its argv there, one argument per line."
  (let ((script (make-temp-file "ai-roam-vector-fixture" nil ".sh")))
    (with-temp-file script
      (insert "#!/bin/sh\n")
      (insert (format "if [ -n \"$ORG_VECTOR_ARGS_LOG\" ]; then printf '%%s\\n' \"$@\" > \"$ORG_VECTOR_ARGS_LOG\"; fi\n"))
      (insert "case \"$1\" in\n")
      (insert (format "  search) printf '%%s\\n' '%s';;\n"
                      ai-roam-vector-test-search-json))
      (insert (format "  embed) echo '%s';;\n" ai-roam-vector-test-embed-line))
      (insert "  *) echo \"unknown subcommand: $1\" >&2; exit 2;;\n")
      (insert "esac\n"))
    (set-file-modes script #o755)
    script))

(defmacro ai-roam-vector-test-with-fixture (&rest body)
  "Run BODY with `ai/roam-vector-command' bound to a canned fixture.
The contract variables are bound to small deterministic values."
  (declare (indent 0))
  `(let ((ai/roam-vector-command (ai-roam-vector-test-write-fixture))
         (ai/roam-vector-org-dir "/tmp/ai-roam-vector-test/org/")
         (ai/roam-vector-db "/tmp/ai-roam-vector-test/db/")
         (ai/roam-vector-top-k 3))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-file ai/roam-vector-command)))))

(ert-deftest ai-roam-vector-contract-defaults ()
  "The defcustom defaults match the documented contract."
  (should (equal ai/roam-vector-command "org-vector"))
  (should (null ai/roam-vector-org-dir))
  (should (equal ai/roam-vector-db "~/.cache/org-vector/"))
  (should (= ai/roam-vector-top-k 8)))

(ert-deftest ai-roam-vector-org-dir-resolved-at-call-time ()
  "A nil `ai/roam-vector-org-dir' falls back to `ai/roam-directory'
at call time, so rebinding the roam root rebinds the indexed dir."
  (let ((ai/roam-vector-org-dir nil)
        (ai/roam-directory "/tmp/ai-roam-vector-test/roam/"))
    (should (equal (ai/roam-vector--org-dir)
                   "/tmp/ai-roam-vector-test/roam/")))
  (let ((ai/roam-vector-org-dir "/tmp/ai-roam-vector-test/other/"))
    (should (equal (ai/roam-vector--org-dir)
                   "/tmp/ai-roam-vector-test/other/"))))

(ert-deftest ai-roam-vector-args-builder-concatenates ()
  "The generic builder prepends the subcommand to the extra flags."
  (should (equal (ai/roam-vector--args "search" "--query" "x")
                 '("search" "--query" "x"))))

(ert-deftest ai-roam-vector-search-args-contract ()
  "Search arguments carry the query, org dir, db path and result count."
  (let ((ai/roam-vector-org-dir "/tmp/ai-roam-vector-test/org/")
        (ai/roam-vector-db "/tmp/ai-roam-vector-test/db/")
        (ai/roam-vector-top-k 3))
    (let ((args (ai/roam-vector--search-args "hydraulic safety")))
      (should (equal (car args) "search"))
      (should (equal (member "--query" args)
                     (list "--query" "hydraulic safety"
                           "--dir" "/tmp/ai-roam-vector-test/org/"
                           "--path" "/tmp/ai-roam-vector-test/db/"
                           "--results" "3"))))))

(ert-deftest ai-roam-vector-embed-args-contract ()
  "Embed arguments carry the org dir and the db path."
  (let ((ai/roam-vector-org-dir "/tmp/ai-roam-vector-test/org/")
        (ai/roam-vector-db "/tmp/ai-roam-vector-test/db/"))
    (let ((args (ai/roam-vector--embed-args)))
      (should (equal (car args) "embed"))
      (should (equal (member "--dir" args)
                     (list "--dir" "/tmp/ai-roam-vector-test/org/"
                           "--path" "/tmp/ai-roam-vector-test/db/")))
      (should (equal (member "--path" args)
                     (list "--path" "/tmp/ai-roam-vector-test/db/"))))))

(ert-deftest ai-roam-vector-search-returns-canned-output ()
  "Search returns the fixture's canned stdout, trimmed."
  (ai-roam-vector-test-with-fixture
    (should (equal (ai/roam-vector-search "hydraulic safety")
                   ai-roam-vector-test-search-json))))

(ert-deftest ai-roam-vector-embed-returns-canned-output ()
  "Embed returns the fixture's canned summary, trimmed."
  (ai-roam-vector-test-with-fixture
    (should (equal (ai/roam-vector-embed)
                   ai-roam-vector-test-embed-line))))

(ert-deftest ai-roam-vector-run-passes-args-to-binary ()
  "The runner hands the builder's arguments to the executable."
  (ai-roam-vector-test-with-fixture
    (let* ((args-log (concat ai/roam-vector-command ".argv"))
           (process-environment
            (cons (format "ORG_VECTOR_ARGS_LOG=%s" args-log)
                  process-environment)))
      (ai/roam-vector-search "grid load")
      (let ((argv (split-string
                   (with-temp-buffer
                     (insert-file-contents args-log)
                     (buffer-string))
                   "\n" t)))
        (should (equal (nth 0 argv) "search"))
        (should (equal (nth 1 argv) "--query"))
        (should (equal (nth 2 argv) "grid load"))
        (should (member "--dir" argv))
        (should (member "--path" argv))
        (should (equal (member "--results" argv) '("--results" "3")))))))

(ert-deftest ai-roam-vector-search-fails-soft-on-nonzero-exit ()
  "A failing binary yields an error string starting with org-vector."
  (let ((ai/roam-vector-command "/bin/false"))
    (let ((result (ai/roam-vector-search "anything")))
      (should (stringp result))
      (should (string-match-p "\\`org-vector" result)))
    (should (string-match-p "\\`org-vector" (ai/roam-vector-embed)))))

(ert-deftest ai-roam-vector-fails-soft-when-binary-missing ()
  "A missing executable yields an error string starting with org-vector."
  (let ((ai/roam-vector-command "/nonexistent/org-vector-nope"))
    (should (string-match-p "\\`org-vector" (ai/roam-vector-search "x")))
    (should (string-match-p "\\`org-vector" (ai/roam-vector-embed)))))

(ert-deftest ai-roam-vector-gptel-tool-names ()
  "The registered gptel tool names match the documented contract."
  (should (equal ai/roam-vector-gptel-tool-search "search_notes_semantic"))
  (should (equal ai/roam-vector-gptel-tool-index "index_notes_embeddings")))

(ert-deftest ai-roam-vector-clear-tool-without-gptel ()
  "The clear helper is a no-op when gptel is not loaded."
  (should (null (ai/roam-vector--clear-gptel-tool "search_notes_semantic"))))

(provide 'ai-roam-vector-test)
;;; ai-roam-vector-test.el ends here
