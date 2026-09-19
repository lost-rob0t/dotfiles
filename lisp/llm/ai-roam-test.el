;;; ai-roam-test.el --- Tests for ai-roam -*- lexical-binding: t; -*-

(require 'ert)
(require 'ai-roam)

(defconst ai-roam-test-directory "/home/u/notes/org/roam/"
  "Fake absolute roam root used by the tests; never touched on disk.")

(defmacro ai-roam-test-with-contract (&rest body)
  "Run BODY with the contract-default ai-roam variable bindings."
  (declare (indent 0))
  `(let ((ai/roam-directory ai-roam-test-directory)
         (ai/roam-sections '(starintel osint entities hacking scada ai llm
                                        programming elisp prolog nix android
                                        reading media daily writing meta))
         (ai/roam-section-rights '((hacking . full) (scada . full)
                                   (writing . full) (meta . full)))
         (ai/roam-default-rights 'outline)
         (ai/full-editor-rights nil))
     ,@body))

(ert-deftest ai-roam-contract-defaults ()
  "The defcustom defaults match the documented contract."
  (should (equal ai/roam-directory "~/Documents/Notes/org/roam/"))
  (should (equal ai/roam-sections
                 '(starintel osint entities hacking scada ai llm
                             programming elisp prolog nix android
                             reading media daily writing meta)))
  (should (equal ai/roam-section-rights
                 '((hacking . full) (scada . full)
                   (writing . full) (meta . full))))
  (should (eq ai/roam-default-rights 'outline))
  (should (null ai/full-editor-rights)))

(ert-deftest ai-roam-section-detection ()
  "Files map to their section; roots, non-roam and prefix collisions do not."
  (ai-roam-test-with-contract
    (should (eq (ai/roam-section-for-file
                 "/home/u/notes/org/roam/hacking/foo.org")
                'hacking))
    ;; Nested depth still belongs to the top-level section.
    (should (eq (ai/roam-section-for-file
                 "/home/u/notes/org/roam/osint/sub/deep/bar.org")
                'osint))
    (should-not (ai/roam-section-for-file "/home/u/notes/org/roam/foo.org"))
    (should-not (ai/roam-section-for-file "/home/u/elsewhere/foo.org"))
    (should-not (ai/roam-section-for-file "/home/u/notes/org/roam-evil/x.org"))
    ;; A roam subdirectory that is not a declared section is not a section.
    (should-not (ai/roam-section-for-file
                 "/home/u/notes/org/roam/notasection/x.org"))))

(ert-deftest ai-roam-rights-resolution ()
  "Rights resolve section alist first, then the default, then global toggle."
  (ai-roam-test-with-contract
    ;; No section -> default rights.
    (should (eq (ai/roam-rights "/home/u/elsewhere/foo.org") 'outline))
    (should (eq (ai/roam-rights) 'outline))
    ;; Section with an explicit `full' entry.
    (should (eq (ai/roam-rights "/home/u/notes/org/roam/hacking/tool.org")
                'full))
    ;; Section without an explicit entry falls back to the default.
    (should (eq (ai/roam-rights "/home/u/notes/org/roam/ai/plan.org")
                'outline))
    ;; Global toggle wins everywhere, including unknown sections.
    (let ((ai/full-editor-rights t))
      (should (eq (ai/roam-rights "/home/u/elsewhere/foo.org") 'full))
      (should (eq (ai/roam-rights "/home/u/notes/org/roam/ai/plan.org")
                  'full))
      (should (eq (ai/roam-rights "/home/u/notes/org/roam/meta/scratch.org")
                  'full))
      (should (eq (ai/roam-rights) 'full)))))

(ert-deftest ai-roam-rights-predicates ()
  "The predicates agree with `ai/roam-rights' for every rights outcome."
  (ai-roam-test-with-contract
    (dolist (file '("/home/u/notes/org/roam/hacking/tool.org"
                    "/home/u/notes/org/roam/ai/plan.org"
                    "/home/u/notes/org/roam/writing/draft.org"
                    "/home/u/elsewhere/foo.org"
                    "/home/u/notes/org/roam-evil/x.org"))
      (should (eq (ai/roam-full-p file)
                  (eq (ai/roam-rights file) 'full)))
      (should (eq (ai/roam-outline-p file)
                  (eq (ai/roam-rights file) 'outline))))
    ;; No-argument calls resolve against the default rights.
    (should (ai/roam-outline-p))
    (should-not (ai/roam-full-p))
    (let ((ai/full-editor-rights t))
      (should (ai/roam-full-p))
      (should-not (ai/roam-outline-p)))))

(ert-deftest ai-roam-section-target-path ()
  "Target paths join the roam root, the SECTION dir and the basename."
  (ai-roam-test-with-contract
    (should (equal (ai/roam-section-target-path "/x/y/note.org" 'hacking)
                   "/home/u/notes/org/roam/hacking/note.org"))
    (should (equal (ai/roam-section-target-path "/tmp/other/README.org" 'prolog)
                   "/home/u/notes/org/roam/prolog/README.org"))
    ;; The basename is kept verbatim.
    (should (equal (file-name-nondirectory
                    (ai/roam-section-target-path "/x/y/deep name.org" 'ai))
                   "deep name.org"))
    ;; The result lives under the SECTION directory of the roam root.
    (should (string-prefix-p
             "/home/u/notes/org/roam/scada/"
             (ai/roam-section-target-path "/elsewhere/note.org" 'scada)))))

(ert-deftest ai-roam-toplevel-dirs ()
  "One absolute directory per section, each named after its section."
  (ai-roam-test-with-contract
    (let ((dirs (ai/roam-toplevel-dirs)))
      (should (= (length dirs) (length ai/roam-sections)))
      (dolist (dir dirs)
        (should (file-name-absolute-p dir))
        (should (string-prefix-p ai-roam-test-directory dir)))
      ;; Every section has a matching directory as its last component.
      (dolist (section ai/roam-sections)
        (let ((found nil))
          (dolist (dir dirs)
            (when (equal (file-name-nondirectory (directory-file-name dir))
                         (symbol-name section))
              (setq found t)))
          (should found))))))

(ert-deftest ai-roam-outline-prompt ()
  "The outline prompt names the topic and section and asks for an outline."
  (ai-roam-test-with-contract
    (let ((prompt (ai/roam-outline-prompt "wireless recon workflow" 'hacking)))
      (should (stringp prompt))
      (should (string-match-p (regexp-quote "wireless recon workflow") prompt))
      (should (string-match-p (regexp-quote "hacking") prompt))
      (should (string-match-p "outline" (downcase prompt)))
      (should (string-match-p "org" (downcase prompt))))
    ;; A different section changes the prompt accordingly.
    (should (string-match-p
             (regexp-quote "elisp")
             (ai/roam-outline-prompt "advice composition" 'elisp)))))

(ert-deftest ai-roam-section-rights-override ()
  "Rebinding `ai/roam-section-rights' changes per-section rights."
  (ai-roam-test-with-contract
    ;; Sanity: the contract default leaves the ai/ section on outline.
    (should (eq (ai/roam-rights "/home/u/notes/org/roam/ai/plan.org")
                'outline))
    (let ((ai/roam-section-rights '((ai . full))))
      (should (eq (ai/roam-rights "/home/u/notes/org/roam/ai/plan.org")
                  'full))
      (should (ai/roam-full-p "/home/u/notes/org/roam/ai/plan.org"))
      (should-not (ai/roam-outline-p "/home/u/notes/org/roam/ai/plan.org"))
      ;; Sections absent from the override fall back to the default.
      (should (eq (ai/roam-rights "/home/u/notes/org/roam/hacking/tool.org")
                  'outline)))))

(provide 'ai-roam-test)
;;; ai-roam-test.el ends here
