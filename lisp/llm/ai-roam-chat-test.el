;;; ai-roam-chat-test.el --- Tests for ai-roam-chat -*- lexical-binding: t; -*-

(require 'ert)
(require 'ai-roam-chat)

(defconst ai-roam-chat-test-directory "/home/u/notes/org/roam/"
  "Fake absolute roam root used by the tests; never touched on disk.")

(defmacro ai-roam-chat-test-with-contract (&rest body)
  "Run BODY with the contract-default ai-roam variable bindings."
  (declare (indent 0))
  `(let ((ai/roam-directory ai-roam-chat-test-directory)
         (ai/roam-sections '(starintel osint entities hacking scada ai llm
                                        programming elisp prolog nix android
                                        reading media daily writing meta))
         (ai/roam-section-rights '((hacking . full) (scada . full)
                                   (writing . full) (meta . full)))
         (ai/roam-default-rights 'outline)
         (ai/full-editor-rights nil))
     ,@body))

(ert-deftest ai-roam-chat-context-fields ()
  "Context reports the file, its section, and the resolved rights."
  (ai-roam-chat-test-with-contract
    (let* ((file "/home/u/notes/org/roam/hacking/tool.org")
           (context (ai/roam-chat--context file)))
      (should (equal (plist-get context :file) file))
      (should (eq (plist-get context :section) 'hacking))
      (should (eq (plist-get context :rights) 'full)))
    (let* ((file "/home/u/notes/org/roam/ai/plan.org")
           (context (ai/roam-chat--context file)))
      (should (eq (plist-get context :section) 'ai))
      (should (eq (plist-get context :rights) 'outline)))))

(ert-deftest ai-roam-chat-context-nil-safe ()
  "A nil file yields no section, the default rights, and no crash."
  (ai-roam-chat-test-with-contract
    (let ((context (ai/roam-chat--context)))
      (should (null (plist-get context :file)))
      (should (null (plist-get context :section)))
      (should (eq (plist-get context :rights) 'outline))
      ;; Batch Emacs has no org-roam, so backlink info stays omitted.
      (unless (featurep 'org-roam)
        (should-not (plist-member context :backlinks))))))

(ert-deftest ai-roam-chat-system-message-full-rights ()
  "Full rights name the section, say FULL, and list the roam tools."
  (ai-roam-chat-test-with-contract
    (let ((ai/full-editor-rights t))
      (let ((message (ai/roam-chat--system-message
                      "/home/u/notes/org/roam/hacking/tool.org")))
        (should (stringp message))
        (should (string-match-p "FULL" message))
        (should (string-match-p "\\_<hacking\\_>" message))
        (should (string-match-p "create_roam_id_link" message))
        (should (string-match-p "search_notes_semantic" message))
        (should (string-match-p "index_notes_embeddings" message))))))

(ert-deftest ai-roam-chat-system-message-outline-only ()
  "Outline rights state OUTLINE-ONLY and never grant edit rights."
  (ai-roam-chat-test-with-contract
    (let ((message (ai/roam-chat--system-message
                    "/home/u/notes/org/roam/ai/plan.org")))
      (should (stringp message))
      (should (string-match-p "OUTLINE" message))
      (should (string-match-p "\\_<ai\\_>" message))
      ;; Wording must withhold editing, never grant it.
      (should (string-match-p "must not edit" message))
      (should-not (string-match-p "may edit" message))
      (should-not (string-match-p "FULL" message)))))

(ert-deftest ai-roam-chat-system-message-no-file ()
  "A nil file still composes a message with the default rights."
  (ai-roam-chat-test-with-contract
    (let ((message (ai/roam-chat--system-message)))
      (should (stringp message))
      (should (> (length message) 0))
      (should (string-match-p "no specific roam section" message))
      (should (string-match-p "OUTLINE" message)))))

(ert-deftest ai-roam-chat-buffer-name ()
  "Explicit names win; otherwise the section names the buffer."
  (ai-roam-chat-test-with-contract
    (should (equal (ai/roam-chat--buffer-name "custom") "custom"))
    (should (equal (ai/roam-chat--buffer-name
                    nil "/home/u/notes/org/roam/hacking/tool.org")
                   "*roam: hacking*"))
    (should (equal (ai/roam-chat--buffer-name) "*roam: notes*"))))

(ert-deftest ai-roam-chat-tools-nil-without-gptel ()
  "The tool helper returns nil without crashing when gptel is absent."
  (should (null (ai/roam-chat--tools))))

(provide 'ai-roam-chat-test)
;;; ai-roam-chat-test.el ends here
