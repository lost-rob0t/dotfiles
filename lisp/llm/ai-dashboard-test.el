;;; ai-dashboard-test.el --- Tests for programmable AI dashboard -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ai-dashboard)

(defmacro ai/dashboard-test--with-state (&rest body)
  `(let ((ai/dashboard-state-file (make-temp-file "ai-dashboard-" nil ".json")))
     (unwind-protect
         (progn
           (delete-file ai/dashboard-state-file)
           ,@body)
       (when (file-exists-p ai/dashboard-state-file)
         (delete-file ai/dashboard-state-file)))))

(ert-deftest ai/dashboard-rejects-unknown-actions ()
  (ai/dashboard-test--with-state
   (should-error
    (ai/dashboard-upsert-card "unsafe" "Unsafe" "Nope" "eval" "(message \"no\")")
    :type 'user-error)))

(ert-deftest ai/dashboard-upsert-persists-typed-card ()
  (ai/dashboard-test--with-state
   (ai/dashboard-upsert-card
    "zara-work" "Zara work" "Open the canonical Zara client." "zara" nil)
   (let ((cards (ai/dashboard--read-cards)))
     (should (= (length cards) 1))
     (should (equal (alist-get 'id (car cards)) "zara-work"))
     (should (equal (alist-get 'action (car cards)) "zara")))))

(ert-deftest ai/dashboard-upsert-replaces-card-by-id ()
  (ai/dashboard-test--with-state
   (ai/dashboard-upsert-card "work" "Old" "one" "agenda" nil)
   (ai/dashboard-upsert-card "work" "New" "two" "chat" nil)
   (let ((cards (ai/dashboard--read-cards)))
     (should (= (length cards) 1))
     (should (equal (alist-get 'title (car cards)) "New"))
     (should (equal (alist-get 'action (car cards)) "chat")))))

(ert-deftest ai/dashboard-remove-card-is-idempotent ()
  (ai/dashboard-test--with-state
   (ai/dashboard-upsert-card "work" "Work" "one" "agenda" nil)
   (ai/dashboard-remove-card "work")
   (ai/dashboard-remove-card "work")
   (should-not (ai/dashboard--read-cards))))

(ert-deftest ai/dashboard-invokes-only-typed-ideas-action ()
  (let ((opened nil)
        (ai/dashboard-ideas-file "/tmp/ideas.org"))
    (cl-letf (((symbol-function 'find-file)
               (lambda (path) (setq opened path))))
      (ai/dashboard--invoke-card
       '((id . "ideas") (title . "Ideas") (body . "Capture") (action . "ideas") (argument)))
      (should (equal opened "/tmp/ideas.org")))))

(ert-deftest ai/dashboard-buffer-renders-custom-card ()
  (ai/dashboard-test--with-state
   (ai/dashboard-upsert-card
    "research" "Research cockpit" "Resume the research flow." "chat" nil)
   (let ((buffer (ai/dashboard-buffer)))
     (unwind-protect
         (with-current-buffer buffer
           (should (string-match-p "Research cockpit" (buffer-string)))
           (should (derived-mode-p 'ai-dashboard-mode)))
       (kill-buffer buffer)))))

(provide 'ai-dashboard-test)
;;; ai-dashboard-test.el ends here
