;;; ai-roam-profiles-test.el --- Tests for ai-roam-profiles -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ai-roam-profiles)

(ert-deftest ai-roam-profiles-names-sorted ()
  "Profile names are sorted symbols including roam and roam-outline."
  (let ((names (ai/roam-profiles-names)))
    (should (symbolp (car names)))
    (should (memq 'roam names))
    (should (memq 'roam-outline names))
    (should (equal (mapcar #'symbol-name names)
                   (sort (mapcar #'symbol-name names) #'string<)))))

(ert-deftest ai-roam-profiles-extract-mention-basic ()
  "A leading @mention yields the profile name and the trimmed rest."
  (should (equal (ai/roam-profiles--extract-mention "@roam hello")
                 '(roam . "hello")))
  (should (equal (ai/roam-profiles--extract-mention "@roam-outline draft it")
                 '(roam-outline . "draft it"))))

(ert-deftest ai-roam-profiles-extract-mention-bare ()
  "A bare @mention yields an empty remainder.
Documented tolerance: the remainder comes back as \"\" or nil."
  (let ((mention (ai/roam-profiles--extract-mention "@ROAM")))
    (should (eq (car mention) 'roam))
    (should (member (cdr mention) '("" nil)))))

(ert-deftest ai-roam-profiles-extract-mention-negative ()
  "Non-mentions return nil."
  (should-not (ai/roam-profiles--extract-mention "@roamish"))
  (should-not (ai/roam-profiles--extract-mention "hello @roam"))
  (should-not (ai/roam-profiles--extract-mention "@"))
  (should-not (ai/roam-profiles--extract-mention "")))

(ert-deftest ai-roam-profiles-extract-mention-leading-whitespace ()
  "Leading whitespace before the @ is tolerated."
  (should (equal (ai/roam-profiles--extract-mention "  @roam-outline  x")
                 '(roam-outline . "x")))
  (should (equal (ai/roam-profiles--extract-mention " @roam hello")
                 '(roam . "hello"))))

(ert-deftest ai-roam-profiles-outline-message ()
  "The fixed outline profile message is a string demanding OUTLINE only."
  (should (stringp ai/roam-profiles-outline-message))
  (should (> (length ai/roam-profiles-outline-message) 0))
  (should (string-match-p "OUTLINE" ai/roam-profiles-outline-message)))

(ert-deftest ai-roam-profiles-definitions-shaped ()
  "Every profile entry is a symbol keyed alist with a description string."
  (should (listp ai/roam-profiles))
  (dolist (entry ai/roam-profiles)
    (should (symbolp (car entry)))
    (should (consp (cdr entry)))
    (should (stringp (alist-get 'description (cdr entry))))
    ;; Function slots exist as keys even when deliberately nil.
    (should (assq 'system-message-fn (cdr entry)))
    (should (assq 'tools-fn (cdr entry)))))

(ert-deftest ai-roam-profiles-apply-requires-gptel ()
  "Applying a profile without gptel fails closed mentioning gptel."
  (when (featurep 'gptel)
    (ert-skip "gptel is loaded; the gptel-less contract is untestable"))
  (let ((err (should-error (ai/roam-profiles-apply 'roam)
                           :type 'user-error)))
    (should (string-match-p "gptel" (error-message-string err)))))

(ert-deftest ai-roam-profiles-send-advice-consumes-mention ()
  "The send advice deletes a leading @mention and applies its profile."
  (let (applied)
    (cl-letf (((symbol-function 'ai/roam-profiles-apply)
               (lambda (name) (setq applied name))))
      (with-temp-buffer
        (insert "@roam remember this")
        (goto-char (point-max))
        (ai/roam-profiles-send-advice)
        (should (eq applied 'roam))
        (should (string= (buffer-string) " remember this"))))
    ;; Without a mention the buffer is untouched and nothing is applied.
    (setq applied nil)
    (cl-letf (((symbol-function 'ai/roam-profiles-apply)
               (lambda (_) (setq applied 'applied))))
      (with-temp-buffer
        (insert "plain prompt")
        (goto-char (point-max))
        (ai/roam-profiles-send-advice)
        (should-not applied)
        (should (string= (buffer-string) "plain prompt"))))))

(ert-deftest ai-roam-profiles-send-advice-scan-stops-at-point ()
  "The advice only sees the current line up to point."
  (let (applied)
    (cl-letf (((symbol-function 'ai/roam-profiles-apply)
               (lambda (_) (setq applied t))))
      (with-temp-buffer
        (insert "@roam hello")
        (goto-char (point-min))
        (ai/roam-profiles-send-advice)
        (should-not applied)
        (should (string= (buffer-string) "@roam hello"))))))

(ert-deftest ai-roam-profiles-setup-no-gptel-is-noop ()
  "Setup without gptel does nothing and does not error."
  (when (featurep 'gptel)
    (ert-skip "gptel is loaded; the gptel-less contract is untestable"))
  (should (null (ai/roam-profiles-setup))))

(provide 'ai-roam-profiles-test)
;;; ai-roam-profiles-test.el ends here
