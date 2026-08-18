;;; async-compat-test.el --- Tests for async compatibility shim -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest star/async-sanitize-readable-output-process-object ()
  "Opaque process output becomes readable before `async-when-done'."
  (with-temp-buffer
    (insert "#<process ob-async-shell>\n")
    (star/async-sanitize-readable-output)
    (should (equal (buffer-string) "(process ob-async-shell)\n"))
    (goto-char (point-min))
    (should (equal (read (current-buffer))
                   '(process ob-async-shell)))))

(ert-deftest star/async-sanitize-readable-output-hash-list ()
  "Unreadable hash-list syntax is normalized like current emacs-async."
  (with-temp-buffer
    (insert "#(alpha beta)\n")
    (star/async-sanitize-readable-output)
    (should (equal (buffer-string) "(alpha beta)\n"))
    (goto-char (point-min))
    (should (equal (read (current-buffer)) '(alpha beta)))))

(ert-deftest star/async-sanitize-readable-output-widens ()
  "Sanitization covers the complete process buffer even when narrowed."
  (with-temp-buffer
    (insert "prefix\n#<buffer hidden>\nsuffix\n")
    (narrow-to-region (point-min) (line-end-position))
    (star/async-sanitize-readable-output)
    (widen)
    (should-not (string-match-p "#<" (buffer-string)))
    (should (string-match-p "(buffer hidden)" (buffer-string)))))

;;; async-compat-test.el ends here
