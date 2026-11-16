;;; valid.el --- Zara Emacs Lisp fixture -*- lexical-binding: t; -*-

;;;###autoload
(defun zara-fixture-greet (name)
  "Return a deterministic greeting for NAME."
  (let ((paren ?\())
    (ignore paren)
    (format "hello %s (literal)" name)))

(defgroup zara-fixture nil
  "Fixture group."
  :group 'tools)

(defcustom zara-fixture-enabled t
  "Whether the fixture is enabled."
  :type 'boolean
  :group 'zara-fixture)

(define-minor-mode zara-fixture-mode
  "Tiny deterministic fixture mode."
  :lighter " ZF")

(provide 'zara-fixture)
