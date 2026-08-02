;;; org-agenda-compat.el --- Doom startup compatibility -*- lexical-binding: t; -*-

;;; Commentary:
;; `setq!' validates user options through their Customize widget.  Current
;; org-super-agenda declares `org-super-agenda-groups' too narrowly for valid
;; nested selectors such as :and/:not and selector value lists.  Treat the
;; option as a general Lisp expression before config.el assigns it.

;;; Code:

;;;###autoload
(with-eval-after-load 'org-super-agenda
  (put 'org-super-agenda-groups 'custom-type 'sexp))

(provide '+org-agenda-compat)
;;; org-agenda-compat.el ends here
