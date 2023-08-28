;;; cl-helpers.el --- Common Lisp Helpers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: June 02, 2023
;; Modified: June 02, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lost-rob0t/dotfiles
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Common Lisp Helpers
;;
;;; Code:

(defun nsaspy/rev-sexp-string (sexp)
  "reverse the sexp and insert car as a string"
    (when (consp sexp))
    (let ((symbol (nth 0 sexp)))
      (list (symbol-name symbol) symbol)))
(defun nsaspy/rev-sexp-region (begin end)
  "reverse the sexp and insert car as a string from the selected region and process each cons cell."
  (interactive "r")
  (when (region-active-p)
    (let* ((str (buffer-substring begin end))
           (lines (split-string str "\n" t)) ;; Split the string into lines
           result)
      (dolist (line lines)
        (let ((sexp (read line)))
          (when (consp sexp)
            (let ((rev-sexp (nsaspy/rev-sexp-string sexp)))
              (push rev-sexp result)))))
      (kill-new (mapconcat 'prin1-to-string (reverse result) "\n")))))


;; Are you on nixos? Do you use direnv and nix-shell/flakes?
;; If so this function is for you!
;; Reload the direnv enviroment, and restart lisp
;; Replace envrc/sly with your setup
(defun nsa/reload-lisp-env ()
  "Reload lisp enviroment after changing the nix paths/envrc."
  (interactive)
  (envrc-reload)
  (sly-restart-inferior-lisp))

(provide 'cl-helpers)
;;; cl-helpers.el ends here
