;;; clear-cache.el --- Clears nim cache -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: September 18, 2023
;; Modified: September 18, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lost-rob0t/clear-cache
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Clears nim cache
;;
;;; Code:

(defun nsa/clear-nim-cache ()
  "Cleare the cache at ~/.cache/nim"
  (interactive)
  (delete-directory (f-join (f-expand "~/.cache") "nim/") t t))

(defun nsa/clear-nim-cache-reinstall ()
  "Clears the nim cache and reinstall the current nimble project.
    This is to fix a nim bug as of <2023-09-18 Mon> that is anoying."
  (interactive)
  (nsa/clear-nim-cache)

  (shell-command "nimble install -y" "*nimble*" "*Messages*"))


(provide 'clear-cache)
;;; clear-cache.el ends here
