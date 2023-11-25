;;; normal-elisp.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: October 24, 2023
;; Modified: October 24, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lost-rob0t/normal-elisp
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code

(require 'async)

(defmacro $sh (&rest commands)
  (let ((cmds (mapcar (lambda (cmd)
                        (cond
                         ((listp cmd)
                          (if (and (eq (car cmd) '$)
                                   (listp (cdr cmd)))
                              (concat "$(" (prin1-to-string (cadr cmd)) ")")
                            (mapconcat (lambda (x)
                                         (if (and (symbolp x) (boundp x))
                                             (symbol-value x)
                                           (if (symbolp x) (symbol-name x) x)))
                                       cmd " ")))
                         ((and (symbolp cmd) (boundp cmd))
                          (symbol-value cmd))
                         (t (if (symbolp cmd) (symbol-name cmd) cmd))))
                      commands)))
    `(progn (async-shell-command (concat "sh -c '"
                                         ,(mapconcat 'identity cmds "; ")
                                         "'") "*el-sh*" "*el-sh*"))))
;; I dont want to see the output
(add-to-list 'display-buffer-alist
             '("\\*el-sh\\*.*" . (display-buffer-no-window)))

(provide 'normal-elisp)
;;; normal-elisp.el ends here
