;;; js-helpers.el --- Helpers for js-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <https://github.com/unseen>
;; Maintainer:  <unseen@hunter-02>
;; Created: July 06, 2023
;; Modified: July 06, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/js-helpers
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'async)
(require 'f)
(defun nsaspy/pretify-js-dir ()
  "Beautify all JavaScript files in the current directory."

  (dolist (filename (f-glob "./*.js"))
    (async-start-process "uglyjs" (executable-find "uglifyjs") #'(lambda (buffer) (message "%s is done" filename)) filename "-b" "-o" (format "%s-cleaned.js" filename))))

(provide 'js-helpers)
;;; js-helpers.el ends here
