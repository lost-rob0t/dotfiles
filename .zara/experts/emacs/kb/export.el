;;; export.el --- Clean batch Emacs self-documentation export -*- lexical-binding: t; -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'json)

(defun zara-emacs-kb--row (symbol kind)
  "Describe SYMBOL's KIND without exporting its value or expanding keys."
  (let* ((doc (cond ((equal kind "function") (documentation symbol t))
                    ((equal kind "variable")
                     (documentation-property symbol 'variable-documentation t))
                    ((equal kind "face")
                     (documentation-property symbol 'face-documentation t))
                    (t (error "Unsupported symbol kind"))))
         (library (symbol-file symbol)))
    (unless (or (null doc) (stringp doc))
      (error "Invalid documentation for %s" symbol))
    `((type . "symbol")
      (name . ,(symbol-name symbol))
      (kind . ,kind)
      (interactive . ,(if (and (equal kind "function") (commandp symbol))
                          t :json-false))
      (doc . ,(if (null doc) :json-null (substring-no-properties doc)))
      (library . ,(if library (file-name-nondirectory library) "<runtime>")))))

(defun zara-emacs-kb--emit (row)
  "Write ROW as one UTF-8 JSON line to standard output."
  (let ((json-encoding-pretty-print nil)
        (json-false :json-false)
        (json-null :json-null))
    (princ (json-encode row))
    (terpri)))

(defun zara-emacs-kb-export-batch ()
  "Export boot-loaded documentation, not private session or complete source state.
Run only using emacs -Q --batch.  ZARA_EMACS_KB_SOURCE identifies the
pinned Emacs closure; the build supplies it, not a model request."
  (unless (and noninteractive (null user-init-file))
    (error "Run the exporter in a clean emacs -Q --batch process"))
  (let ((source (getenv "ZARA_EMACS_KB_SOURCE"))
        (coding-system-for-write 'utf-8-unix)
        symbols)
    (unless (and source (> (length source) 0))
      (error "ZARA_EMACS_KB_SOURCE is required"))
    (mapatoms (lambda (symbol)
                (unless (or (keywordp symbol)
                            (string-prefix-p "zara-emacs-kb-" (symbol-name symbol)))
                  (push symbol symbols))))
    (setq symbols (sort symbols (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
    (when (> (length symbols) 100000)
      (error "Symbol inventory exceeds build ceiling"))
    (zara-emacs-kb--emit
     `((type . "corpus") (schema . 1) (emacs_version . ,emacs-version)
       (source_id . ,source) (profile . "core-loaded")))
    (dolist (symbol symbols)
      (when (fboundp symbol)
        (zara-emacs-kb--emit (zara-emacs-kb--row symbol "function")))
      (when (or (boundp symbol) (get symbol 'variable-documentation))
        (zara-emacs-kb--emit (zara-emacs-kb--row symbol "variable")))
      (when (facep symbol)
        (zara-emacs-kb--emit (zara-emacs-kb--row symbol "face"))))))

(provide 'zara-emacs-kb-export)
;;; export.el ends here
