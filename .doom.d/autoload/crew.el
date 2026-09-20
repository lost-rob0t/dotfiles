;;; crew.el --- Namespaced crew entry -*- lexical-binding: t; -*-
;; Generated from lisp/llm/crew.org.
;;;###autoload
(defun +crew/workspace ()
  "Open role-aware ARADR crews without starting model calls."
  (interactive)
  (require 'crew)
  (crew-open))
