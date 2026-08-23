;;; init.el --- Termux Doom module selection -*- lexical-binding: t; -*-

(doom!
 :completion
 vertico

 :ui
 doom
 doom-dashboard
 hl-todo
 modeline
 ophints

 :editor
 (evil +everywhere)
 file-templates
 fold
 snippets

 :emacs
 dired
 electric
 undo
 vc

 :term
 eshell

 :checkers
 syntax

 :tools
 (eval +overlay)
 lookup
 magit

 :lang
 common-lisp
 emacs-lisp
 (org +pretty)
 python
 sh

 :config
 (default +bindings +smartparens))
