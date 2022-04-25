;; [[file:config.org::+BEGIN_SRC emacs-lisp][No heading:1]]
;; -*- lexical-binding: t -*-
;; No heading:1 ends here

;; [[file:config.org::*Theme][Theme:1]]
(setq doom-theme 'doom-outrun-electric)
;; Theme:1 ends here

;; [[file:config.org::*Show Line numbers][Show Line numbers:1]]
(setq display-line-numbers-type t)
;; Show Line numbers:1 ends here

;; [[file:config.org::*Magit][Magit:1]]
(map! :leader
      :desc "Push to Remote"
      "g p p" #'magit-push-current-to-pushremote)
;; Magit:1 ends here

;; [[file:config.org::*Org Agenda views][Org Agenda views:1]]
(map! :leader
      :desc "Switch to week view"
      "o a w" #'org-agenda-week-view)

(map! :leader
      :desc "switch to month view"
      "o a m" #'org-agenda-month-view)

(map! :leader
      :desc "switch to month view"
      "o a y" #'org-agenda-year-view)
;; Org Agenda views:1 ends here

;; [[file:config.org::*Org Babel][Org Babel:1]]
(map! :leader
      :desc "Tangle a file"
      "b t" #'org-babel-tangle)
;; Org Babel:1 ends here

;; [[file:config.org::*Yasnippet][Yasnippet:1]]
(map! :leader
      :desc "Add a neew template to yasnippet"
      "a y s" #'+snippets/new)
;; Yasnippet:1 ends here

;; [[file:config.org::*Yasnippet][Yasnippet:2]]
(map! :leader
      :desc "Edit template"
      "a y e" #'+snippets/find)
;; Yasnippet:2 ends here

;; [[file:config.org::*Org structured templates (org tempo)][Org structured templates (org tempo):1]]
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("ni" . "src nim"))
  (add-to-list 'org-structure-template-alist '("erl" . "src erlang"))
  (add-to-list 'org-structure-template-alist '("ss" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp")))
;; Org structured templates (org tempo):1 ends here
