# -*- mode: snippet -*-
# name: __.asdf
# key:
# --
(asdf:defsystem :${1:name of package}
  :description "${2:a lisp package}"
  :author "${3:nsaspy}"
  :license "${4:MIT}"
  :version "0.1.0"
  :serial t
  :depends-on (${5:#:alexandria #:serapeum})
  :components ((:file "${6:package.lisp}"
               (:file "$1")
               )))
