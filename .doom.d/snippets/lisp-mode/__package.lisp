# -*- mode: snippet -*-
# name: __package.lisp
# key:
# --
(in-package :cl-user)
(uiop:define-package :${1:name of package}
  (:use :cl $2)
)
