(defpackage #:zara-lisp-broken
  (:use #:cl))

(in-package #:zara-lisp-broken)

(defun broken (x)
  (list x (1+ x))
