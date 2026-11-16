(defpackage #:zara-lisp-fixture
  (:use #:cl)
  (:export #:greet))

(in-package #:zara-lisp-fixture)

#| Reader comments may contain delimiters: ( ) #| nested ( ) |# |#

(defun greet (name)
  (let ((marker #\())
    (declare (ignore marker))
    (format nil "hello ~A (literal)" name)))

#+sbcl
(defparameter *reader-feature* :sbcl)

(defmacro with-value ((name value) &body body)
  `(let ((,name ,value))
     ,@body))
