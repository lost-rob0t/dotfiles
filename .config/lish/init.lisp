(in-package :lish-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))
(ql:quickload "usocket")
(ql:quickload "uiop")
(ql:quickload "bt-semaphore")
(ql:quickload "slynk")
(ql:quickload "trivial-sockets")

(print "finished floating, ready to pwn!")

(when (asdf:load-system :slynk)
  (defun start-slynk (&optional (slynk-port 4006))
    (slynk:create-server :port slynk-port :dont-close t)
    (print (format nil "Starting slynk on: ~d" slynk-port))))


(defun print-thread-info ()
  (let* ((curr-thread (bt:current-thread))
         (curr-thread-name (bt:thread-name curr-thread))
         (all-threads (bt:all-threads)))
    (format t "Current thread: ~a~%~%" curr-thread)
    (format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads))
  nil)
