(in-package :lish-user)

(require :asdf)

(defun star--quicklisp-setup ()
  (let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file setup)
      (load setup))
    (find-package :ql)))

(defun star--quickload (system)
  (handler-case
      (when (or (find-package :ql) (star--quicklisp-setup))
        (funcall (intern "QUICKLOAD" :ql) system :silent t)
        t)
    (serious-condition (condition)
      (format *error-output* "lish: optional system ~a unavailable: ~a~%"
              system condition)
      nil)))

(star--quicklisp-setup)
(star--quickload :uiop)
(star--quickload :lisa)
(star--quickload :prolog-rlm-cl)

(when (member (string-downcase (or (uiop:getenv "STAR_LISH_SLYNK") "0"))
              '("1" "true" "on" "yes")
              :test #'string=)
  (when (star--quickload :slynk)
    (defun start-slynk (&optional (port 4006))
      (slynk:create-server :port port :dont-close t)
      (format t "Slynk listening on ~d~%" port))))

(defun print-thread-info ()
  (if (find-package :bordeaux-threads)
      (let* ((current (bt:current-thread))
             (threads (bt:all-threads)))
        (format t "Current thread: ~a~%All threads:~% ~{~a~%~}" current threads))
      (format t "bordeaux-threads is not loaded.~%"))
  nil)

(let* ((root (or (uiop:getenv "STAR_DOTFILES_ROOT")
                 (namestring (merge-pathnames ".dotfiles/" (user-homedir-pathname)))))
       (agentic (merge-pathnames ".config/lish/agentic.lisp"
                                 (uiop:ensure-directory-pathname root))))
  (when (probe-file agentic)
    (load agentic)))

(format t "Lish ready — Lisp native, Prolog-RLM agentic, LISA expert tools loaded when available.~%")
