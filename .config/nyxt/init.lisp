#-quicklisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(ql:quickload :slynk)

(in-package :nyxt)

(when (asdf:load-system :slynk)
  (define-command start-slynk (&optional (slynk-port *swank-port*))
    "Start a Slynk server that can be connected to, for instance, in
Emacs via SLY.

Warning: This allows Nyxt to be controlled remotely, that is, to
execute arbitrary code with the privileges of the user running Nyxt.
Make sure you understand the security risks associated with this
before running this command."
    (slynk:create-server :port slynk-port :dont-close t)
      (echo "Slynk server started at port ~a" slynk-port)))





(in-package #:nyxt-user)


(define-configuration (buffer web-buffer prompt-buffer)
  ((default-modes (append  '(vi-normal-mode)
                          %slot-default%))))

(define-configuration web-buffer
  ((default-modes (append
                   '(auto-mode
                     vi-normal-mode
                     ;; noscript-mode
                     blocker-mode
                     reduce-tracking-mode
                     )
                   %slot-default%))))

(define-configuration browser
  ;; Enable --remote --eval code evaluation.
  ((remote-execution-p t)
   (external-editor-program
    (list "emacsclient" "-cn" "-a" "" "-F"
          "(name . \"floating\")"))))


(define-configuration browser
  ((default-new-buffer-url "http://10.50.50.10:3000")))


(defvar *my-request-resource-handlers* '())

(load-after-system :nx-freestance-handler
                   (nyxt-init-file "freestance.lisp"))

(define-configuration web-buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook
            (mapcar #'make-handler-resource
		    *my-request-resource-handlers*)
            :initial-value %slot-default%))))

(load-after-system :nx-search-engines (nyxt-init-file "search-engines.lisp"))

(nyxt::load-lisp "~/.config/nyxt/theme.lisp")
(nyxt::load-lisp "~/.config/nyxt/proxy.lisp")
(nyxt::load-lisp "~/.config/nyxt/status.lisp")
;; (nyxt::load-lisp "~/.config/nyxt/theme.lisp")
