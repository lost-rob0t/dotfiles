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
