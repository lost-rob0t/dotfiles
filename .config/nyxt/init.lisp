(in-package :nyxt)


(define-configuration (buffer web-buffer prompt-buffer)
  ((default-modes (append  '(vi-normal-mode)
                          %slot-default%))))

(define-configuration web-buffer
  ((default-modes (append
                   '(auto-mode
                     vi-normal-mode
                     ;; noscript-mode
                     blocker-mode
                     reduce-tracking-mode)
                   %slot-default%))))
;; (nyxt::load-lisp "~/.config/nyxt/theme.lisp")
