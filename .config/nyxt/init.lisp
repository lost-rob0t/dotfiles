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
                     reduce-tracking-mode)
                   %slot-default%))))

(define-configuration browser
  ;; Enable --remote --eval code evaluation.
  ((remote-execution-p t)
   (external-editor-program
    (list "emacsclient" "-cn" "-a" "" "-F"
          "(name . \"floating\")"))))

;(define-configuration password:keepassxc-interface
;  ((password:password-file interface "/home/unseen/Documents/encrypted/Passwords.kdbx")))
;(defmethod initialize-instance :after ((interface password:keepassxc-interface) &key &allow-other-keys)
;  (setf (password:password-file interface) "/home/unseen/Documents/encrypted/Passwords.kdbx"))

;; source: https://github.com/aartaka/nyxt-config/blob/master/init.lisp
(defmacro load-after-system* (system &optional file)
  "Helper macro to load configuration for extensions.
Loads a newly-generated ASDF system depending on SYSTEM.
FILE, if provided, is loaded after the generated system successfully
loads."
  #+nyxt-2
  `(load-after-system ,system ,@(when file
                                  `((nyxt-init-file ,(if (str:ends-with-p ".lisp" file)
                                                         file
                                                         (str:concat file ".lisp"))))))
  #+nyxt-3
  `(define-nyxt-user-system-and-load ,(gensym "NYXT-USER/")
     :depends-on (,system) ,@(when file
                               `(:components (,file)))))



;; Source: https://github.com/dracula/nyxt/blob/master/statusline.lisp
(define-configuration status-buffer ((glyph-mode-presentation-p t)))
(define-configuration nyxt/force-https-mode:force-https-mode ((glyph "")))
(define-configuration nyxt/blocker-mode:blocker-mode ((glyph "")))
(define-configuration nyxt/proxy-mode:proxy-mode ((glyph "")))
(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode  ((glyph "")))
(define-configuration nyxt/certificate-exception-mode:certificate-exception-mode ((glyph "")))
(define-configuration nyxt/style-mode:style-mode ((glyph "")))
(define-configuration nyxt/help-mode:help-mode ((glyph "")))
(define-configuration nyxt/web-mode:web-mode ((glyph "ω")))
(define-configuration nyxt/auto-mode:auto-mode ((glyph "α")))


;(define-configuration browser
;  ((theme (make-instance
;           'theme:theme
;           :dark-p t
;           :background-color "#170c32"
;           :text-color "#f3f4f5"
;           :accent-color "#f6019d"
;           :primary-color "#fba922"
;           :secondary-color "#dd546e"
;           :tertiary-color "#202146"
;           :quaternary-color "#92406e"))))

;; Custom Dark-mode for webpages
;(define-configuration nyxt/style-mode:dark-mode
;  ((style #.(cl-css:css
;             '((*
;                :background-color "#170c32 !important"
;                :background-image "none !important"
;                :color "#f3f4f5")
;               (a
;                :background-color "#170c32 !important"
;                :background-image "none !important"
;                :color "#2de2e6 !important"))))))


;;(nyxt::load-lisp "~/.config/nyxt/theme.lisp")
;; (nyxt::load-lisp "~/.config/nyxt/theme.lisp")
