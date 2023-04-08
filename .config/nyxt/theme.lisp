(in-package #:nyxt-user)


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
(define-configuration browser
  ((theme (make-instance
           'theme:theme
           :dark-p t
           :background-color "black"
           :text-color "white"
           :accent-color "#CD5C5C"
           :primary-color "rgb(170, 170, 170)"
           :secondary-color "rgb(140, 140, 140)"
           :tertiary-color "rgb(115, 115, 115)"
           :quaternary-color "rgb(85, 85, 85)"))))

;;; Dark-mode is a simple mode for simple HTML pages to color those in
;;; a darker palette. I don't like the default gray-ish colors,
;;; though. Thus, I'm overriding those to be a bit more laconia-like.
(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "black !important"
                :background-image "none !important"
                :color "white")
               (a
                :background-color "black !important"
                :background-image "none !important"
                :color "#556B2F !important"))))))
;;For version 2.2.4 AND below use this:
