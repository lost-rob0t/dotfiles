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


(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '((body
               :background-color "#170c32"
               :color "#f6019d")
              ("#prompt-area"
               :background-color "#170c32")
              ;; The area you input text in.
              ("#input"
               :background-color "#170c32"
               :color "#f3f4f5")

              (".source-name"
               :color "#170c32"
               :background-color "#f6019d")
              (".source-content"
               :background-color "#170c32")
              (".source-content th"
               :border "1px solid #f6019d"
               :background-color "#170c32")
              ;; The currently highlighted option.
              ("#selection"
               :background-color "#92406e"
               :color "black")
              (.marked :background-color "#170c32"
                       :font-weight "bold"
                       :color "#fba922")
              (.selected :background-color "#170c32"
                         :color "#92406e")))))))

(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#170c32"
         :color "#f3f4f5")))))))

(define-configuration status-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '(("#controls"
               :background-color "#92406e"
               :color "#f6019d")
              ("#url"
               :background-color "#fba922"
               :color "#f3f4f5")
              ;; The area you input text in.
              ("#modes"
               :background-color "#dd546e"
               :color "#f3f4f5")
              ("#tabs"
               :background-color "#92406e"
               :color "#f3f4f5")
                     ))))))
(define-configuration (internal-buffer panel-buffer)
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#170c32"
         :color "#f3f4f5")
        (hr
         :color "#170c32")
        (a
         :color "#2de2e6")
        (.button
         :color "#f3f4f5"
         :background-color "#202146")))))))
(define-configuration (message-buffer)
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#170c32"
         :color "#f3f4f5")
        (hr
         :color "#170c32")
        (a
         :color "#2de2e6")
        (.button
         :color "#f3f4f5"
         :background-color "#202146")))))))
(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "#170c32 !important"
                :background-image "none !important"
                :color "#f3f4f5")
               (a
                :background-color "#170c32 !important"
                :background-image "none !important"
                :color "#2de2e6 !important"))))))
