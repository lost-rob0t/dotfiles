(in-package #:nyxt-user)

(defparameter *synthwave-theme*
  (make-instance
   'theme:theme
   :background-color "#170c32"
   :primary-color "#202146"
   :secondary-color "#92406e"
   :action-color "#f6019d"
   :success-color "#2de2e6"
   :warning-color "#fba922"
   :highlight-color "#2de2e6")
  "Synthwave palette derived from the archived Nyxt theme.")

(define-configuration browser
  ((theme *synthwave-theme*)))

(define-configuration prompt-buffer
  ((style
    (str:concat
     %slot-value%
     (theme:themed-css
      (theme *browser*)
      '("#prompt-area"
        :border-radius "8px"
        :box-shadow "0 0 16px rgba(246, 1, 157, 0.40)")
      '("#input"
        :box-shadow "inset 0 0 10px rgba(45, 226, 230, 0.12)")
      '("#input:focus"
        :border-color "#2de2e6"
        :box-shadow "inset 0 0 10px rgba(45, 226, 230, 0.16), 0 0 8px rgba(45, 226, 230, 0.20)")
      '(".source-name"
        :text-transform "uppercase"
        :letter-spacing "0.08em")
      '("#selection"
        :background-color "#202146"
        :color "#f3f4f5"
        :border "1px solid #2de2e6"
        :box-shadow "inset 4px 0 0 #2de2e6, 0 0 10px rgba(246, 1, 157, 0.28)")
      '("tr:hover"
        :background-color "#2a285b"
        :color "#f3f4f5")
      '(".marked"
        :background-color "#3a285d"
        :color "#fba922"
        :box-shadow "inset 4px 0 0 #fba922")
      '(".selected"
        :background-color "#202146"
        :color "#f3f4f5"))))))

(define-configuration status-buffer
  ((height 40)
   (style
    (str:concat
     %slot-value%
     (theme:themed-css
      (theme *browser*)
      '(body
        :background "linear-gradient(90deg, #170c32 0%, #202146 58%, #170c32 100%)")
      '("#container"
        :gap "2px")
      '("#controls"
        :display "none")
      '("#url"
        :background-color "#170c32"
        :color "#2de2e6"
        :border "1px solid #92406e"
        :border-radius "7px"
        :box-shadow "inset 0 0 9px rgba(45, 226, 230, 0.12)")
      '("#tabs"
        :padding-left "0")
      '(".tab"
        :border "1px solid transparent"
        :border-radius "7px"
        :transition "background-color 120ms ease, color 120ms ease, box-shadow 120ms ease")
      '(".selected-tab"
        :background-color "#f6019d"
        :color "#170c32"
        :border-color "#2de2e6"
        :box-shadow "0 0 10px rgba(246, 1, 157, 0.35)")
      '("#modes"
        :background-color "#202146"
        :color "#fba922"
        :border "1px solid #92406e"
        :border-radius "7px"
        :box-shadow "inset 0 0 8px rgba(246, 1, 157, 0.10)"))))))

(define-configuration (internal-buffer panel-buffer message-buffer)
  ((style
    (str:concat
     %slot-value%
     (theme:themed-css
      (theme *browser*)
      '(body
        :background-color "#170c32"
        :color "#f3f4f5")
      '(a
        :color "#2de2e6")
      '("a:visited"
        :color "#92406e")
      '("a:hover"
        :color "#f6019d")
      '("a:active"
        :color "#f6019d")
      '(hr
        :border-color "#92406e")
      '(".button"
        :background-color "#202146"
        :color "#f3f4f5"
        :border-radius "6px"))))))
