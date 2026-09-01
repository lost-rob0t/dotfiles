(in-package #:nyxt-user)

(define-configuration status-buffer
  ((glyph-mode-presentation-p t)))

(define-configuration :force-https-mode
  ((glyph "ϕ")))

(define-configuration :user-script-mode
  ((glyph "u")))

(define-configuration :blocker-mode
  ((glyph "β")))

(define-configuration :proxy-mode
  ((glyph "π")))

(define-configuration :reduce-tracking-mode
  ((glyph "τ")))

(define-configuration :certificate-exception-mode
  ((glyph "χ")))

(define-configuration :style-mode
  ((glyph "ϕ")))

(define-configuration :cruise-control-mode
  ((glyph "σ")))

(defmethod format-status-load-status ((status status-buffer))
  "Display a compact loading state."
  (declare (ignore status))
  (spinneret:with-html-string
    (:span
     (if (and (current-buffer)
              (web-buffer-p (current-buffer)))
         (case (slot-value (current-buffer) 'nyxt::status)
           (:unloaded "∅")
           (:loading "∞")
           (:finished ""))
         ""))))
