(in-package #:nyxt-user)

;; Beter load status
;; source: https://github.com/aartaka/nyxt-config/blob/master/status.lisp
(defmethod format-status-load-status ((status status-buffer))
  (spinneret:with-html-string
    (:span (if (web-buffer-p (current-buffer))
               (case (slot-value (current-buffer) 'nyxt::status)
                 (:unloaded "∅")
                 (:loading "∞")
                 (:finished ""))
               ""))))
