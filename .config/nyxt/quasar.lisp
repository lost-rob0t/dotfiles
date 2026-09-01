(in-package #:nyxt-user)

(defparameter *quasar-url* "http://127.0.0.1:5173"
  "Quasar development UI opened by the Nyxt plugin.")

(defun quasar-launch-background ()
  (uiop:launch-program
   '("systemd-run"
     "--user"
     "--collect"
     "--unit=quasar-nyxt"
     "--property=Restart=on-failure"
     "quasar-start")
   :output nil
   :error-output nil))

(define-command-global quasar-open ()
  "Open the Quasar UI without changing its process state."
  (ffi-buffer-load (make-buffer-focus) *quasar-url*))

(define-command-global quasar-start ()
  "Start the pinned Quasar runtime as a user unit and open its UI."
  (quasar-launch-background)
  (quasar-open)
  (echo "Quasar start requested via quasar-nyxt.service."))

(define-command-global quasar-stop ()
  "Stop the Nyxt-managed Quasar user unit."
  (uiop:launch-program '("systemctl" "--user" "stop" "quasar-nyxt.service")
                       :output nil
                       :error-output nil)
  (echo "Quasar stop requested."))

(define-command-global quasar-restart ()
  "Restart the pinned Quasar runtime and open its UI."
  (uiop:launch-program '("systemctl" "--user" "stop" "quasar-nyxt.service")
                       :output nil
                       :error-output nil)
  (quasar-launch-background)
  (quasar-open)
  (echo "Quasar restart requested."))

(define-configuration nyxt/mode/vi:vi-normal-mode
  ((keyscheme-map
    (define-keyscheme-map
     "vi-quasar" (list :import %slot-value%)
     nyxt/keyscheme:vi-normal
     (list
      "g q" 'quasar-start
      "g Q" 'quasar-open)))))
