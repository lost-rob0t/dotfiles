(in-package #:nyxt-user)

(define-mode zap-mode ()
  "Enable forwarding of all network requests to zap.
As for every mode, it only applies to the current buffer.  If you want to enable
a proxy for all buffers, add it to the list of default modes.
"
  ((proxy (make-instance 'nyxt:proxy
                         :url (quri:uri "http://localhost:8080")
                         :allowlist nil
                         :proxied-downloads-p t)
          :type nyxt:proxy)))


(defmethod enable ((mode zap-mode) &key)
  (if (web-buffer-p (buffer mode))
      (progn
        (setf (nyxt:proxy (buffer mode)) (proxy mode))
        (echo "Buffer ~a proxy set to ~a, allowlisting ~a."
              (id (buffer mode))
              (render-url (url (proxy mode)))
              (allowlist (proxy mode))))
      (echo-warning "You cannot set the proxy for internal buffers.")))

(defmethod disable ((mode zap-mode) &key)
  (when (web-buffer-p (buffer mode))
    (setf (nyxt:proxy (buffer mode)) nil)))

