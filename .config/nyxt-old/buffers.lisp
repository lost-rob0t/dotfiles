;; Buffer stuffs
(in-package #:nyxt-user)

(define-command exec-all-buffers ()
  (let ((buffers (buffer-list))
        (command (first (nyxt::prompt :prompt "Command/Mode" :sources (make-instance 'user-command-source) :hide-suggestion-count-p t))))
    (setf (nyxt::last-access command) (local-time:now))
    (loop for buffer in buffers
          do (with-current-buffer buffer
               (echo buffer)
               (nyxt::run-async command)))
    ))
