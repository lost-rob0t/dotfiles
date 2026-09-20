(in-package #:cl-user)

(defun fail (condition)
  (format *error-output* "Common Lisp reader rejected fixture: ~A~%" condition)
  (sb-ext:exit :code 2))

(let* ((path (car (last sb-ext:*posix-argv*)))
       (*read-eval* nil)
       (eof (gensym "EOF")))
  (handler-case
      (with-open-file (stream path :direction :input)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (declare (ignore form))))
    (condition (condition)
      (fail condition))))

(sb-ext:exit :code 0)
