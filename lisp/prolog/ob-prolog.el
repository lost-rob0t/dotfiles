;;; ob-prolog.el --- Load upstream ob-prolog with local extensions -*- lexical-binding: t; -*-

(require 'cl-lib)

(let* ((local-directory
        (file-name-as-directory
         (file-truename
          (file-name-directory (or load-file-name buffer-file-name)))))
       (load-path
        (cl-remove-if
         (lambda (directory)
           (and directory
                (condition-case nil
                    (string=
                     local-directory
                     (file-name-as-directory (file-truename directory)))
                  (file-error nil))))
         load-path))
       (upstream-library (locate-library "ob-prolog")))
  (unless upstream-library
    (error "Local ob-prolog loader could not find the upstream ob-prolog package"))
  (load upstream-library nil 'nomessage))

(require 'ob-prolog-async)
(require 'nsa-reload-lisp)

(provide 'ob-prolog)
;;; ob-prolog.el ends here
