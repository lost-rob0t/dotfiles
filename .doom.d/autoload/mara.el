;;; mara.el --- Doom entrypoint for the local Mara runtime -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the loader plumbing invisible.  `M-x mara' loads the local runtime
;; through `fren-loader', which owns Mara's external `load-path' setup.

;;; Code:

;;;###autoload
(defun mara (&optional profile)
  "Load the local Mara runtime and open optional PROFILE."
  (interactive)
  (require 'fren-loader)
  (unless (featurep 'mara)
    (user-error "Mara runtime failed to load from %s"
                (if (boundp 'fren-loader-root)
                    fren-loader-root
                  "~/Documents/mara/")))
  ;; Loading `fren-loader' loads mara.el and replaces this bootstrap function
  ;; with the runtime's real `mara' definition.
  (funcall (symbol-function 'mara) profile))

(provide '+mara-autoloads)
;;; mara.el ends here
