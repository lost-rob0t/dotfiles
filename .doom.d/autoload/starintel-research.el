;;; starintel-research.el --- Lazy Starintel research entry point -*- lexical-binding: t; -*-

;;;###autoload
(defun star/research ()
  "Open the Starintel research control surface."
  (interactive)
  (load "research" nil nil)
  (call-interactively #'star/research-dispatch))

;;; starintel-research.el ends here
