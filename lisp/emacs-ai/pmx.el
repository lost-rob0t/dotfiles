;;; pmx.el --- Positron Machine Extensions -*- lexical-binding: t; -*-

(require 'json)
(require 'info)
(require 'orderless)

(defgroup pmx nil
  "Positron Machine Extensions configuration."
  :group 'convenience)

(defcustom pmx-tools '()
  "List of available tools for introspection and interaction."
  :type '(repeat symbol)
  :group 'pmx)

(defun pmx--gptel-symbolp (name)
  "Check if NAME is an interned symbol."
  (add-to-list 'pmx-tools 'symbol_exists)
  (intern-soft name))

(defun pmx--gptel-manual-names ()
  "Return available manual names."
  (add-to-list 'pmx-tools 'manual_names)
  (json-serialize (vconcat (info--filter-manual-names
                            (info--manual-names nil)))))

(provide 'pmx)
;;; pmx.el ends here