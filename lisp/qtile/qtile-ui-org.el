;;; qtile-ui-org.el --- compact Org-inspired Qtile dashboard primitives -*- lexical-binding: t; -*-

(require 'button)

(defface qtile-ui-org-heading
  '((t (:inherit font-lock-function-name-face :weight bold :height 1.2)))
  "Face for a dashboard heading.")
(defface qtile-ui-org-section
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for a dashboard section heading.")
(defface qtile-ui-org-muted
  '((t (:inherit shadow)))
  "Face for muted dashboard metadata.")
(defface qtile-ui-org-value
  '((t (:inherit font-lock-string-face)))
  "Face for dashboard values.")
(defface qtile-ui-org-selected
  '((t (:inherit highlight :extend t :weight bold)))
  "Face for the selected dashboard item.")
(defface qtile-ui-org-warning
  '((t (:inherit warning :weight bold)))
  "Face for dashboard warnings.")
(defface qtile-ui-org-success
  '((t (:inherit success :weight bold)))
  "Face for successful dashboard state.")

(defun qtile-ui-org-heading (text)
  "Insert a dashboard heading containing TEXT."
  (insert (propertize (concat text "\n") 'face 'qtile-ui-org-heading)))

(defun qtile-ui-org-section (text)
  "Insert a section heading containing TEXT."
  (insert (propertize (concat "\n" text "\n") 'face 'qtile-ui-org-section)))

(defun qtile-ui-org-muted (text)
  "Insert muted TEXT."
  (insert (propertize text 'face 'qtile-ui-org-muted)))

(defun qtile-ui-org-value (label value)
  "Insert LABEL and VALUE with a compact dashboard hierarchy."
  (insert (propertize (format "%-14s " label) 'face 'qtile-ui-org-muted))
  (insert (propertize (format "%s\n" value) 'face 'qtile-ui-org-value)))

(defun qtile-ui-org-separator ()
  "Insert the shared dashboard separator."
  (insert (propertize "\n────────────────────────────────────────\n"
                     'face 'qtile-ui-org-muted)))

(defun qtile-ui-org-button (label action &optional face)
  "Insert a clickable LABEL invoking ACTION with FACE."
  (insert-button label
                 'action action
                 'follow-link t
                 'help-echo "Activate this Qtile dashboard action"
                 'face (or face 'button)))

(provide 'qtile-ui-org)
;;; qtile-ui-org.el ends here
