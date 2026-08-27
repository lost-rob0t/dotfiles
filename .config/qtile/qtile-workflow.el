;;; qtile-workflow.el --- Qtile workflow picker -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/.dotfiles/lisp/qtile"))
(require 'qtile-ui)
(require 'qtile-ui-org)

(defun qtile-workflow--get (key object)
  (or (alist-get key object nil nil #'equal)
      (alist-get (symbol-name key) object nil nil #'equal)
      (alist-get (intern (symbol-name key)) object nil nil #'eq)))

(defun qtile-workflow-open (params)
  "Select a workflow from a shared, widget-anchored Qtile popup."
  (let* ((args (qtile-ui-args params))
         (choices (or (qtile-workflow--get 'choices args) '()))
         (choices (if (vectorp choices) (append choices nil) choices)))
    (switch-to-buffer (get-buffer-create "*Qtile Workflows*"))
    (qtile-ui-prepare-buffer)
    (erase-buffer)
    (qtile-ui-org-heading "WORKFLOWS")
    (qtile-ui-org-muted "Select a desktop workflow to apply.\n\n")
    (let ((selected (completing-read "Workflow: " choices nil t)))
      (prog1 selected
        (qtile-ui-close-current)))))

(defun qtile-workflow-read-right (choices)
  "Compatibility wrapper for callers that still provide CHOICES directly."
  (completing-read "Qtile workflow: " choices nil t))

(provide 'qtile-workflow)
;;; qtile-workflow.el ends here
