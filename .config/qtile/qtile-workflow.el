;;; qtile-workflow.el --- Qtile workflow picker -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "~/.dotfiles/lisp/qtile"))
(require 'qtile-ui)
(require 'qtile-ui-org)

(defconst qtile-workflow-cancel-label "[Cancel]"
  "Completion choice used to leave the workflow picker without applying one.")

(defun qtile-workflow--get (key object)
  (or (alist-get key object nil nil #'equal)
      (alist-get (symbol-name key) object nil nil #'equal)
      (alist-get (intern (symbol-name key)) object nil nil #'eq)))

(defun qtile-workflow-open (params)
  "Select a workflow from a shared, widget-anchored Qtile popup."
  (let* ((args (qtile-ui-args params))
         (choices (or (qtile-workflow--get 'choices args) '()))
         (choices (if (vectorp choices) (append choices nil) choices))
         (default (or (qtile-workflow--get 'default args) (car choices)))
         (picker-choices (append choices (list qtile-workflow-cancel-label))))
    (switch-to-buffer (get-buffer-create "*Qtile Workflows*"))
    (qtile-ui-prepare-buffer)
    (erase-buffer)
    (qtile-ui-org-heading "WORKFLOWS")
    (qtile-ui-org-muted
     (format "Select a desktop workflow. Default: %s. Escape/C-g cancels.\n\n"
             default))
    (condition-case nil
        (let ((selected
               (completing-read "Workflow: " picker-choices nil t
                                default nil default)))
          (if (equal selected qtile-workflow-cancel-label)
              (progn
                (qtile-ui-close-current)
                nil)
            (prog1 selected
              (qtile-ui-close-current))))
      (quit
       (qtile-ui-close-current)
       nil))))

(defun qtile-workflow-read-right (choices)
  "Compatibility wrapper for callers that still provide CHOICES directly."
  (completing-read "Qtile workflow: " choices nil t))

(provide 'qtile-workflow)
;;; qtile-workflow.el ends here
