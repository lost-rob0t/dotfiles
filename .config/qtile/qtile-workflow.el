;;; qtile-workflow.el --- right-aligned Qtile workflow picker -*- lexical-binding: t; -*-

(defun qtile-workflow-read-right (choices)
  "Select one Qtile workflow from CHOICES in a top-right utility frame."
  (let ((frame (make-frame '((name . "qtile-workflow")
                             (title . "qtile-workflow")
                             (width . 58)
                             (height . 10)
                             (minibuffer . t)
                             (left . 1.0)
                             (top . 0.0)
                             (user-position . t)))))
    (unwind-protect
        (with-selected-frame frame
          (select-frame-set-input-focus frame)
          (completing-read "Qtile workflow: " choices nil t))
      (when (frame-live-p frame)
        (delete-frame frame)))))

(provide 'qtile-workflow)
;;; qtile-workflow.el ends here
