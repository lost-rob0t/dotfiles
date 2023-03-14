;;; ../.dotfiles/.doom.d/lisp/org-helpers.el -*- lexical-binding: t; -*-
(require 'org

 (defun get-org-tags ()
    "Get all tags from org headlines in the current buffer and append them to the top of the file under the #+filetags: line."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((tags '()))
        (while (re-search-forward "^*+ .+?\\(:[[:alnum:]_@:]+:\\)" nil t)
          (let ((tag-string (match-string 1)))
            (setq tags (append (split-string (substring tag-string 1 -1) ":" t) tags))))
        (setq tags (delete-dups tags))
        (setq tags (mapcar 'downcase tags))  ; convert tags to lowercase
        (setq tags (delete-dups tags))  ; remove duplicates
        (goto-char (point-min))
        (if (re-search-forward "^#\\+filetags:" nil t)
            (progn
              (beginning-of-line)
              (kill-line)
              (insert "#+filetags: " (mapconcat 'identity tags " ")))
          (progn
            (goto-char (point-min))
            (insert "#+filetags: " (mapconcat 'identity tags " "))))))))
