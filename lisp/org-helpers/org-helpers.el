;;; ../.dotfiles/.doom.d/lisp/org-helpers.el -*- lexical-binding: t; -*-
(require 'org)
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
           (insert "#+filetags: " (mapconcat 'identity tags " ")))))))

(defun nsaspy/add-setup-file-to-org-files (directory setup-file)
  "Add SETUP-FILE after the #+TITLE line in all Org files in DIRECTORY that don't already have it."

  (let ((files (directory-files-recursively directory "\\.org$")))
    (dolist (file files)
      (unless (string-match-p (regexp-quote setup-file) (with-temp-buffer
                                                          (insert-file-contents file)
                                                          (buffer-string)))
        (with-temp-file file
          (insert-file-contents file)
          (goto-char (point-min))
          (if (re-search-forward "^\\(#\\+TITLE:.+\\)$" nil t)
              (progn
                (end-of-line)
                (insert "\n#+SETUPFILE: " setup-file "\n"))
            (error "Title line not found in file %s" file)))))))


(defun nsaspy/fixup-org-css ()
  "Interactivly fix the org css files by adding the setup file."
  (interactive)
  (let ((org-setup (read-file-name "Enter org-setup filename: " org-directory))
        (path (read-directory-name "Enter path: " org-directory)))
    (nsaspy/add-setup-file-to-org-files path org-setup)))
