(defun nsa/wiki/toggle-publish (&optional file)
  "Toggle :PUBLISH: property in org file.
If FILE is provided, operate on that file, otherwise use current buffer."
  (interactive)
  (let ((buffer (if file
                    (find-file-noselect file)
                  (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (if (org-entry-get (point) "PUBLISH")
            (org-delete-property "PUBLISH")
          (org-set-property "PUBLISH" "t")))
      (unless (eq buffer (current-buffer))
        (save-buffer)
        (kill-buffer)))))

(defun nsa/wiki/toggle-directory-publish (directory)
  "Toggle :PUBLISH: property for all org files in DIRECTORY."
  (interactive "DDirectory: ")
  (let ((org-files (directory-files-recursively directory "\\.org$")))
    (dolist (file org-files)
      (nsa/wiki/toggle-publish file))
    (message "Toggled publish state for %d org files" (length org-files))))

(defun wiki/org-publish-to-html (plist filename pub-dir)
  "Only Uplish when the file contains a publish param."
   (when (publishp filename)
       (org-html-publish-to-html plist filename pub-dir)))
