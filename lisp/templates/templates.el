;;; template.el --- copy a template dir into a target dir -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup template nil
  "Project template copier."
  :group 'tools)

(defcustom template-root (expand-file-name "~/Templates/")
  "Root directory containing template directories (and/or symlinks)."
  :type 'directory)

(defun template--dir-children (root)
  "Return list of template names under ROOT (dirs or symlinks-to-dirs)."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (unless (file-directory-p root)
      (user-error "Template root does not exist: %s" root))
    (cl-loop for f in (directory-files root t "\\`[^.]")
             for name = (file-name-nondirectory (directory-file-name f))
             for target = (file-truename f)
             when (or (file-directory-p f)
                      (and (file-symlink-p f) (file-directory-p target)))
             collect name)))

(defun template--copy-dir (src dest &optional overwrite)
  "Copy SRC directory into DEST. If OVERWRITE is non-nil, allow overwriting."
  (let* ((src (file-name-as-directory (expand-file-name src)))
         (dest (file-name-as-directory (expand-file-name dest))))
    (unless (file-directory-p src)
      (user-error "Template is not a directory: %s" src))
    (when (and (file-exists-p dest) (not overwrite))
      (user-error "Destination already exists: %s" dest))
    (make-directory (file-name-directory (directory-file-name dest)) t)
    ;; copy-directory args: DIR NEWNAME &optional KEEP-TIME PARENTS COPY-CONTENTS
    (copy-directory src dest t t t)
    dest))

;;;###autoload
(defun template-copy (template-name dest-parent &optional dest-name overwrite)
  "Interactively select a template from `template-root` and copy it.

TEMPLATE-NAME: chosen from subdirs in `template-root`.
DEST-PARENT: parent directory where the new directory will be created.
DEST-NAME: name of the new directory (defaults to TEMPLATE-NAME).
OVERWRITE: if non-nil, overwrite existing DEST."
  (interactive
   (let* ((choices (template--dir-children template-root))
          (tpl (completing-read "Template: " choices nil t))
          (parent (read-directory-name "Copy into (parent dir): " default-directory nil t))
          (name (read-string (format "New dir name (default %s): " tpl) nil nil tpl))
          (ow (y-or-n-p "Overwrite if destination exists? ")))
     (list tpl parent name ow)))
  (let* ((src (expand-file-name template-name (file-name-as-directory template-root)))
         (dest (expand-file-name dest-name (file-name-as-directory dest-parent)))
         (final (template--copy-dir src dest overwrite)))
    (message "Template copied: %s -> %s" src final)
    (when (file-directory-p final)
      (dired final))))

(provide 'template)
;;; template.el ends here
