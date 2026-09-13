;;; inventory.el --- Read the desktop migration baseline -*- lexical-binding: t; -*-
;; Read declarations, never execute the old user's init or its package installer.
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun star-audit-read (file)
  "Read all Lisp forms in FILE; report truncated forms as errors."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (let (forms)
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (push (read (current-buffer)) forms))
      (nreverse forms))))

(defun star-audit-walk (fn form)
  "Visit each cons cell in FORM with FN, including dotted lists."
  (when (consp form)
    (funcall fn form)
    (star-audit-walk fn (car form))
    (star-audit-walk fn (cdr form))))

(let* ((root (file-name-as-directory default-directory))
       (core (getenv "DOOM_AUDIT_CORE"))
       (modules-root (getenv "DOOM_AUDIT_MODULES"))
       (init (cl-find-if (lambda (form) (eq (car-safe form) 'doom!))
                         (star-audit-read ".doom.d/init.el")))
       (category nil) (modules nil) (records nil) (symbols nil))
  (unless (and init core modules-root) (error "Missing baseline or audit inputs"))
  (dolist (entry (cdr init))
    (if (keywordp entry)
        (setq category entry)
      (push (list category (if (consp entry) (car entry) entry)
                  (if (consp entry) (cdr entry) nil)) modules)))
  (setq modules (nreverse modules))
  (let ((sources
         (append (list (expand-file-name "modules/doom/packages.el" core)
                       ".doom.d/packages.el")
                 (mapcar
                  (lambda (module)
                    (expand-file-name
                     (format "modules/%s/%s/packages.el"
                             (substring (symbol-name (car module)) 1)
                             (cadr module)) modules-root)) modules))))
    (dolist (file sources)
      (when (file-exists-p file)
        (dolist (form (star-audit-read file))
          (star-audit-walk
           (lambda (node)
             (when (eq (car node) 'package!)
               (push (list (cadr node) (file-relative-name file root) (cddr node)) records)))
           form)))))
  (princ (format "MODULES %S\n" modules))
  (princ (format "DECLARED-PACKAGE-SUPERSET %S\n"
                 (sort (delete-dups (mapcar #'car records))
                       (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
  (dolist (file (process-lines "git" "ls-files" ".doom.d/*.el" "lisp/*.el"))
    (condition-case err
        (dolist (form (star-audit-read file))
          (star-audit-walk
           (lambda (node)
             (let ((head (car node)))
               (when (and (symbolp head)
                          (string-match-p "!\\'\\|\\`doom[-/]\\|\\`\\+org-\\|\\`\\+popup"
                                          (symbol-name head)))
                 (let ((cell (assq head symbols)))
                   (if cell (cl-pushnew file (cdr cell) :test #'equal)
                     (push (list head file) symbols)))))) form))
      (error (princ (format "EXISTING-PARSE-ERROR %s: %S\n" file err)))))
  (dolist (record (sort symbols (lambda (a b) (string< (symbol-name (car a))
                                                     (symbol-name (car b))))))
    (princ (format "LEGACY-API %S\n" record)))
  (princ "Conditional declarations above are a conservative superset, not a resolved installation.\n"))
