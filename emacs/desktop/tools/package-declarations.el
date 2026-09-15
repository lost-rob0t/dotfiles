;;; package-declarations.el --- Safe Doom package declaration reader -*- lexical-binding: t; -*-

;; Loaded after migrate.el.  Keep package inventory evaluation data-only: we
;; never execute arbitrary upstream forms while deciding the native closure.

(defun star-port-contains-package-declaration-p (form)
  "Return non-nil when FORM contains package! or unpin!."
  (cond
   ((atom form) nil)
   ((memq (car-safe form) '(package! unpin!)) t)
   (t (or (star-port-contains-package-declaration-p (car form))
          (star-port-contains-package-declaration-p (cdr form))))))

(defun star-port-declaration-test (form)
  "Resolve a package declaration condition without executing upstream Lisp.

Doom package files sometimes use `package!' as the test of `when'.  Treat that
as a declaration plus a truth value.  Logical forms recurse through this
function so declarations nested in `and', `or', or `not' cannot be missed."
  (cond
   ((eq (car-safe form) 'package!)
    (star-port-declaration form)
    (and (assq (cadr form) star-port-packages) t))
   ((eq (car-safe form) 'and)
    (cl-every #'star-port-declaration-test (cdr form)))
   ((eq (car-safe form) 'or)
    (cl-some #'star-port-declaration-test (cdr form)))
   ((eq (car-safe form) 'not)
    (not (star-port-declaration-test (cadr form))))
   ;; The migration models the Emacs 31 Nix desktop target, not the Ubuntu
   ;; runner's build flags.  Nix's Emacs has native tree-sitter support.
   ((equal form '(treesit-available-p)) t)
   (t (star-port-test form))))

(defun star-port-declaration (form)
  "Collect selected package declarations without executing upstream Lisp.

Unknown metadata forms are ignored only when they cannot hide package! or
unpin!.  Unknown forms containing package declarations fail closed so a Doom
module cannot silently disappear from the native package closure."
  (when (consp form)
    (pcase (car form)
      ('package!
       (let ((name (cadr form))
             (properties (cddr form)))
         (unless (symbolp name)
           (error "Non-symbol package name: %S" form))
         (if (plist-get properties :disable)
             (setq star-port-packages
                   (assq-delete-all name star-port-packages))
           (let ((merged (copy-sequence
                          (alist-get name star-port-packages))))
             (while properties
               (let ((key (pop properties))
                     (value (pop properties)))
                 (setq merged (plist-put merged key value))))
             (setf (alist-get name star-port-packages) merged)
             (cl-pushnew star-port-source
                         (alist-get name star-port-package-sources)
                         :test #'equal)))))
      ('progn
       (mapc #'star-port-declaration (cdr form)))
      ((or 'eval-and-compile 'eval-when-compile)
       ;; These wrappers often define constants consumed by later package!
       ;; properties.  Inventory does not need to execute those definitions,
       ;; but package declarations nested inside the wrapper must still be seen.
       (mapc #'star-port-declaration (cdr form)))
      ('when
       (when (star-port-declaration-test (cadr form))
         (mapc #'star-port-declaration (cddr form))))
      ('unless
       (unless (star-port-declaration-test (cadr form))
         (mapc #'star-port-declaration (cddr form))))
      ('if
       (if (star-port-declaration-test (cadr form))
           (star-port-declaration (caddr form))
         (mapc #'star-port-declaration (cdddr form))))
      ('cond
       (let ((clause
              (cl-find-if
               (lambda (item) (star-port-declaration-test (car item)))
               (cdr form))))
         (mapc #'star-port-declaration (cdr clause))))
      ('unpin!
       (dolist (name (cdr form))
         (setf (alist-get name star-port-packages)
               (plist-put (alist-get name star-port-packages) :pin nil))))
      (_
       (when (star-port-contains-package-declaration-p form)
         (error "Unsupported upstream declaration wrapper: %S" (car form)))))))

(provide 'star-port-package-declarations)
;;; package-declarations.el ends here
