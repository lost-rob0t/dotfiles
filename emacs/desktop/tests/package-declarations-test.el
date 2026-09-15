;;; package-declarations-test.el --- Regression tests -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name "../tools/migrate.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "../tools/package-declarations.el" (file-name-directory load-file-name)))

(defmacro star-port-test-with-empty-state (&rest body)
  `(let ((star-port-packages nil)
         (star-port-package-sources nil)
         (star-port-source "test")
         (star-port-modules '((:tools lsp nil)))
         (star-port-current-module '(:tools lsp nil)))
     ,@body))

(ert-deftest star-port-eval-and-compile-metadata-does-not-abort ()
  (star-port-test-with-empty-state
   (star-port-declaration '(eval-and-compile (defvar lsp-use-plists t)))
   (should-not star-port-packages)))

(ert-deftest star-port-eval-and-compile-keeps-nested-package-declarations ()
  (star-port-test-with-empty-state
   (star-port-declaration
    '(eval-and-compile
       (defvar ignored t)
       (package! nested-package :pin "deadbeef")))
   (should (equal (plist-get (alist-get 'nested-package star-port-packages) :pin)
                  "deadbeef"))))

(ert-deftest star-port-unknown-metadata-without-packages-is-safe-to-ignore ()
  (star-port-test-with-empty-state
   (star-port-declaration '(defvar lsp-use-plists t))
   (should-not star-port-packages)))

(ert-deftest star-port-unknown-wrapper-cannot-hide-a-package ()
  (star-port-test-with-empty-state
   (should-error
    (star-port-declaration '(mystery-wrapper (package! hidden-package)))
    :type 'error)))

(ert-deftest star-port-disabled-package-is-removed ()
  (star-port-test-with-empty-state
   (star-port-declaration '(package! sample :pin "one"))
   (star-port-declaration '(package! sample :disable t))
   (should-not (assq 'sample star-port-packages))))

;;; package-declarations-test.el ends here
