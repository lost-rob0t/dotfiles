;;; termux-bridge-test.el --- Termux startup regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defvar explicit-shell-file-name)

(defmacro star/test-with-termux-environment (&rest body)
  "Run BODY with isolated settings and a real executable fixture.
Do not replace C subrs under a fake PATH: native-comp may spawn a compiler."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "termux-bridge-" t))
          (star/android-termux-prefix (expand-file-name "usr" directory))
          (process-environment (copy-sequence process-environment))
          (exec-path '("/system/bin" nil))
          (shell-file-name "/system/bin/sh")
          (explicit-shell-file-name nil))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "bin" star/android-termux-prefix) t)
           (let ((bash (expand-file-name "bin/bash" star/android-termux-prefix)))
             (with-temp-file bash (insert "#!/bin/sh\nexit 0\n"))
             (set-file-modes bash #o700))
           (setenv "PATH" "/system/bin")
           (setenv "HOME" "/data/data/org.gnu.emacs/files")
           (setenv "LD_LIBRARY_PATH" "keep-library-path")
           (setenv "LD_PRELOAD" "keep-preload")
           ,@body)
       (delete-directory directory t))))

(ert-deftest star/termux-desktop-is-unchanged ()
  (star/test-with-termux-environment
    (let ((before (copy-sequence process-environment)))
      (cl-letf (((symbol-function 'android-p) (lambda () nil)))
        (should-not (star/android-termux-enable)))
      (should (equal before process-environment))
      (should (equal shell-file-name "/system/bin/sh"))
      (should (equal exec-path '("/system/bin" nil))))))

(ert-deftest star/termux-inaccessible-does-not-break-startup ()
  (star/test-with-termux-environment
    (delete-file (expand-file-name "bin/bash" star/android-termux-prefix))
    (let ((before (copy-sequence process-environment)))
      (cl-letf (((symbol-function 'android-p) (lambda () t)))
        (should-not (star/android-termux-enable)))
      (should (equal before process-environment))
      (should-not explicit-shell-file-name))))

(ert-deftest star/termux-bash-and-executable-path-are-wired ()
  (star/test-with-termux-environment
    (cl-letf (((symbol-function 'android-p) (lambda () t)))
      (should (star/android-termux-enable)))
    (should (equal shell-file-name (expand-file-name "bin/bash" star/android-termux-prefix)))
    (should (equal explicit-shell-file-name shell-file-name))
    (should (equal (getenv "SHELL") shell-file-name))
    (should (equal (getenv "PREFIX") star/android-termux-prefix))
    (should (equal (car exec-path) (expand-file-name "bin" star/android-termux-prefix)))))

(ert-deftest star/termux-bridge-preserves-home-and-loader-environment ()
  (star/test-with-termux-environment
    (cl-letf (((symbol-function 'android-p) (lambda () t)))
      (star/android-termux-enable))
    (should (equal (getenv "HOME") "/data/data/org.gnu.emacs/files"))
    (should (equal (getenv "LD_LIBRARY_PATH") "keep-library-path"))
    (should (equal (getenv "LD_PRELOAD") "keep-preload"))))

(ert-deftest star/termux-bridge-is-idempotent ()
  (star/test-with-termux-environment
    (cl-letf (((symbol-function 'android-p) (lambda () t)))
      (star/android-termux-enable)
      (let ((path (getenv "PATH")) (executables (copy-sequence exec-path)))
        (star/android-termux-enable)
        (should (equal path (getenv "PATH")))
        (should (equal executables exec-path))
        (should (memq nil exec-path))))))
