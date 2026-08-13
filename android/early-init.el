;;; early-init.el --- Native Android Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep startup cheap and expose the paired Termux toolchain to native Emacs.
;; Do not set LD_LIBRARY_PATH: Android Emacs and Termux manage their own native
;; library lookup and only executable discovery is shared here.

;;; Code:

(defun android-p ()
  "Return non-nil when Emacs is running as the native Android port."
  (or (eq system-type 'android)
      (featurep 'android)))

(defconst star/android-termux-prefix "/data/data/com.termux/files/usr")

(when (android-p)
  (let ((bin (expand-file-name "bin" star/android-termux-prefix)))
    (setenv "PATH" (concat bin path-separator (or (getenv "PATH") "")))
    (add-to-list 'exec-path bin))

  (setq android-use-legacy-external-storage nil
        touch-screen-display-keyboard t
        read-process-output-max (* 1024 1024)
        gc-cons-threshold (* 64 1024 1024)
        gc-cons-percentage 0.2
        package-enable-at-startup nil
        frame-inhibit-implied-resize t
        inhibit-startup-message t
        inhibit-startup-screen t
        initial-scratch-message nil)

  (let ((handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook
     'emacs-startup-hook
     (lambda ()
       (setq file-name-handler-alist handlers
             gc-cons-threshold (* 16 1024 1024)
             gc-cons-percentage 0.1)))))

;;; early-init.el ends here
