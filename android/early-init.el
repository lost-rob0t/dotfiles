;;; early-init.el --- Android Emacs early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Early initialization for Android Emacs app
;; Provides Termux integration and performance optimizations

;;; Code:

;; Android detection
(defun android-p ()
  "Return t if Emacs is running on Android, nil otherwise."
  (or (eq system-type 'android)
      (featurep 'android)))

;; Termux integration - Add Termux binaries to PATH
(when (android-p)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
                         (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path)

  ;; Android-specific optimizations
  (setq android-use-legacy-external-storage nil)
  (setq touch-screen-display-keyboard t)

  ;; Performance optimizations for mobile
  (setq gc-cons-threshold (* 50 1000 1000))  ; 50MB
  (setq read-process-output-max (* 1024 1024)) ; 1MB

  ;; Reduce startup time
  (setq package-enable-at-startup nil)
  (setq file-name-handler-alist-original file-name-handler-alist)
  (setq file-name-handler-alist nil)

  ;; Restore after startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist file-name-handler-alist-original)
              (setq gc-cons-threshold (* 2 1000 1000))))) ; 2MB

;; Set user-emacs-directory to point to dotfiles repo
(when (android-p)
  (let ((dotfiles-android-path "/storage/emulated/0/Android/data/com.termux/files/dotfiles/android/temple"))
    (when (file-directory-p dotfiles-android-path)
      (setq user-emacs-directory (file-name-as-directory dotfiles-android-path)))))

;;; early-init.el ends here