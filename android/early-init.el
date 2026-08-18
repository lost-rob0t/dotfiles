;;; early-init.el --- Native Android Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep startup cheap and expose the paired Termux toolchain to native Emacs.
;; Do not set LD_LIBRARY_PATH: Android Emacs and Termux manage their own native
;; library lookup and only executable discovery is shared here.
;;
;; The Temple Chemacs profile also enters through this directory.  Keep small
;; compatibility shims that must run before Temple package loading here.

;;; Code:

(defun android-p ()
  "Return non-nil when Emacs is running as the native Android port."
  (or (eq system-type 'android)
      (featurep 'android)))

(defconst star/android-termux-prefix "/data/data/com.termux/files/usr")

(defun star/async-sanitize-readable-output (&optional buffer)
  "Make printed opaque objects readable as Lisp in BUFFER.

Older releases of `async' let representations such as `#<process ...>' reach
`async-when-done', which then calls `read' and fails with invalid-read-syntax.
Mirror the sanitization used by current emacs-async before its sentinel reads
child output.  BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "#<\\([^>]*\\)>" nil t)
          (replace-match (concat "(" (match-string 1) ")") t t))
        (goto-char (point-min))
        (while (re-search-forward "#(" nil t)
          (replace-match "(" t t))))))

(defun star/async-sanitize-process-output (proc &rest _ignored)
  "Sanitize PROC output before an older `async-when-done' reads it."
  (let ((buffer (and (processp proc) (process-buffer proc))))
    (when (buffer-live-p buffer)
      (star/async-sanitize-readable-output buffer))))

(with-eval-after-load 'async
  (unless (advice-member-p #'star/async-sanitize-process-output
                           'async-when-done)
    (advice-add 'async-when-done :before #'star/async-sanitize-process-output)))

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