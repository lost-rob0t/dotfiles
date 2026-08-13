;;; init.el --- Minimal native Android Emacs profile -*- lexical-binding: t; -*-

;;; Commentary:
;; The default Android profile is intentionally read-first.  Temple remains
;; available through the existing TEMPLE_MODE=active profile without imposing
;; its development stack on ordinary research review sessions.

;;; Code:

(defconst star/android-config-root
  (file-name-as-directory
   (file-name-directory (or load-file-name buffer-file-name))))

(if (equal (getenv "TEMPLE_MODE") "active")
    (load (expand-file-name "temple/init.el" star/android-config-root)
          nil nil t)
  (progn
    (require 'package)

    (defconst star/android-termux-home
      (if (file-directory-p "/data/data/com.termux/files/home/")
          "/data/data/com.termux/files/home/"
        (file-name-as-directory (expand-file-name "~"))))

    (defconst star/android-state-root
      (expand-file-name ".local/state/emacs-android/"
                        star/android-termux-home))

    (make-directory star/android-state-root t)

    (setq package-enable-at-startup nil
          package-install-upgrade-built-in t
          package-user-dir (expand-file-name "elpa/" star/android-state-root)
          custom-file (expand-file-name "custom.el" star/android-state-root)
          package-archives
          '(("gnu" . "https://elpa.gnu.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("melpa" . "https://melpa.org/packages/")))

    (package-initialize)

    (defun star/android-ensure-package (package)
      "Install PACKAGE when it is not already available."
      (unless (package-installed-p package)
        (unless package-archive-contents
          (package-refresh-contents))
        (package-install package)))

    ;; Only two external packages are part of the default Android profile.
    (dolist (package '(org-roam gptel))
      (star/android-ensure-package package))

    (add-to-list 'load-path (expand-file-name "lisp" star/android-config-root))

    (require 'star-android-reader)
    (star/android-initialize)))

;;; init.el ends here
