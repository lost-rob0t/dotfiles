;;; early-init.el --- Stowed native Android entry point -*- lexical-binding: t; -*-

(defun star/android-stow--config-root ()
  "Return the Android config root backing this Stow wrapper."
  (file-name-as-directory
   (cond
    ((getenv "STAR_ANDROID_CONFIG_ROOT")
     (expand-file-name (getenv "STAR_ANDROID_CONFIG_ROOT")))
    ((getenv "STAR_DOTFILES_ROOT")
     (expand-file-name "android" (getenv "STAR_DOTFILES_ROOT")))
    (t
     (let ((wrapper (or load-file-name
                        buffer-file-name
                        (expand-file-name "early-init.el" user-emacs-directory))))
       (expand-file-name
        "../../.."
        (file-name-directory (file-truename wrapper))))))))

(defconst star/android-stow-root (star/android-stow--config-root))

(load (expand-file-name "early-init.el" star/android-stow-root) nil nil t)
