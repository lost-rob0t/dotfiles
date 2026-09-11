;;; early-init.el --- Stowed native Android entry point -*- lexical-binding: t; -*-

(defconst star/android-stow-root
  (file-name-as-directory
   (expand-file-name
    "android"
    (or (getenv "STAR_DOTFILES_ROOT")
        (expand-file-name "~/.dotfiles")))))

(load (expand-file-name "early-init.el" star/android-stow-root) nil nil t)
