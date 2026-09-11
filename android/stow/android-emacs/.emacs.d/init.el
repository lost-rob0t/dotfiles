;;; init.el --- Stowed native Android entry point -*- lexical-binding: t; -*-

(unless (boundp 'star/android-stow-root)
  (defconst star/android-stow-root
    (file-name-as-directory
     (expand-file-name
      "android"
      (or (getenv "STAR_DOTFILES_ROOT")
          (expand-file-name "~/.dotfiles"))))))

(load (expand-file-name "init.el" star/android-stow-root) nil nil t)

(unless (equal (getenv "TEMPLE_MODE") "active")
  (add-to-list 'load-path (expand-file-name "lisp" star/android-stow-root))
  (require 'star-android-modern)
  (star/android-modern-initialize))
