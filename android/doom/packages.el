;;; packages.el --- Termux Doom packages -*- lexical-binding: t; -*-
;; Reuses the desktop package list; edit packages in .doom.d/packages.org.

(require 'seq)

(defun star/termux--desktop-doomdir ()
  "Locate the desktop Doom configuration directory."
  (or (getenv "STAR_DESKTOP_DOOMDIR")
      (and (boundp 'doom-private-dir)
           doom-private-dir
           (let ((sibling (expand-file-name "../../.doom.d" doom-private-dir)))
             (and (file-regular-p (expand-file-name "packages.el" sibling))
                  sibling)))
      (let ((candidates '("~/.dotfiles/.doom.d" "~/dotfiles/.doom.d")))
        (seq-find (lambda (dir)
                    (file-regular-p (expand-file-name "packages.el" dir)))
                  candidates))))

(let ((dir (star/termux--desktop-doomdir)))
  (if dir
      (load (expand-file-name "packages.el" dir) nil 'nomessage)
    (warn "Termux Doom: desktop .doom.d not found; no packages declared")))
