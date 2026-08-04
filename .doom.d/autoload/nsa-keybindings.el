;;; nsa-keybindings.el -*- lexical-binding: t; -*-

(defun nsa/switch-buffer ()
  "Switch buffers with Consult when available."
  (interactive)
  (call-interactively
   (if (fboundp 'consult-buffer)
       #'consult-buffer
     #'switch-to-buffer)))

(defun nsa/swiper-search ()
  "Run Swiper, installing it through Doom's package backend if necessary."
  (interactive)
  (unless (require 'swiper nil t)
    (when (fboundp 'straight-use-package)
      (straight-use-package 'swiper)
      (require 'swiper nil t)))
  (if (fboundp 'swiper-isearch)
      (call-interactively #'swiper-isearch)
    (user-error "Swiper is unavailable; run doom sync after network access is restored")))

(defun nsa/restore-doom-buffer-bindings ()
  "Restore essential Doom buffer bindings and Swiper."
  (map! :leader
        (:prefix ("b" . "buffer")
         :desc "Switch buffer" "b" #'nsa/switch-buffer
         :desc "Switch buffer (builtin)" "B" #'switch-to-buffer
         :desc "Ibuffer" "i" #'ibuffer
         :desc "Kill current buffer" "k" #'kill-current-buffer
         :desc "Next buffer" "n" #'next-buffer
         :desc "Previous buffer" "p" #'previous-buffer
         :desc "Revert buffer" "r" #'revert-buffer
         :desc "Save buffer" "s" #'save-buffer
         :desc "Save all buffers" "S" #'save-some-buffers
         :desc "Kill buffer and window" "x" #'kill-buffer-and-window
         :desc "Bury buffer" "z" #'bury-buffer)
        :desc "Swiper search buffer"
        "s s" #'nsa/swiper-search))

;;;###autoload
(progn
  (add-hook 'after-init-hook #'nsa/restore-doom-buffer-bindings)
  (add-hook 'doom-after-init-hook #'nsa/restore-doom-buffer-bindings))
