;;; claude-code.el --- Run claude-code in vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a function to run claude-code in a vterm buffer.

;;; Code:

(require 'vterm)

(defun claude-code ()
  "Run claude-code in a new vterm buffer, splitting window below."
  (interactive)
  (let* ((buffer-name "*claude-code*")
         (existing-buffer (get-buffer buffer-name)))
    ;; If buffer exists, just switch to it
    (if existing-buffer
        (progn
          (split-window-below)
          (other-window 1)
          (switch-to-buffer existing-buffer))
      ;; Create new vterm buffer
      (split-window-below)
      (other-window 1)
      (let ((vterm-shell "nix run nixpkgs#claude-code --impure"))
        (vterm buffer-name)))))

(provide 'claude-code)
;;; claude-code.el ends here
