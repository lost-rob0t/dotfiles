;;; native-pdf.el --- Native Doom PDF parity -*- lexical-binding: t; -*-

(defun star-native-pdf-cleanup ()
  "Kill auxiliary pdf-tools buffers associated with the current document."
  (when (bound-and-true-p pdf-annot-list-document-buffer)
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (ignore-errors (pdf-info-close pdf-annot-list-document-buffer))))
  (when (bound-and-true-p pdf-annot-list-buffer)
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer)))
  (when-let ((contents (get-buffer "*Contents*")))
    (kill-buffer contents)))

(defun star-native-pdf-view-setup ()
  (display-line-numbers-mode -1)
  (setq-local evil-normal-state-cursor (list nil))
  (add-hook 'kill-buffer-hook #'star-native-pdf-cleanup nil t))

(defun star-native-pdf-init ()
  "Install the useful runtime behavior from Doom's PDF module."
  (when (require 'pdf-tools nil t)
    (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
    (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
    (setq-default pdf-view-display-size 'fit-page)
    (setq pdf-view-use-scaling t
          pdf-view-use-imagemagick nil)
    ;; This only installs pdf-tools hooks/associations; unlike pdf-tools-install it
    ;; does not unexpectedly compile epdfinfo during startup.
    (when (fboundp 'pdf-tools-install-noverify)
      (pdf-tools-install-noverify))
    (add-hook 'pdf-view-mode-hook #'star-native-pdf-view-setup)
    (with-eval-after-load 'pdf-view
      (define-key pdf-view-mode-map (kbd "q") #'kill-current-buffer))
    (add-to-list
     'display-buffer-alist
     '("^\\*Outline\\*"
       (display-buffer-reuse-window display-buffer-in-side-window)
       (side . right)
       (window-width . 40)))
    (require 'saveplace-pdf-view nil t)))

(add-hook 'after-init-hook #'star-native-pdf-init t)

(provide 'native-pdf)
;;; native-pdf.el ends here
