;;; pdf-parity-smoke.el --- Native PDF parity gate -*- lexical-binding: t; -*-

(require 'native-pdf)
(star-native-pdf-init)

(unless (locate-library "pdf-tools")
  (error "PDF PARITY: pdf-tools is missing"))
(unless (locate-library "saveplace-pdf-view")
  (error "PDF PARITY: saveplace-pdf-view is missing"))
(unless (eq (cdr (assoc "\\.pdf\\'" auto-mode-alist)) 'pdf-view-mode)
  (error "PDF PARITY: .pdf files are not associated with pdf-view-mode"))
(unless (eq (cdr (assoc "%PDF" magic-mode-alist)) 'pdf-view-mode)
  (error "PDF PARITY: PDF magic is not associated with pdf-view-mode"))
(unless (eq (default-value 'pdf-view-display-size) 'fit-page)
  (error "PDF PARITY: pdf-view-display-size is not fit-page"))
(unless pdf-view-use-scaling
  (error "PDF PARITY: scaling is disabled"))
(when pdf-view-use-imagemagick
  (error "PDF PARITY: ImageMagick rendering should remain disabled"))
(unless (memq 'star-native-pdf-view-setup pdf-view-mode-hook)
  (error "PDF PARITY: pdf-view setup hook is missing"))
(unless (assoc "^\\*Outline\\*" display-buffer-alist)
  (error "PDF PARITY: PDF outline popup rule is missing"))

(message "native-pdf-parity-smoke-ok")

;;; pdf-parity-smoke.el ends here
