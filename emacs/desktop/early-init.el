;;; early-init.el --- Native desktop startup -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil
      package-quickstart nil
      inhibit-startup-screen t
      inhibit-startup-message t
      frame-resize-pixelwise t
      gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 4 1024 1024))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(let ((config-root (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (library '("autoload/native-bootstrap.el"
                     "autoload/native-pdf.el"
                     "autoload/native-tools.el"
                     "autoload/native-session.el"))
    (load (expand-file-name library config-root) nil 'nomessage)))

;;; early-init.el ends here
