;;; star-android-modern.el --- Modern mobile Org layer -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'tool-bar)

(defgroup star/android-modern nil
  "Modern native Android Emacs behavior."
  :group 'star/android)

(defcustom star/android-modern-packages
  '(evil evil-collection org-modern)
  "Packages required by the native Android interaction layer."
  :type '(repeat symbol)
  :group 'star/android-modern)

(defconst star/android-modern-toolbar-spec
  '((dashboard "Home" star/android-dashboard "Research dashboard")
    (previous-heading "Up" star/android-modern-previous-heading "Previous Org heading")
    (next-heading "Down" star/android-modern-next-heading "Next Org heading")
    (cycle "Fold" star/android-modern-cycle "Cycle Org visibility")
    (open "Open" star/android-modern-open-at-point "Open link or item at point")
    (todo "Todo" star/android-modern-todo "Cycle TODO state")
    (sync "Sync" star/android-sync "Synchronize research repositories")
    (back "Back" star/android-previous-buffer "Previous buffer"))
  "Bottom toolbar actions for the touch-first Android profile.")

(defun star/android-modern--ensure-package (package)
  "Ensure PACKAGE is available using the base Android package helper."
  (cond
   ((package-installed-p package) t)
   ((fboundp 'star/android-ensure-package)
    (star/android-ensure-package package))
   (t
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package))))

(defun star/android-modern-ensure-packages ()
  "Install packages required by the modern Android layer."
  (require 'package)
  (dolist (package star/android-modern-packages)
    (star/android-modern--ensure-package package)))

(defun star/android-modern-previous-heading ()
  "Move to the previous visible Org heading."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-previous-visible-heading 1)
    (previous-line 5)))

(defun star/android-modern-next-heading ()
  "Move to the next visible Org heading."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-next-visible-heading 1)
    (next-line 5)))

(defun star/android-modern-cycle ()
  "Cycle Org visibility or fall back to indentation."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-cycle)
    (indent-for-tab-command)))

(defun star/android-modern-open-at-point ()
  "Open the Org object at point or follow a normal button/link."
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    (org-open-at-point))
   ((button-at (point))
    (push-button))
   (t
    (user-error "Nothing to open at point"))))

(defun star/android-modern-todo ()
  "Cycle the TODO state at the current Org heading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "TODO state is only available in Org buffers"))
  (org-back-to-heading t)
  (org-todo 'right))

(defun star/android-modern--toolbar-button (key label command help)
  "Add toolbar KEY with LABEL invoking COMMAND and HELP text."
  (define-key-after
   tool-bar-map (vector key)
   `(menu-item ,label ,command :help ,help)))

(defun star/android-modern-configure-touch-ui ()
  "Install the native Android touch toolbar and touch interaction defaults."
  (setq tool-bar-position 'bottom
        tool-bar-style 'text
        tool-bar-button-margin 4
        touch-screen-display-keyboard t)
  (when (fboundp 'context-menu-mode)
    (context-menu-mode 1))
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (when (fboundp 'modifier-bar-mode)
    (modifier-bar-mode 1))
  (tool-bar-mode 1)
  (menu-bar-mode 1)
  (setq tool-bar-map (make-sparse-keymap))
  (dolist (entry star/android-modern-toolbar-spec)
    (pcase-let ((`(,key ,label ,command ,help) entry))
      (star/android-modern--toolbar-button key label command help))))

(defun star/android-modern-configure-evil ()
  "Make Vim-style bindings the default interaction model."
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t)
  (require 'evil)
  (evil-mode 1)
  (require 'evil-collection)
  (evil-collection-init)
  (with-eval-after-load 'org
    (evil-define-key 'normal org-mode-map
      (kbd "TAB") #'org-cycle
      (kbd "RET") #'org-open-at-point
      (kbd "gj") #'org-next-visible-heading
      (kbd "gk") #'org-previous-visible-heading)))

(defun star/android-modern-org-visuals ()
  "Enable modern, low-noise Org rendering in the current buffer."
  (visual-line-mode 1)
  (org-indent-mode 1)
  (when (fboundp 'org-modern-mode)
    (org-modern-mode 1))
  (when (featurep 'evil)
    (evil-normal-state)))

(defun star/android-modern-configure-org ()
  "Configure modern Org presentation without replacing built-in Org."
  (require 'org-modern)
  (setq org-modern-star 'replace
        org-modern-hide-stars 'leading
        org-modern-table nil
        org-modern-timestamp t
        org-modern-tag t
        org-modern-priority t
        org-modern-checkbox
        '((?X . "☒")
          (?- . "◫")
          (32 . "☐")))
  (add-hook 'org-mode-hook #'star/android-modern-org-visuals)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (star/android-modern-org-visuals)))))

(defun star/android-modern-initialize ()
  "Initialize the modern native Android interaction layer."
  (star/android-modern-ensure-packages)
  (star/android-modern-configure-evil)
  (star/android-modern-configure-org)
  (star/android-modern-configure-touch-ui))

(provide 'star-android-modern)
;;; star-android-modern.el ends here
