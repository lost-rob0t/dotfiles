;;; doom-ux.el --- Doom-equivalent native UX -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup star-doom-ux nil
  "Doom-compatible UX for the native desktop profile."
  :group 'convenience)

(defcustom star-doom-theme 'doom-outrun-electric
  "Theme used by the native desktop profile."
  :type 'symbol
  :group 'star-doom-ux)

(defun star-doom--call (primary fallback)
  (cond
   ((fboundp primary) (call-interactively primary))
   ((fboundp fallback) (call-interactively fallback))
   (t (user-error "Neither %s nor %s is available" primary fallback))))

(defun star-doom/find-file ()
  (interactive)
  (star-doom--call 'counsel-find-file 'find-file))

(defun star-doom/recent-file ()
  (interactive)
  (star-doom--call 'counsel-recentf 'recentf-open-files))

(defun star-doom/switch-buffer ()
  (interactive)
  (star-doom--call 'ivy-switch-buffer 'switch-to-buffer))

(defun star-doom/switch-buffer-other-window ()
  (interactive)
  (star-doom--call 'ivy-switch-buffer-other-window 'switch-to-buffer-other-window))

(defun star-doom/project-switch ()
  (interactive)
  (star-doom--call 'projectile-switch-project 'project-switch-project))

(defun star-doom/project-find-file ()
  (interactive)
  (star-doom--call 'projectile-find-file 'project-find-file))

(defun star-doom/project-search ()
  (interactive)
  (cond
   ((fboundp 'counsel-projectile-rg) (call-interactively #'counsel-projectile-rg))
   ((fboundp 'projectile-ripgrep) (call-interactively #'projectile-ripgrep))
   ((fboundp 'project-find-regexp) (call-interactively #'project-find-regexp))
   (t (user-error "No project search command is available"))))

(defun star-doom/search-buffer ()
  (interactive)
  (star-doom--call 'swiper 'isearch-forward))

(defun star-doom/imenu ()
  (interactive)
  (star-doom--call 'counsel-imenu 'imenu))

(defun star-doom/vterm-popup ()
  (interactive)
  (if (fboundp 'vterm)
      (let ((display-buffer-overriding-action
             '((display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.32))))
        (call-interactively #'vterm))
    (user-error "vterm is unavailable")))

(defun star-doom/eshell-popup ()
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-height . 0.32))))
    (eshell t)))

(defun star-doom/open-config ()
  (interactive)
  (find-file (expand-file-name "personal.org" star-config-directory)))

(defun star-doom/open-init ()
  (interactive)
  (find-file (expand-file-name "init.org" star-config-directory)))

(defun star-doom/open-ux ()
  (interactive)
  (find-file (expand-file-name "doom-ux.org" star-config-directory)))

(defun star-doom/org-agenda ()
  (interactive)
  (require 'org-agenda)
  (org-agenda nil "a"))

(defun star-doom/org-capture ()
  (interactive)
  (require 'org-capture)
  (call-interactively #'org-capture))

(defun star-doom/org-roam-find ()
  (interactive)
  (star-doom--call 'org-roam-node-find 'find-file))

(defun star-doom--popup-rule (regexp height)
  (add-to-list
   'display-buffer-alist
   `(,regexp
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)
     (window-height . ,height)
     (window-parameters . ((no-other-window . t))))))

(defun star-doom--disable-line-numbers ()
  (display-line-numbers-mode -1))

(defun star-doom--visual-shell ()
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        initial-scratch-message nil
        use-dialog-box nil
        use-file-dialog nil
        ring-bell-function #'ignore
        visible-bell nil
        frame-resize-pixelwise t
        window-resize-pixelwise t
        x-underline-at-descent-line t
        truncate-partial-width-windows nil
        cursor-in-non-selected-windows nil
        fast-but-imprecise-scrolling t
        redisplay-skip-fontification-on-input t
        frame-title-format '("%b — Emacs"))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (when (fboundp 'fringe-mode)
    (fringe-mode 8))
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode 1)
  (dolist (hook '(org-mode-hook
                  dired-mode-hook
                  special-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  eshell-mode-hook
                  shell-mode-hook
                  pdf-view-mode-hook))
    (add-hook hook #'star-doom--disable-line-numbers)))

(defun star-doom--theme ()
  (when (require 'doom-themes nil t)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme star-doom-theme t)
    (when (fboundp 'doom-themes-visual-bell-config)
      (doom-themes-visual-bell-config))
    (when (fboundp 'doom-themes-org-config)
      (doom-themes-org-config)))
  (when (require 'solaire-mode nil t)
    (when (fboundp 'solaire-global-mode)
      (solaire-global-mode 1)))
  (when (require 'vi-tilde-fringe nil t)
    (cond
     ((fboundp 'global-vi-tilde-fringe-mode)
      (global-vi-tilde-fringe-mode 1))
     ((fboundp 'vi-tilde-fringe-mode)
      (add-hook 'prog-mode-hook #'vi-tilde-fringe-mode)
      (add-hook 'text-mode-hook #'vi-tilde-fringe-mode)))))

(defun star-doom--modeline ()
  (when (require 'doom-modeline nil t)
    (setq doom-modeline-height 26
          doom-modeline-bar-width 3
          doom-modeline-buffer-file-name-style 'truncate-upto-project
          doom-modeline-buffer-state-icon t
          doom-modeline-buffer-modification-icon t
          doom-modeline-icon t
          doom-modeline-major-mode-icon t
          doom-modeline-major-mode-color-icon t
          doom-modeline-minor-modes nil
          doom-modeline-project-detection 'projectile
          doom-modeline-workspace-name t
          doom-modeline-persp-name t)
    (doom-modeline-mode 1)))

(defun star-doom--completion ()
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (when (featurep 'ivy)
    (setq ivy-use-virtual-buffers t
          ivy-use-selectable-prompt t
          ivy-wrap t
          ivy-height 15
          ivy-count-format "(%d/%d) "
          ivy-initial-inputs-alist nil
          ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
    (ivy-mode 1)
    (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-alt-done))
  (when (require 'ivy-rich nil t)
    (ivy-rich-mode 1))
  (when (require 'amx nil t)
    (amx-mode 1))
  (when (require 'counsel-projectile nil t)
    (counsel-projectile-mode 1)))

(defun star-doom--editing ()
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 80)
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (recentf-mode 1)
  (winner-mode 1)
  (global-auto-revert-mode 1)
  (global-visual-line-mode 1)
  (when (fboundp 'global-so-long-mode)
    (global-so-long-mode 1))
  (when (require 'dtrt-indent nil t)
    (when (fboundp 'dtrt-indent-global-mode)
      (dtrt-indent-global-mode 1)))
  (when (require 'undo-fu-session nil t)
    (undo-fu-session-global-mode 1))
  (when (require 'yasnippet nil t)
    (yas-global-mode 1))
  (when (require 'smartparens-config nil t)
    (smartparens-global-mode 1))
  (when (require 'apheleia nil t)
    (apheleia-global-mode 1))
  (when (require 'ws-butler nil t)
    (ws-butler-global-mode 1)))

(defun star-doom--evil ()
  (setq evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-Y-yank-to-eol t
        evil-kill-on-visual-paste nil
        evil-move-cursor-back nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-symbol-word-search t)
  (evil-mode 1)
  (when (require 'evil-surround nil t)
    (global-evil-surround-mode 1))
  (when (require 'evil-snipe nil t)
    (evil-snipe-mode 1)
    (evil-snipe-override-mode 1))
  (when (require 'evil-exchange nil t)
    (evil-exchange-install))
  (when (require 'evil-goggles nil t)
    (setq evil-goggles-duration 0.12
          evil-goggles-pulse t)
    (evil-goggles-mode 1))
  (when (require 'evil-visualstar nil t)
    (global-evil-visualstar-mode 1))
  (when (require 'evil-org nil t)
    (add-hook 'org-mode-hook #'evil-org-mode)
    (with-eval-after-load 'evil-org
      (require 'evil-org-agenda nil t)
      (when (fboundp 'evil-org-agenda-set-keys)
        (evil-org-agenda-set-keys)))))

(defun star-doom--company ()
  (when (featurep 'company)
    (setq company-idle-delay 0.18
          company-minimum-prefix-length 2
          company-tooltip-limit 12
          company-tooltip-align-annotations t
          company-selection-wrap-around t
          company-require-match nil)
    (global-company-mode 1)
    (define-key company-active-map (kbd "TAB") #'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)))

(defun star-doom--projects ()
  (when (featurep 'projectile)
    (setq projectile-completion-system 'ivy
          projectile-enable-caching t
          projectile-switch-project-action #'projectile-dired)
    (projectile-mode 1)))

(defun star-doom--workspaces ()
  (when (require 'persp-mode nil t)
    (setq persp-autokill-buffer-on-remove 'kill-weak
          persp-nil-name "main"
          persp-kill-foreign-buffer-behaviour 'kill)
    (persp-mode 1)))

(defun star-doom--vc ()
  (when (require 'diff-hl nil t)
    (global-diff-hl-mode 1)
    (when (fboundp 'diff-hl-flydiff-mode)
      (diff-hl-flydiff-mode 1)))
  (when (require 'hl-todo nil t)
    (global-hl-todo-mode 1)))

(defun star-doom--popups ()
  (star-doom--popup-rule "\\*Help\\*" 0.35)
  (star-doom--popup-rule "\\*Warnings\\*" 0.30)
  (star-doom--popup-rule "\\*Compile-Log\\*" 0.30)
  (star-doom--popup-rule "\\*compilation\\*" 0.35)
  (star-doom--popup-rule "\\*Backtrace\\*" 0.40)
  (star-doom--popup-rule "\\*Messages\\*" 0.30)
  (star-doom--popup-rule "\\*vterm" 0.32)
  (star-doom--popup-rule "\\*eshell" 0.32))

(defun star-doom--prefixes ()
  (dolist (entry '(("b" . "buffer")
                   ("c" . "code")
                   ("f" . "file")
                   ("g" . "git")
                   ("h" . "help")
                   ("n" . "notes")
                   ("o" . "open")
                   ("p" . "project")
                   ("q" . "quit/session")
                   ("s" . "search")
                   ("w" . "window")
                   ("TAB" . "workspace")))
    (general-define-key
     :keymaps 'star-leader-map
     (car entry) (list :ignore t :which-key (cdr entry)))))

(defun star-doom--leader ()
  (star-doom--prefixes)
  (general-define-key
   :keymaps 'star-leader-map
   "." #'star-doom/find-file
   "," #'star-doom/switch-buffer
   "<" #'star-doom/switch-buffer-other-window
   ":" #'counsel-M-x
   "SPC" #'counsel-M-x
   "b b" #'star-doom/switch-buffer
   "b B" #'ibuffer
   "b k" #'kill-current-buffer
   "b n" #'next-buffer
   "b p" #'previous-buffer
   "b s" #'save-buffer
   "f f" #'star-doom/find-file
   "f r" #'star-doom/recent-file
   "f s" #'save-buffer
   "f S" #'write-file
   "p p" #'star-doom/project-switch
   "p f" #'star-doom/project-find-file
   "p /" #'star-doom/project-search
   "s s" #'star-doom/search-buffer
   "s p" #'star-doom/project-search
   "s i" #'star-doom/imenu
   "g g" #'magit-status
   "g b" #'magit-branch-checkout
   "g l" #'magit-log-current
   "o t" #'star-doom/vterm-popup
   "o e" #'star-doom/eshell-popup
   "o a" #'star-doom/org-agenda
   "o c" #'star-doom/org-capture
   "n r" #'star-doom/org-roam-find
   "w h" #'windmove-left
   "w j" #'windmove-down
   "w k" #'windmove-up
   "w l" #'windmove-right
   "w v" #'split-window-right
   "w s" #'split-window-below
   "w d" #'delete-window
   "w o" #'delete-other-windows
   "w =" #'balance-windows
   "TAB TAB" #'persp-switch
   "TAB n" #'persp-add-new
   "TAB d" #'persp-kill
   "TAB r" #'persp-rename
   "TAB ]" #'persp-next
   "TAB [" #'persp-prev
   "h f" #'helpful-callable
   "h v" #'helpful-variable
   "h k" #'describe-key
   "h m" #'describe-mode
   "f p" #'star-doom/open-config
   "f i" #'star-doom/open-init
   "f u" #'star-doom/open-ux
   "q r" #'star-config-sync
   "q q" #'save-buffers-kill-terminal))

(defun star-doom--which-key ()
  (when (featurep 'which-key)
    (setq which-key-idle-delay 0.35
          which-key-idle-secondary-delay 0.05
          which-key-max-description-length 32
          which-key-side-window-location 'bottom
          which-key-side-window-max-height 0.25)
    (which-key-mode 1)))

(defun star-doom-ux-init ()
  "Apply Doom-equivalent behavior without loading the Doom runtime."
  (interactive)
  (star-doom--visual-shell)
  (star-doom--theme)
  (star-doom--modeline)
  (star-doom--completion)
  (star-doom--editing)
  (star-doom--evil)
  (star-doom--company)
  (star-doom--projects)
  (star-doom--workspaces)
  (star-doom--vc)
  (star-doom--popups)
  (star-doom--leader)
  (star-doom--which-key)
  (message "Native Doom UX layer active"))

(provide 'doom-ux)
;;; doom-ux.el ends here
