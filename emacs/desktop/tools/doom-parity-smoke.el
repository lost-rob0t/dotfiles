;;; doom-parity-smoke.el --- Native Doom parity gate -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defun star-parity-fail (format-string &rest args)
  (error "DOOM PARITY: %s" (apply #'format format-string args)))

(defun star-parity-assert (value format-string &rest args)
  (unless value
    (apply #'star-parity-fail format-string args)))

(defun star-parity-bound-and-true-p (symbol)
  (and (boundp symbol) (symbol-value symbol)))

(defun star-parity-binding (keys command)
  (let ((actual (lookup-key star-leader-map (kbd keys))))
    (star-parity-assert
     (eq actual command)
     "%s expected %S, got %S"
     keys command actual)))

(defun star-parity-prefix (keys)
  (star-parity-assert
   (star-native--prefix-p (lookup-key star-leader-map (kbd keys)))
   "%s is not a prefix"
   keys))

(defun star-parity-library (library)
  (star-parity-assert
   (locate-library library)
   "required library missing: %s"
   library))

(defun star-parity-hook (hook function)
  (star-parity-assert
   (memq function (symbol-value hook))
   "%S does not contain %S"
   hook function))

(defun star-parity-remap (command expected)
  (let ((actual (lookup-key global-map (vector 'remap command))))
    (star-parity-assert
     (eq actual expected)
     "remap %S expected %S, got %S"
     command expected actual)))

(defun star-parity-run ()
  "Fail batch Emacs when native desktop no longer behaves like its Doom baseline."
  (require 'org)
  (require 'org-agenda)
  (require 'doom-ux)
  (require 'native-bootstrap)
  (require 'native-tools)
  (require 'native-session)
  (star-doom-ux-init)
  (star-native-doom-core-compat-init)
  (star-native-tools-init)
  (star-native-session-init)

  ;; Runtime boundary: native must reproduce Doom behavior, not secretly load Doom.
  (star-parity-assert (not (featurep 'doom)) "Doom runtime leaked into native desktop")
  (star-parity-assert (keymapp star-leader-map) "native leader map is unavailable")

  ;; The old private config deliberately disabled doom-dashboard and opened Agenda.
  (star-parity-assert
   (functionp initial-buffer-choice)
   "initial-buffer-choice must remain the Org Agenda launcher")

  ;; Visual shell.
  (star-parity-assert
   (member 'doom-outrun-electric custom-enabled-themes)
   "Doom Outrun Electric is not active: %S"
   custom-enabled-themes)
  (star-parity-assert
   (star-parity-bound-and-true-p 'doom-modeline-mode)
   "doom-modeline-mode is not active")
  (star-parity-assert (not menu-bar-mode) "menu bar must remain hidden")
  (when (boundp 'tool-bar-mode)
    (star-parity-assert (not tool-bar-mode) "tool bar must remain hidden"))

  ;; Editing/completion baseline.
  (dolist (mode '(evil-mode ivy-mode counsel-mode global-company-mode
                  projectile-mode which-key-mode persp-mode
                  global-diff-hl-mode global-hl-todo-mode global-anzu-mode
                  undo-fu-session-global-mode better-jumper-mode))
    (star-parity-assert
     (star-parity-bound-and-true-p mode)
     "%S is not active"
     mode))

  ;; Package availability is necessary but not sufficient. These libraries implement
  ;; behavior from the enabled Doom modules and must stay in the native closure.
  (dolist (library '("adaptive-wrap" "amx" "anzu" "apheleia" "better-jumper"
                     "counsel-projectile" "dape" "diff-hl" "dirvish" "doom-modeline"
                     "doom-snippets" "doom-themes" "dtrt-indent" "dumb-jump"
                     "emacs-everywhere" "emojify" "eros" "evil-anzu" "evil-args"
                     "evil-collection" "evil-easymotion" "evil-embrace" "evil-escape"
                     "evil-exchange" "evil-goggles" "evil-indent-plus" "evil-lion"
                     "evil-nerd-commenter" "evil-numbers" "evil-org" "evil-quick-diff"
                     "evil-smartparens" "evil-snipe" "evil-surround"
                     "evil-textobj-anyblock" "evil-traces" "evil-vimish-fold"
                     "evil-visualstar" "forge" "git-link" "git-timemachine" "gptel"
                     "helpful" "hl-todo" "ivy-rich" "ivy-xref" "kkp" "link-hint"
                     "lispy" "lispyville" "lsp-mode" "magit" "makefile-executor"
                     "mcp" "org-modern" "org-ql" "org-roam" "parinfer-rust-mode"
                     "persp-mode" "projectile" "rainbow-mode" "sly" "smartparens"
                     "solaire-mode" "ssh-deploy" "swiper" "unicode-fonts"
                     "vi-tilde-fringe" "vimish-fold" "vterm" "vundo" "which-key"
                     "yasnippet" "yasnippet-snippets"))
    (star-parity-library library))

  ;; Doom core replaced Evil and xref jump history with better-jumper.
  (star-parity-remap 'evil-jump-forward 'better-jumper-jump-forward)
  (star-parity-remap 'evil-jump-backward 'better-jumper-jump-backward)
  (star-parity-remap 'xref-go-back 'better-jumper-jump-backward)
  (star-parity-remap 'xref-go-forward 'better-jumper-jump-forward)
  (star-parity-hook 'kill-buffer-hook 'star-native-doom-set-jump-h)

  ;; :tools lookup restores Doom's xref front door, not just dumb-jump installation.
  (star-parity-assert (fboundp '+lookup/definition) "+lookup/definition is missing")
  (star-parity-assert (fboundp '+lookup/references) "+lookup/references is missing")
  (star-parity-assert (fboundp '+lookup/implementations) "+lookup/implementations is missing")
  (star-parity-assert (fboundp '+lookup/file) "+lookup/file is missing")
  (star-parity-assert (fboundp '+lookup/documentation) "+lookup/documentation is missing")
  (star-parity-remap 'xref-find-definitions '+lookup/definition)
  (star-parity-remap 'xref-find-references '+lookup/references)
  (star-parity-assert (eq dumb-jump-prefer-searcher 'rg)
                      "dumb-jump is no longer preferring ripgrep")
  (star-parity-assert (eq dumb-jump-selector 'ivy)
                      "dumb-jump is no longer using Ivy")

  ;; :tools eval +overlay and :editor word-wrap were behavior, not package names.
  (star-parity-hook 'emacs-lisp-mode-hook 'eros-mode)
  (star-parity-hook 'prog-mode-hook 'star-native-word-wrap-mode)
  (star-parity-hook 'text-mode-hook 'star-native-word-wrap-mode)

  ;; Enabled tool/app modules retain their runtime glue, not only their packages.
  (star-parity-assert (fboundp '+make/run) "+make/run compatibility command is missing")
  (star-parity-assert (fboundp '+make/run-last) "+make/run-last compatibility command is missing")
  (star-parity-hook 'dockerfile-mode-hook 'star-native-dockerfile-setup)
  (star-parity-hook 'css-mode-hook 'star-native-web-colors-setup)
  (star-parity-hook 'sass-mode-hook 'star-native-web-colors-setup)
  (star-parity-hook 'stylus-mode-hook 'star-native-web-colors-setup)
  (star-parity-hook 'after-save-hook '+upload-init-after-save-h)
  (star-parity-hook 'find-file-hook '+upload-init-find-file-h)
  (star-parity-hook 'xref-backend-functions 'dumb-jump-xref-activate)
  (star-parity-hook 'emacs-everywhere-mode-hook 'star-native-everywhere-buffer-setup)
  (star-parity-assert (= ssh-deploy-on-explicit-save 1)
                      "ssh-deploy explicit-save behavior changed")
  (star-parity-assert (not ssh-deploy-automatically-detect-remote-changes)
                      "ssh-deploy remote-change default changed")
  (star-parity-assert
   (eq (get 'ssh-deploy-root-remote 'safe-local-variable) 'stringp)
   "ssh-deploy remote root is not declared safe like Doom's module")

  ;; The leader hierarchy itself is part of Doom's UX. A command may not collapse a
  ;; subtree such as SPC o a or SPC n r.
  (dolist (prefix '("b" "c" "d" "f" "g" "g f" "g l" "g o" "h" "i"
                    "n" "n r" "n r d" "o" "o a" "p" "q" "r" "s" "t"
                    "w" "y" "TAB"))
    (star-parity-prefix prefix))

  ;; Representative Doom defaults and native equivalents.
  (dolist (binding '(("." . star-doom/find-file)
                     ("," . star-doom/switch-buffer)
                     ("b b" . star-doom/switch-buffer)
                     ("b i" . ibuffer)
                     ("c a" . lsp-execute-code-action)
                     ("c c" . compile)
                     ("c d" . xref-find-definitions)
                     ("c f" . star-native/format-buffer)
                     ("d d" . dape)
                     ("d b" . dape-breakpoint-toggle)
                     ("f f" . star-doom/find-file)
                     ("f y" . star-doom/yank-file-path)
                     ("g g" . magit-status)
                     ("g ." . magit-file-dispatch)
                     ("g f f" . magit-find-file)
                     ("g l i" . forge-list-issues)
                     ("g o p" . forge-browse-pullreq)
                     ("i s" . yas-insert-snippet)
                     ("n a" . org-agenda)
                     ("n r f" . org-roam-node-find)
                     ("n r i" . org-roam-node-insert)
                     ("o A" . org-agenda)
                     ("o a a" . org-agenda)
                     ("o t" . star-doom/vterm-popup)
                     ("o e" . star-doom/eshell-popup)
                     ("p p" . star-doom/project-switch)
                     ("p f" . star-doom/project-find-file)
                     ("p d" . projectile-remove-known-project)
                     ("q c" . star-config-sync)
                     ("q r" . star-native/restart)
                     ("q s" . star-native/quicksave-session)
                     ("q l" . star-native/quickload-session)
                     ("r u" . ssh-deploy-upload-handler)
                     ("s b" . swiper)
                     ("s u" . vundo)
                     ("t w" . star-native-word-wrap-mode)
                     ("t v" . visible-mode)
                     ("TAB ." . persp-switch)
                     ("w u" . winner-undo)))
    (star-parity-binding (car binding) (cdr binding)))

  ;; Never let compatibility glue erase bindings from the user's literate config.
  (dolist (binding '(("t T" . ivan/cycle-theme)
                     ("o a u" . org-agenda-update-files)
                     ("o a w" . org-agenda-week-view)
                     ("o a m" . org-agenda-month-view)
                     ("o a y" . org-agenda-year-view)
                     ("y y" . gptel)
                     ("o D" . +dslide/start)))
    (star-parity-binding (car binding) (cdr binding)))
  (require 'magit)
  (dolist (binding '(("g R" . ar/git-clone-clipboard-url)
                     ("g p P" . magit-push-current-to-pushremote)
                     ("g p p" . magit-pull-from-pushremote)
                     ("g c t" . magit-tag-create)))
    (star-parity-binding (car binding) (cdr binding)))

  ;; Mechanical port regressions that previously produced a technically-starting but
  ;; broken editor.
  (star-parity-assert (fboundp 'track-org-file) "track-org-file alias is missing")
  (star-parity-assert
   (eq (symbol-function 'track-org-file)
       (symbol-function 'nsa/track-org-file))
   "track-org-file compatibility alias is broken")
  (star-parity-assert
   (equal (cdr (assoc "\\.fs\\'" auto-mode-alist)) 'forth-mode)
   "Forth auto-mode entry is missing or malformed")
  (star-parity-assert
   (equal (expand-file-name "eshell/aliases" star-config-directory)
          eshell-aliases-file)
   "native Eshell is not using the migrated aliases file")
  (star-parity-assert
   (file-readable-p eshell-aliases-file)
   "native Eshell aliases file is unreadable: %s"
   eshell-aliases-file)

  ;; Lisp editing was enabled through Doom's lispy/parinfer/smartparens modules.
  (star-parity-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (star-parity-hook 'emacs-lisp-mode-hook 'lispyville-mode)
  (star-parity-hook 'emacs-lisp-mode-hook 'parinfer-rust-mode)
  (star-parity-hook 'common-lisp-mode-hook 'lispy-mode)
  (star-parity-hook 'common-lisp-mode-hook 'lispyville-mode)
  (star-parity-hook 'common-lisp-mode-hook 'parinfer-rust-mode)
  (star-parity-hook 'common-lisp-mode-hook 'evil-smartparens-mode)

  ;; Doom snippets must be activated, not merely installed.
  (require 'yasnippet)
  (require 'doom-snippets)
  (star-parity-assert
   (memq 'doom-snippets-dir yas-snippet-dirs)
   "Doom snippets are installed but not registered with yasnippet")

  ;; Popup policy should catch the major temporary-buffer families we use daily.
  (dolist (regexp '("\\*Help\\*" "\\*compilation\\*" "\\*vterm" "\\*eshell"))
    (star-parity-assert
     (assoc regexp display-buffer-alist)
     "popup rule missing: %s"
     regexp))

  (message "native-doom-parity-smoke-ok")
  t)

(star-parity-run)

;;; doom-parity-smoke.el ends here
