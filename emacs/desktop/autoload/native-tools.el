;;; native-tools.el --- Native Doom tool compatibility -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst star-native-cache-directory
  (file-name-as-directory
   (expand-file-name
    "star-emacs/"
    (or (getenv "XDG_CACHE_HOME")
        (expand-file-name "~/.cache/"))))
  "Cache directory for the native desktop profile.")

(defun star-native-project-p ()
  "Return non-nil when point is inside a known project."
  (or (and (fboundp 'projectile-project-p)
           (ignore-errors (projectile-project-p)))
      (project-current nil)))

(defun +make/run ()
  "Run a Make target like Doom's enabled :tools make module."
  (interactive)
  (require 'makefile-executor)
  (if (star-native-project-p)
      (makefile-executor-execute-project-target)
    (let ((makefile
           (cl-loop with start = (or buffer-file-name default-directory)
                    for file in '("Makefile" "makefile")
                    for root = (locate-dominating-file start file)
                    when root return (expand-file-name file root))))
      (unless makefile
        (user-error "Cannot find a Makefile from %s" default-directory))
      (let ((default-directory (file-name-directory makefile)))
        (makefile-executor-execute-target makefile)))))

(defun +make/run-last ()
  "Repeat the most recent Make target."
  (interactive)
  (require 'makefile-executor)
  (makefile-executor-execute-last nil))

(defun star-native-dockerfile-setup ()
  "Restore Doom's Dockerfile docset/formatter integration."
  (when (boundp 'dash-docs-docsets)
    (setq-local dash-docs-docsets
                (cons "Docker" (delete "Docker" dash-docs-docsets))))
  (when (require 'apheleia nil t)
    (when (executable-find "dockerfmt")
      (setf (alist-get 'dockerfmt apheleia-formatters)
            '("dockerfmt"))
      (setf (alist-get 'dockerfile-mode apheleia-mode-alist)
            'dockerfmt))))

(defun star-native-web-colors-setup ()
  "Show CSS/Sass/Stylus color literals as Doom's web/RGB setup did."
  (when (require 'rainbow-mode nil t)
    (rainbow-mode 1)))

(defun +upload-init-after-save-h ()
  "Upload explicitly saved buffers when ssh-deploy locals request it."
  (when (and (bound-and-true-p ssh-deploy-root-remote)
             (require 'ssh-deploy nil t)
             (integerp ssh-deploy-on-explicit-save)
             (> ssh-deploy-on-explicit-save 0))
    (ssh-deploy-upload-handler ssh-deploy-force-on-explicit-save)
    (when (or ssh-deploy-root-remote ssh-deploy-root-local)
      (ssh-deploy-line-mode 1))))

(defun +upload-init-find-file-h ()
  "Initialize ssh-deploy for buffers that declare a remote root."
  (when (and (bound-and-true-p ssh-deploy-root-remote)
             (require 'ssh-deploy nil t))
    (unless ssh-deploy-root-local
      (setq ssh-deploy-root-local
            (or (ignore-errors (star-project-root))
                default-directory)))
    (when ssh-deploy-automatically-detect-remote-changes
      (ssh-deploy-remote-changes-handler))
    (when (or ssh-deploy-root-remote ssh-deploy-root-local)
      (ssh-deploy-line-mode 1))))

(defun star-native--upload ()
  "Restore Doom's ssh-deploy defaults and safe local-variable declarations."
  (setq ssh-deploy-revision-folder
        (expand-file-name "ssh-revisions/" star-native-cache-directory)
        ssh-deploy-on-explicit-save 1
        ssh-deploy-automatically-detect-remote-changes nil)
  (dolist (entry '((ssh-deploy-root-local . stringp)
                   (ssh-deploy-root-remote . stringp)
                   (ssh-deploy-script . functionp)
                   (ssh-deploy-on-explicit-save . booleanp)
                   (ssh-deploy-force-on-explicit-save . booleanp)
                   (ssh-deploy-async . booleanp)
                   (ssh-deploy-exclude-list . listp)))
    (put (car entry) 'safe-local-variable (cdr entry)))
  (add-hook 'after-save-hook #'+upload-init-after-save-h)
  (add-hook 'find-file-hook #'+upload-init-find-file-h))

(defun star-native-thing-at-point-or-region (&optional kind)
  "Return active region text or KIND at point, stripped of text properties."
  (let ((value
         (cond
          ((use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))
          ((thing-at-point (or kind 'symbol) t)))))
    (and value (string-trim value))))

(defun star-native--lookup-identifier (&optional identifier kind prompt)
  (or identifier
      (star-native-thing-at-point-or-region kind)
      (and prompt (read-string prompt))))

(defun +lookup/definition (&optional identifier)
  "Jump to IDENTIFIER's definition using xref with dumb-jump fallback."
  (interactive)
  (let ((identifier (star-native--lookup-identifier identifier 'symbol)))
    (unless identifier
      (user-error "Nothing under point"))
    (condition-case xref-error
        (xref-find-definitions identifier)
      (error
       (if (and (require 'dumb-jump nil t)
                (fboundp 'dumb-jump-go))
           (call-interactively #'dumb-jump-go)
         (signal (car xref-error) (cdr xref-error)))))))

(defun +lookup/references (&optional identifier)
  "Show references to IDENTIFIER using xref."
  (interactive)
  (let ((identifier (star-native--lookup-identifier identifier 'symbol)))
    (unless identifier
      (user-error "Nothing under point"))
    (xref-find-references identifier)))

(defun +lookup/implementations (&optional identifier)
  "Show implementations for IDENTIFIER, preferring the active LSP client."
  (interactive)
  (let ((identifier (star-native--lookup-identifier identifier 'symbol)))
    (unless identifier
      (user-error "Nothing under point"))
    (cond
     ((and (bound-and-true-p lsp-mode)
           (fboundp 'lsp-find-implementation))
      (call-interactively #'lsp-find-implementation))
     ((fboundp 'xref-find-definitions)
      (xref-find-definitions identifier))
     (t
      (user-error "No implementation lookup backend is active")))))

(defun +lookup/type-definition (&optional identifier)
  "Show the type definition for IDENTIFIER when the active LSP client supports it."
  (interactive)
  (let ((identifier (star-native--lookup-identifier identifier 'symbol)))
    (unless identifier
      (user-error "Nothing under point"))
    (if (and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-find-type-definition))
        (call-interactively #'lsp-find-type-definition)
      (+lookup/definition identifier))))

(defun +lookup/file (&optional path)
  "Open PATH or the filename/URL at point."
  (interactive)
  (require 'ffap)
  (let ((path (or path (ffap-guesser) (thing-at-point 'filename t))))
    (unless path
      (user-error "Couldn't find any files here"))
    (find-file-at-point path)))

(defun +lookup/documentation (&optional identifier)
  "Show documentation for IDENTIFIER using the strongest local backend."
  (interactive)
  (let ((identifier
         (star-native--lookup-identifier identifier 'symbol "Describe symbol: ")))
    (cond
     ((and (bound-and-true-p lsp-mode)
           (fboundp 'lsp-describe-thing-at-point))
      (call-interactively #'lsp-describe-thing-at-point))
     ((and identifier
           (intern-soft identifier)
           (fboundp 'helpful-symbol))
      (helpful-symbol (intern identifier)))
     ((and identifier (intern-soft identifier))
      (describe-symbol (intern identifier)))
     ((fboundp 'eldoc-doc-buffer)
      (eldoc-doc-buffer))
     (t
      (user-error "No documentation backend is active")))))

(defun star-native--lookup ()
  "Install native lookup fallbacks used by Doom's :tools lookup module."
  (when (require 'dumb-jump nil t)
    (setq dumb-jump-prefer-searcher 'rg
          dumb-jump-aggressive nil
          dumb-jump-selector 'ivy)
    (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90))
  (when (require 'ivy-xref nil t)
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs
          xref-show-definitions-function #'ivy-xref-show-defs))
  (global-set-key [remap xref-find-definitions] #'+lookup/definition)
  (global-set-key [remap xref-find-references] #'+lookup/references)
  (when (fboundp 'general-define-key)
    (general-define-key
     :states '(normal visual)
     :keymaps 'override
     "K" #'+lookup/documentation
     "gd" #'+lookup/definition
     "gD" #'+lookup/references
     "gf" #'+lookup/file
     "gI" #'+lookup/implementations)))

(defun +everywhere-app-info-hyprland ()
  "Return active-window data for Emacs Everywhere on Hyprland."
  (require 'emacs-everywhere)
  (require 'json)
  (let-alist
      (json-read-from-string
       (emacs-everywhere--call "hyprctl" "-j" "activewindow"))
    (make-emacs-everywhere-app
     :id .address
     :class .class
     :title .title
     :geometry (list (aref .at 0)
                     (aref .at 1)
                     (aref .size 0)
                     (aref .size 1)))))

(defun star-native--find-niri-socket ()
  (car
   (sort
    (file-expand-wildcards
     (format "/run/user/%d/niri.*sock" (user-uid)))
    #'string-lessp)))

(defun +everywhere-app-info-niri ()
  "Return active-window data for Emacs Everywhere on Niri."
  (require 'emacs-everywhere)
  (require 'json)
  (unless (getenv "NIRI_SOCKET")
    (let ((socket (star-native--find-niri-socket)))
      (unless socket
        (user-error "Could not find an active niri socket"))
      (setenv "NIRI_SOCKET" socket)))
  (let ((json-raw (emacs-everywhere--call
                   "niri" "msg" "-j" "focused-window")))
    (when (string-prefix-p "Error" json-raw)
      (user-error "niri focused-window failed: %s" json-raw))
    (let-alist (json-read-from-string json-raw)
      (make-emacs-everywhere-app
       :id (if (numberp .id) (number-to-string .id) .id)
       :class .app_id
       :title .title))))

(defun star-native-everywhere-buffer-setup ()
  "Restore snippets and compact Doom-modeline behavior in Everywhere buffers."
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1))
  (when (bound-and-true-p doom-modeline-mode)
    (setq-local doom-modeline-buffer-file-name-style 'buffer-name)))

(defun star-native--everywhere ()
  "Configure Emacs Everywhere without loading Doom's module runtime."
  (add-hook 'emacs-everywhere-mode-hook
            #'star-native-everywhere-buffer-setup)
  (with-eval-after-load 'emacs-everywhere
    (add-to-list
     'emacs-everywhere-system-configs
     '((wayland . Hyprland)
       :focus-command ("hyprctl" "dispatch" "focuswindow" "address:%w")
       :info-function +everywhere-app-info-hyprland))
    (add-to-list
     'emacs-everywhere-system-configs
     '((wayland . niri)
       :focus-command ("niri" "msg" "action" "focus-window" "--id" "%w")
       :info-function +everywhere-app-info-niri))))

(defun star-native-tools-init ()
  "Install runtime glue for enabled Doom tool/app modules."
  (interactive)
  (add-hook 'dockerfile-mode-hook #'star-native-dockerfile-setup)
  (dolist (hook '(css-mode-hook sass-mode-hook stylus-mode-hook))
    (add-hook hook #'star-native-web-colors-setup))
  (star-native--upload)
  (star-native--lookup)
  (star-native--everywhere))

(add-hook 'after-init-hook #'star-native-tools-init t)

(provide 'native-tools)
;;; native-tools.el ends here
