(setq doom-theme 'doom-outrun-electric)

(setq display-line-numbers-type t)

(setq frame-resize-pixelwise t)

(map! :leader
      :desc "Push Current branch to remote branch"
      "g p P" #'magit-push-current-to-pushremote)

(map! :leader
      :desc "Pull current branch from remote"
      "g p p" #'magit-pull-from-pushremote)

(require 'magit-todos)

(setq projectile-project-search-path
      '(("~/Documents/Projects" . 1)))

(setq org-directory "~/Documents/Notes/org")

;;(add-hook 'before-save-hook 'time-stamp)

(defun org-ask-location ()
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location nil nil t t))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")))
    (end-of-line))

;; (setq org-capture-templates
;;   '(
;;      ("n" "note"
;;        entry (file+olp+datetree "/Users/me/org/logbook.org" "JOURNAL")
;;        ...)
;;      ("t" "tasks"
;;        entry (file+olp+datetree "/Users/me/org/todo.org" "TASKS")
;;        ...)))

(setq org-agenda-files (directory-files-recursively "~/Documents/Notes/" "\\.org$"))

(defun org-agenda-update-files ()
  "Update the org-agenda-files"
  (interactive)
  (setq org-agenda-files (directory-files-recursively "~/Documents/Notes/" "\\.org$")))
(map! :leader
      :desc "update agenda"
      "o a u" #'org-agenda-update-files)

(map! :leader
      :desc "Switch to week view"
      "o a w" #'org-agenda-week-view)

(map! :leader
      :desc "switch to month view"
      "o a m" #'org-agenda-month-view)

(map! :leader
      :desc "switch to month view"
      "o a y" #'org-agenda-year-view)

(setq org-super-agenda-groups
      '(
        (:and (:todo "IDEA" :name "Starintel Idea" :tag ("starintel" "sit")))
        (:and (:todo "TODO" :name "Starintel Bugs" :tag ("starintel-bug" "sib")))
        (:and (:todo "TODO" :name "Personal" :tag ("mow" "trash")))
        (:and (:todo "TODO" :name "Read inbox" :tag ("book" "artical" "books")))))

(map! :leader
      :desc "Tangle a file"
      "b t" #'org-babel-tangle)

(map! :leader
      :desc "Babel execute selected source block"
      "c b" #'org-babel-execute-src-block)

(map! :leader
      :desc "Babel execute buffer"
      "c B" #'org-babel-execute-buffer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (org . t) (nim . t) (python . t) (erlang . t) (ein . t) (lisp . t)))

(defun edit-src-block (src fn language)
  "Replace SRC org-element's value property with the result of FN.
FN is a function that operates on org-element's value and returns a string.
LANGUAGE is a string referring to one of orb-babel's supported languages.
(https://orgmode.org/manual/Languages.html#Languages)"
  (let ((src-language (org-element-property :language src))
        (value (org-element-property :value src)))
    (when (string= src-language language)
      (let ((copy (org-element-copy src)))
        (org-element-put-property copy :value
                                  (funcall fn value))
        (org-element-set-element src copy)))))

(defun format-elisp-string (string)
  "Indents elisp buffer string and reformats dangling parens."
  (with-temp-buffer
    (let ((inhibit-message t))
      (emacs-lisp-mode)
      (insert
       (replace-regexp-in-string "[[:space:]]*
[[:space:]]*)" ")" string))
      (indent-region (point-min) (point-max))
      (buffer-substring (point-min) (point-max)))))

(defun format-elisp-src-blocks ()
  "Format Elisp src blocks in the current org buffer"
  (interactive)
  (save-mark-and-excursion
    (let ((AST (org-element-parse-buffer)))
      (org-element-map AST 'src-block
        (lambda (element)
          (edit-src-block element #'format-elisp-string "emacs-lisp")))
      (delete-region (point-min) (point-max))
      (insert (org-element-interpret-data AST)))))

(map! :leader
      :after org
      :prefix ("b" . "org-babel-fomats")
      :desc "format src" "f" #'format-elisp-src-blocks)

(defun org-babel-edit-prep:python (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(defun org-babel-edit-prep:nim (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(defun org-babel-edit-prep:sh (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("php" . "src php"))
  (add-to-list 'org-structure-template-alist '("jn" . "src json"))
  (add-to-list 'org-structure-template-alist '("xm" . "src xml"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("nim" . "src nim"))
  (add-to-list 'org-structure-template-alist '("erl" . "src erlang"))
  (add-to-list 'org-structure-template-alist '("ss" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist '("nix" . "src nix")))

(defvar org-configs-list ()
  "A List of org documents that holds your configuration. Will be used to tangle to elisp")
(setq org-configs-list '("~/.doom.d/config.org" "~/.doom.d/packages.org"))
(defun tangle-orgs (config-list)
  "Tangle a list of org documents."
  (mapcar 'org-babel-tangle-file config-list))

(defun doom-config-sync ()
  "Tangle your dotfiles and run doom sync"
  (interactive)
  (tangle-orgs org-configs-list)
  (doom/reload))

;(map! :leader
;      :after help-mode
;      :map help-mode-map
;      :prefix ("r" . "+reload")
;      :desc "Tangle Configs and reload" "s" #'doom-config-sync)

;;(require 'org-wiki)

(require 'org-download)

;; Drag-and-drop to `dired`
;;(add-hook 'dired-mode-hook 'org-download-enable)

(map! :localleader
      :after org
      :map org-mode-map
      :prefix ("a" . "attachments")
      :desc "paste image" "p" #'org-download-clipboard
      :desc "insert image from url" "i" #'org-download-yank)

(setq org-roam-directory "~/Documents/Notes/org/roam")

;;  Tahnk you, this comment fixed my old config!
;;  https://www.reddit.com/r/DoomEmacs/comments/sk8558/comment/hxxp7l0/?utm_source=share&utm_medium=web2x&context=3

(after! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory "~/Documents/Notes/org/roam")
  (setq org-roam-complete-everywhere t)
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n<t") :unnarrowed t)
          ("w" "wiki" plain "*%? %^g"
           :target (file+head "wiki/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: %n"))
          ("h" "hackthebox" plain "%?"
           :target (file+head "hackthebox/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")))))

(defun url2org (begin end)
  "Download a webpage from selected url and convert to org."
  (interactive "r")
  (shell-command-on-region begin end
                           (concat "pandoc --from=html --to=org " (buffer-substring begin end))
                           nil t))

;;(setq org-roam-db-update-on-save t)

(require 'epa-file)
(epa-file-enable)

(setq epa-file-encrypt-to '("nsaspy@airmail.cc"))

(setq epa-file-select-keys "235327FBDEFB3719")

(map! :localleader
      :after org
      :map org-mode-map
      :prefix ("c" . "clock")
      :desc "Start Pomodoro" "T" #'org-pomodoro)

(eval-after-load 'org-present
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(map! :leader
      :desc "Add a neew template to yasnippet"
      "a y s"                   #'+snippets/new)

(map! :leader
      :desc "Edit template"
      "a y e" #'+snippets/find)

(setq deft-extenstions '("txt", "org", "md"))
(setq deft-directory "~/Documents/Notes")

(setq deft-recursive t)

(setq deft-use-filename-as-title t)

(require 'notifications)

(require 'elfeed-org)

(elfeed-org)

(setq rmh-elfeed-org-files '("~/Documents/Notes/org/rss.org"))

(require 'webpaste)

(setq webpaste-paste-confirmation t)

(setq webpaste-provider-priority '("ix.io" "dpaste.org"
                                   "dpaste.com" "clbin.com"
                                   "0x0.st" "bpa.st"
                                   "paste.rs"))

(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix ("p" . "webpaste")
        :desc "paste region to a paste service" "r" #'webpaste-paste-region
        :desc "paste entire buffer to paste service" "b" #'webpaste-paste-buffer)))

(require 'pcap-mode)

(with-eval-after-load 'org
  (require 'inherit-org)

  (with-eval-after-load 'info
    (add-hook 'Info-mode-hook 'inherit-org-mode))

  (with-eval-after-load 'helpful
    (add-hook 'helpful-mode-hook 'inherit-org-mode))

  (with-eval-after-load 'w3m
    (add-hook 'w3m-fontify-before-hook 'inherit-org-w3m-headline-fontify) ;only one level is supported
    (add-hook 'w3m-fontify-after-hook 'inherit-org-mode)))

(eval-after-load "w3m-form"
  '(progn
     (define-minor-mode dme:w3m-textarea-mode
       "Minor mode used when editing w3m textareas."
       nil " dme:w3m-textarea" w3m-form-input-textarea-keymap)
     (defun dme:w3m-textarea-hook ()
                                        ; protect the form local variables from being killed by `text-mode'
       (mapcar (lambda (v)
		 (if (string-match "^w3m-form-input-textarea.*"
				   (symbol-name (car v)))
		     (put (car v) 'permanent-local t)))
	       (buffer-local-variables))
       (text-mode)
       (dme:w3m-textarea-mode))
     (add-hook! 'w3m-form-input-textarea-mode-hook 'dme:w3m-textarea-hook)))

(defun vterm--rename-buffer-as-title (title)
  (let ((dir (string-trim-left (concat (nth 1 (split-string title ":")) "/"))))
    (cd-absolute dir)
    (rename-buffer (format "term %s" title))))
(add-hook 'vterm-set-title-functions 'vterm--rename-buffer-as-title)

(require 'dirvish)
(dirvish-override-dired-mode)

(setq python-ident-offset 4)

(after! lsp-python-ms
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  (set-lsp-priority! 'mspyls 1))

(envrc-global-mode)

(map! :leader
      :after nix
      :map nix-mode-map
      :prefix ("s" . "search")
      :desc "search option" "n" #'helm-nixos-options)

(require 'nix-update)
(map! :localleader
      :after nix
      :map nix-mode-map
      :prefix ("u" . "update")
      :desc "Update fetchgit" "g" #'nix-update-fetch)

(add-to-list 'company-backends 'company-nixos-options)

(setq flycheck-command-wrapper-function
        (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
      flycheck-executable-find
        (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

(require 'flycheck-nim)

(require 'lsp-mode)
(add-to-list 'lsp-language-id-configuration '(nim-mode . "nim"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "nimlsp")
                  :major-modes '(nim-mode)
                  :server-id 'nimlsp))
(add-hook 'nim-mode-hook #'lsp)

(add-to-list 'auto-mode-alist '("\\.fs" . 'forth-mode))

(use-package! flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(explain-pause-mode nil)

;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "*.i2p:4444")))

(defun cht-sh ()
  "look up a cheat"
  (interactive)
  (async-shell-command (format "cht.sh %s" (read-string "Enter search: "))))
(map! :leader
      :prefix ("s" . "search")
      :desc "cheat sheat" "c" #'cht-sh)

(setq bookmark-file "~/Documents/Emacs/bookmarks")

(global-activity-watch-mode)

(require 'ks)

    (setq mastodon-instance-url "https://pleroma.nobodyhasthe.biz"
          mastodon-active-user "nott")

(use-package! shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package! eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
 (require 'shrface))

(use-package! nov
  :defer t
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))

(use-package! anki
  :defer t
  :load-path "~/.emacs.d/lisp/anki/"
  :init
  (add-hook 'anki-mode-hook #'shrface-mode)
  (autoload 'anki "anki")
  (autoload 'anki-browser "anki")
  (autoload 'anki-list-decks "anki")
  :config
  (require 'shrface)
  (setq anki-shr-rendering-functions (append anki-shr-rendering-functions shr-external-rendering-functions))
 (setq sql-sqlite-program "/usr/bin/sqlite3")
 (setq anki-collection-dir "/Users/chandamon/Library/Application Support/Anki2/User 1"))
