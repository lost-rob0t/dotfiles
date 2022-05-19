;; [[file:config.org::+begin_src emacs-lisp][No heading:1]]
;; -*- lexical-binding: t -*-
;; No heading:1 ends here

;; [[file:config.org::*Theme][Theme:1]]
(setq doom-theme 'doom-outrun-electric)
;; Theme:1 ends here

;; [[file:config.org::*Line numbers][Line numbers:1]]
(setq display-line-numbers-type t)
;; Line numbers:1 ends here

;; [[file:config.org::*Magit][Magit:1]]
(map! :leader
      :desc "Push Current branch to remote branch"
      "g p P" #'magit-push-current-to-pushremote)
;; Magit:1 ends here

;; [[file:config.org::*Magit][Magit:2]]
(map! :leader
      :desc "Pull current branch from remote"
      "g p p" #'magit-pull-from-pushremote)
;; Magit:2 ends here

;; [[file:config.org::*Magit Todos][Magit Todos:1]]
(require 'magit-todos)
;; Magit Todos:1 ends here

;; [[file:config.org::*Projectile][Projectile:1]]
(setq projectile-project-search-path
      '(("~/Documents/Projects" . 1)))
;; Projectile:1 ends here

;; [[file:config.org::*Org Mode][Org Mode:1]]
(setq org-directory "~/Documents/Notes/org")
;; Org Mode:1 ends here

;; [[file:config.org::*Org Mode][Org Mode:2]]
(add-hook 'before-save-hook 'time-stamp)
;; Org Mode:2 ends here

;; [[file:config.org::*Org Agenda][Org Agenda:1]]
(setq org-agenda-files (directory-files-recursively "~/Documents/Notes/" "\\.org$"))
;; Org Agenda:1 ends here

;; [[file:config.org::*Org Agenda][Org Agenda:2]]
(defun org-agenda-update-files ()
  "Update the org-agenda-files"
  (interactive)
  (setq org-agenda-files (directory-files-recursively "~/Documents/Notes/" "\\.org$")))
(map! :leader
      :desc "update agenda"
      "o a u" #'org-agenda-update-files)
;; Org Agenda:2 ends here

;; [[file:config.org::*Org Agenda][Org Agenda:3]]
(map! :leader
      :desc "Switch to week view"
      "o a w" #'org-agenda-week-view)

(map! :leader
      :desc "switch to month view"
      "o a m" #'org-agenda-month-view)

(map! :leader
      :desc "switch to month view"
      "o a y" #'org-agenda-year-view)
;; Org Agenda:3 ends here

;; [[file:config.org::*adapt this][adapt this:1]]
(setq org-super-agenda-groups
      '(
        (:and (:todo "IDEA" :name "Starintel Bugs" :tag ("starintel-bug" "sib")))
        (:and (:todo "TODO" :name "Starintel Bugs" :tag ("starintel-bug" "sib")))
        (:and (:todo "TODO" :name "Personal" :tag ("mow" "trash")))
        (:and (:todo "TODO" :name "Read inbox" :tag ("book" "artical" "books")))))
;; adapt this:1 ends here

;; [[file:config.org::*Babel][Babel:1]]
(map! :leader
      :desc "Tangle a file"
      "b t" #'org-babel-tangle)
;; Babel:1 ends here

;; [[file:config.org::*Babel][Babel:2]]
(map! :leader
      :desc "Babel execute selected source block"
      "c b" #'org-babel-execute-src-block)
;; Babel:2 ends here

;; [[file:config.org::*Babel][Babel:3]]
(map! :leader
      :desc "Babel execute buffer"
      "c B" #'org-babel-execute-buffer)
;; Babel:3 ends here

;; [[file:config.org::*Babel][Babel:4]]
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (org . t) (nim . t) (python . t) (erlang . t) (ein . t)))
;; Babel:4 ends here

;; [[file:config.org::*Better Formating in org-babel][Better Formating in org-babel:1]]
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
;; Better Formating in org-babel:1 ends here

;; [[file:config.org::*Org Tempo templates][Org Tempo templates:1]]
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("nim" . "src nim"))
  (add-to-list 'org-structure-template-alist '("erl" . "src erlang"))
  (add-to-list 'org-structure-template-alist '("ss" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist '("nix" . "src nix")))
;; Org Tempo templates:1 ends here

;; [[file:config.org::*org-download][org-download:1]]
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
;; org-download:1 ends here

;; [[file:config.org::*org-download][org-download:2]]
(map! :localleader
      :after org
      :map org-mode-map
      :prefix ("a" . "attachments")
      :desc "paste image" "p" #'org-download-clipboard
      :desc "insert image from url" "i" #'org-download-yank)
;; org-download:2 ends here

;; [[file:config.org::*Org Roam][Org Roam:1]]
(setq org-roam-directory "~/Documents/Notes/org/roam")
;; Org Roam:1 ends here

;; [[file:config.org::*Org Roam][Org Roam:2]]
(setq  org-roam-capture-templates '(
                                    ("D" "daily entry" entry (function org-roam--capture-get-point)
                                     "* %<%I:%M %p>: %?"
                                     :file-name "%<%Y-%m-%d-%H%M%S>-${slug}"
                                     :head "#+TITLE: ${title} "
                                     :unnarrowed t)
                                    ("u" "url" entry (function org-roam--capture-get-point)
                                     "* %?\n- Comment: "
                                     :file-name "references/%<%Y-%m-%d-%H%M%S>-${slug}"
                                     :head "#+TITLE: ${title}"
                                     :unnarrowed t)

                                    ("t" "do today" item
                                     #'org-roam-capture--get-point
                                     "[ ] %(princ as/agenda-captured-link)"
                                     :file-name "daily/%<%Y-%m-%d>"
                                     :head "#+title: %<%Y-%m-%d (%A)>\n* [/] Do Today\n* [/] Maybe Do Today\n* Journal\n"
                                     :olp ("Do Today")
                                     :immediate-finish t)))
;; Org Roam:2 ends here

;; [[file:config.org::*Org Roam][Org Roam:3]]
(defun url2org (begin end)
  "Download a webpage from selected url and convert to org."
  (interactive "r")
  (shell-command-on-region begin end
                           (concat "pandoc --from=html --to=org " (buffer-substring begin end))
                           nil t))
;; Org Roam:3 ends here

;; [[file:config.org::*Org File Encryption][Org File Encryption:1]]
(require 'epa-file)
(epa-file-enable)
;; Org File Encryption:1 ends here

;; [[file:config.org::*Org File Encryption][Org File Encryption:2]]
(setq epa-file-select-keys "235327FBDEFB3719")
;; Org File Encryption:2 ends here

;; [[file:config.org::*Yasnippet][Yasnippet:1]]
(map! :leader
      :desc "Add a neew template to yasnippet"
      "a y s"                   #'+snippets/new)
;; Yasnippet:1 ends here

;; [[file:config.org::*Yasnippet][Yasnippet:2]]
(map! :leader
      :desc "Edit template"
      "a y e" #'+snippets/find)
;; Yasnippet:2 ends here

;; [[file:config.org::*Deft][Deft:1]]
(setq deft-extenstions '("txt", "org", "md"))
(setq deft-directory "~/Documents/Notes")
;; Deft:1 ends here

;; [[file:config.org::*Deft][Deft:2]]
(setq deft-recursive t)
;; Deft:2 ends here

;; [[file:config.org::*Deft][Deft:3]]
(setq deft-use-filename-as-title t)
;; Deft:3 ends here

;; [[file:config.org::*Notifications][Notifications:1]]
(require 'notifications)
;; Notifications:1 ends here

;; [[file:config.org::*RSS (Elfeed)][RSS (Elfeed):1]]
(require 'elfeed-org)
;; RSS (Elfeed):1 ends here

;; [[file:config.org::*RSS (Elfeed)][RSS (Elfeed):2]]
(elfeed-org)
;; RSS (Elfeed):2 ends here

;; [[file:config.org::*RSS (Elfeed)][RSS (Elfeed):3]]
(setq rmh-elfeed-org-files '("~/Documents/Notes/org/rss.org"))
;; RSS (Elfeed):3 ends here

;; [[file:config.org::*Webpaste][Webpaste:1]]
(require 'webpaste)
;; Webpaste:1 ends here

;; [[file:config.org::*Webpaste][Webpaste:2]]
(setq webpaste-paste-confirmation t)
;; Webpaste:2 ends here

;; [[file:config.org::*Webpaste][Webpaste:3]]
(setq webpaste-provider-priority '("ix.io" "dpaste.org"
                                   "dpaste.com" "clbin.com"
                                   "0x0.st" "bpa.st"
                                   "paste.rs"))
;; Webpaste:3 ends here

;; [[file:config.org::*Webpaste][Webpaste:4]]
(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix ("p" . "webpaste")
        :desc "paste region to a paste service" "r" #'webpaste-paste-region
        :desc "paste entire buffer to paste service" "b" #'webpaste-paste-buffer)))
;; Webpaste:4 ends here

;; [[file:config.org::*Pcap mode][Pcap mode:1]]
(require 'pcap-mode)
;; Pcap mode:1 ends here

;; [[file:config.org::*inherit org][inherit org:1]]
(with-eval-after-load 'org
  (require 'inherit-org)

  (with-eval-after-load 'info
    (add-hook 'Info-mode-hook 'inherit-org-mode))

  (with-eval-after-load 'helpful
    (add-hook 'helpful-mode-hook 'inherit-org-mode))

  (with-eval-after-load 'w3m
    (add-hook 'w3m-fontify-before-hook 'inherit-org-w3m-headline-fontify) ;only one level is supported
    (add-hook 'w3m-fontify-after-hook 'inherit-org-mode)))
;; inherit org:1 ends here

;; [[file:config.org::*W3M][W3M:1]]
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
;; W3M:1 ends here

;; [[file:config.org::*Python][Python:1]]
(setq python-ident-offset 4)
;; Python:1 ends here

;; [[file:config.org::*Python][Python:2]]
(after! lsp-python-ms
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  (set-lsp-priority! 'mspyls 1))
;; Python:2 ends here

;; [[file:config.org::*Direnv][Direnv:1]]
(envrc-global-mode)
;; Direnv:1 ends here

;; [[file:config.org::*Nix][Nix:1]]
(map! :leader
      :after nix
      :map nix-mode-map
      :prefix ("s" . "search")
      :desc "search option" "o" #'helm-nixos-options)
;; Nix:1 ends here

;; [[file:config.org::*Nix][Nix:2]]
(setq flycheck-command-wrapper-function
      (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
      flycheck-executable-find
      (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))
;; Nix:2 ends here

;; [[file:config.org::*Nix][Nix:3]]
(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix))
;; Nix:3 ends here

;; [[file:config.org::*Nim][Nim:1]]
(require 'flycheck-nim)
;; Nim:1 ends here

;; [[file:config.org::*Performance][Performance:1]]
(explain-pause-mode nil)
;; Performance:1 ends here

;; [[file:config.org::*Enviroment][Enviroment:1]]
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; Enviroment:1 ends here

;; [[file:config.org::*Url proxy][Url proxy:1]]
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "*.i2p:4444")))
;; Url proxy:1 ends here

;; [[file:config.org::*Puff Count][Puff Count:1]]
(defun puff-add ()
  "Add a puff"
  (interactive)
  (shell-command "/run/current-system/sw/bin/puffer -a"))
(map!
 :leader
 :desc "add a puff"
 "]" #'puff-add)
;; Puff Count:1 ends here

;; [[file:config.org::*Cheat-sh][Cheat-sh:1]]
(defun cht-sh ()
  "look up a cheat"
  (interactive)
  (async-shell-command (format "cht.sh %s" (read-string "Enter search: "))))
(map! :leader
      :prefix ("s" . "search")
      :desc "cheat sheat" "c" #'cht-sh)
;; Cheat-sh:1 ends here

;; [[file:config.org::*Bookmarks][Bookmarks:1]]
(setq bookmark-file "~/Documents/Emacs/bookmarks")
;; Bookmarks:1 ends here
