(defvar nsaspy/system "desktop"
        "platform name to base config for.
        it will load the name from ~/.platform")

(defun nsaspy/load-platform (path)
  "load the paatform name from file"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq doom-theme 'doom-outrun-electric)

(setq display-line-numbers-type t)

(setq frame-resize-pixelwise t)

(setq
 doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12)
 doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)
 doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 12)
 doom-serif-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))

(map! :leader
      :desc "Push Current branch to remote branch"
      "g p P" #'magit-push-current-to-pushremote)

(map! :leader
      :desc "Pull current branch from remote"
      "g p p" #'magit-pull-from-pushremote)

(map! :leader
      :map 'magit-mode-map
      (:prefix-map ("g" . "git")
       (:prefix ("c" . "create")
      :desc "Create new git tag" "t" #'magit-tag-create)))

(require 'magit-todos)

(setq projectile-project-search-path
      '(("~/Documents/Projects" . 1)))

(setq org-directory "~/Documents/Notes/org")

(setq time-stamp-active t
      time-stamp-start "#\\+LAST_MODIFIED:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
(add-hook 'before-save-hook 'time-stamp nil)

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

(setq  org-capture-templates (quote (("m" "Personal Meditations")
                                     ("mm" "Meditations Moon" entry
                                      (file+olp+datetree "~/Documents/Notes/org/moon.org")
                                      "** Relections\n\n*** Acomplished\n\n*** Thoughts\n\n*** Happenings\n\n** Plans for next moon\n" :tree-type month)
                                     ("t" "Personal todo" entry
                                      (file+headline +org-capture-todo-file "Inbox")
                                      "* [ ] %?\n%i\n%a" :prepend t)
                                     ("n" "Personal notes" entry
                                      (file+headline +org-capture-notes-file "Inbox")
                                      "* %u %?\n%i\n%a" :prepend t)
                                     ("j" "Journal" entry
                                      (file+olp+datetree +org-capture-journal-file)
                                      "* %U %?\n%i\n%a" :prepend t)
                                     ("p" "Templates for projects")
                                     ("pt" "Project-local todo" entry
                                      (file+headline +org-capture-project-todo-file "Inbox")
                                      "* TODO %?\n%i\n%a" :prepend t)
                                     ("pn" "Project-local notes" entry
                                      (file+headline +org-capture-project-notes-file "Inbox")
                                      "* %U %?\n%i\n%a" :prepend t)
                                     ("pc" "Project-local changelog" entry
                                      (file+headline +org-capture-project-changelog-file "Changelog")
                                      "* %U %?\n%i\n%a" :prepend t)
                                     ("o" "Centralized templates for projects")
                                     ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                                     ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
                                     ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
                                     ("i" "Ideas Box" entry (file+headline "~/Documents/Notes/org/ideas.org" "Ideas")
                                      "* IDEA %? %^g"))))

(setq org-agenda-files (directory-files-recursively "~/Documents/Notes/org/agenda/" "\\.org$"))
;(dolist (file (directory-files-recursively "~/Documents/Notes/org/roam/" "\\.org$"))
;  (add-to-list org-agenda-files file))

(defun org-agenda-update-files ()
  "Update the org-agenda-files"
  (interactive)
  (setq org-agenda-files (directory-files-recursively "~/Documents/Notes/org/agenda" "\\.org$")))
(map! :leader
      :desc "update agenda"
      "o a u" #'org-agenda-update-files)

(defun track-org-file ()
  "Create a symbolic link to the current file in the 'agenda' directory."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when current-file
      (make-symbolic-link current-file "~/Documents/Notes/org/agenda/")
      (setq org-agenda-files (directory-files-recursively "~/Documents/Notes/org/agenda/" "\\.org$")))))

(map! :after org
      :localleader
      :map org-mode-map
      :desc "Add file to Org agenda" "w" #'track-org-file)

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
        (:and (:todo "IDEA" :name "Starintel Idea" :tag ("starintel" "sit")) :name "Starintel ideas")
        (:and (:todo "TODO" :name "Starintel Bugs" :tag ("starintel-bug" "sib")) :name "Star intel Bugs")
        (:and (:todo "TODO" :name "Starintel" :tag ("starintel")) :name "Star Intel")
        (:and (:todo "TODO" :name "Personal" :tag ("mow" "trash" "personal")) :name "Personal")
        (:and (:todo "TODO" :name "Emacs" :tag ("emacs")) :name "Emacs")
        (:and (:todo "TODO" :name "Read inbox" :tag ("book" "artical" "books")) :name "Reading")))

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
  "Setup for lsp-mode in Org Src buffer using BABEL-INFO."
  (setq-local default-directory (->> babel-info caddr (alist-get :dir)))
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(defun org-babel-edit-prep:sh (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

(with-eval-after-load 'org
  ;; is needed as of Org 9.2
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
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n") :unnarrowed t)
          ("t" "tutorial" plain "*%?"
           :target (file+head "Tutorial/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"))
          ("h" "hacking" plain "%?"
           :target (file+head "hacking/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"))
          ("s" "star intel" plain "*%? %^g"
           :target (file+head "starintel/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"))
          ("d" "sunshine wiki dox" plain "* {slug}\n%?"
           :target (file+head "starintel/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"))
          ("r" "Reading notes" plain "%?"
           :target (file+head "reading-notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"))
          ("v" "Video notes" plain "%?"
           :target (file+head "reading-notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"))
          ("p" "Programming" plain "%?"
           :target (file+head "programming/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"))))
   (setq org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
     ("n" "news" entry "* %? :news:"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
     ("j" "journal" entry "* %<%I:%M %p>%? :personal:"
        :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

(defun url2org (begin end)
  "Download a webpage from selected url and convert to org."
  (interactive "r")
  (shell-command-on-region begin end
                           (concat "pandoc --from=html --to=org " (buffer-substring begin end))
                           nil t))

(require 'epa-file)
(epa-file-enable)

(setq epa-file-encrypt-to '("nsaspy@airmail.cc"))

(setq epa-file-select-keys "235327FBDEFB3719")

(map! :localleader
      :after org
      :map org-mode-map
      :prefix ("c" . "clock")
      :desc "Start Pomodoro" "T" #'org-pomodoro)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)
;;; Centering Org Documents --------------------------------
;; Configure fill width
(setq visual-fill-column-width 180
      visual-fill-column-center-text t)

;;; Org Present --------------------------------------------

;; Install org-present if needed

(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun my/org-present-start ()
  ;; Tweak font sizes
  (doom-big-font-mode)
  (org-present-read-only)
  (org-present-hide-cursor)
  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")
  ;; Hide line numbers
  (setq-local display-line-numbers nil)
  ;; Display inline images automatically
  (org-display-inline-images)

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  ;; Reset font customizations
  (doom-big-font-mode)
  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)
  ;; Shone line numbers
  (setq-local display-line-numbers t)
  ;; Stop displaying inline images
  (org-remove-inline-images)
  (org-present-read-write)
  (org-present-show-cursor))
  ;; Stop centering the document


;; Turn on variable pitch fonts in Org Mode buffers
;(add-hook! 'org-mode variable-pitch-mode)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(defun update-timestamps (directory)
  "Update timestamps in all org files in DIRECTORY."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively directory "\\.org$")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (time-stamp))))))

(defun update-timestamps-in-directory (directory)
  "Update timestamps in all org files in DIRECTORY."
  (let ((files (directory-files-recursively directory "\\.org$")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (time-stamp))))))



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

(use-package! dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(setq lsp-package-path (executable-find "pyright"))

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
   '(("no_proxy" . "^\\(localhost\\|10.*\\|\\.(?!i2p)[a-zA-Z0-9-]{1,255}$\\)")
     ("http" . "127.0.0.1:4444")
     ("https" . "127.0.0.1:4444")
))
(setq elfeed-use-curl nil)

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

(use-package! org-pomodoro
  :init
  (setq org-pomodoro-audio-player "/usr/bin/mpv"))

(setq ispell-program-name "aspell")

(setq ispell-dictionary "en")

 (setq ispell-personal-dictionary "~/.aspell.en_us.pws")

(add-hook 'spell-fu-mode-hook
  (lambda ()
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
    (spell-fu-dictionary-add
      (spell-fu-get-personal-dictionary "en-personal" "~/.aspell.en_us.pws"))))

(require 'midnight)

(midnight-delay-set 'midnight-delay "7:00am")
