(dolist (file (directory-files-recursively "~/.dotfiles/lisp" "\\.el$"))
  (load file))

(setq doom-theme 'doom-outrun-electric)

(setq ivan/themes '(doom-gruvbox-light doom-outrun-electric))
(setq ivan/themes-index 0)

(defun ivan/cycle-theme ()
  (interactive)
  (setq ivan/themes-index (% (1+ ivan/themes-index) (length ivan/themes)))
  (ivan/load-indexed-theme))

(defun ivan/load-indexed-theme ()
  (ivan/try-load-theme (nth ivan/themes-index ivan/themes)))

(defun ivan/try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Cycle The Theme" "T" #'ivan/cycle-theme))

(setq display-line-numbers-type t)

(setq frame-resize-pixelwise t)

;(setq
; doom-font (font-spec :family "Hack Regular Nerd Font Complete Mono" :size 12)
; doom-big-font (font-spec :family "Hack Bold Nerd Font Complete" :size 18)
; doom-variable-pitch-font (font-spec :family "Hack Regular Nerd Font Complete Mono" :size 12)
; doom-serif-font (font-spec :family "Hack Regular Nerd Font Complete Mono" :size 12))

(add-to-list 'display-buffer-alist
  (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defvar browse-url-brave-program "brave")
(defvar browse-url-brave-arguments nil)

(defun browse-url-brave (url &optional _new-window)
  "Ask the Brave browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-brave-arguments' are also passed to
Brave.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "brave" url) nil
	   browse-url-brave-program
	   (append
	    browse-url-brave-arguments
	    (list url)))))

(setq
 browse-url-browser-function
 '(
  ("wikipedia\\.org" . eww-browse-url)
  ("github" . browse-url-brave)
  ("." . browse-url-brave)
  ))

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

(after! 'magit
  (require 'forge))

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

(setq  org-capture-templates '(("m" "Personal Meditations")
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
                                "* IDEA %? %^g")
                               ("a" "Templates for AI")
                               ("ap" "Save a AI prompt for later" entry
                                (file+headline "~/Documents/Notes/org/ai-prompts.org" "Prompts")
                                "* %U %?\n%i\n%a" :prepend t)
                               ("ai" "LLM/AI Injection (Bypasses)" entry
                                (file+headline "~/Documents/Notes/org/ai-prompts.org" "Injections")
                                "* %U %?\n%i\n%a" :prepend t)
                               ("h" "templates for hacking notes")
                               ("hr" "Save a note about revsering a Piece of code" entry
                                (file+headline +org-capture-project-todo-file "Reversing") "* %U %?\n%i\n%a")
                               ("hs" "Save a note about a service" entry
                                (file+headline +org-capture-project-todo-file "Services") "* %U Port %?\n%i\n%a")
                               ("hl" "Save a note to check later" entry
                                (file+headline  +org-capture-project-todo-file "Check Later") "* %U %?\n%i\n%a")
                               ))

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
        (:and (:todo "TODO" :name "Personal" :tag ("personal")) :name "Personal")
        (:and (:todo "TODO" :name "Habits" :tag ("mow" "trash" "clean" "habit")) :name "Habits")
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
  (add-to-list 'org-structure-template-alist '("cl" . "src lisp"))
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

(setq deft-extenstions '("txt", "org", "md"))
(setq deft-directory "~/Documents/Notes")

(setq deft-recursive t)

(setq deft-use-filename-as-title t)

(require 'notifications)

(require 'elfeed-org)

(elfeed-org)

(setq rmh-elfeed-org-files '("~/Documents/Notes/org/rss.org"))

(add-hook 'elfeed-search-mode-hook 'turn-off-evil-mode)
(add-hook 'elfeed-show-mode-hook 'turn-off-evil-mode)

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
; BUG something is wrong with spc h f on nixos, works on arch
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

(setq atomic-chrome-buffer-open-style 'frame)

(setq eshell-aliases-file "~/.doom.d/eshell/aliases")

(require 'f)

(require 'dash)

(require 's)

(require 'alert)
(setq alert-default-style 'alert-libnotify-notify)
(setq alert-libnotify-command "dunstify")

(use-package! org-timed-alerts
  :after (org)
  :config
  (setq org-timed-alerts-alert-function #'alert-libnotify-notify)
  (setq org-timed-alerts-tag-exclusions nil)
  (setq org-timed-alerts-default-alert-props nil)
  (setq org-timed-alerts-warning-times '(-10 -5))
  (setq org-timed-alerts-agenda-hook-p t)
  (setq org-timed-alert-final-alert-string "IT IS %alert-time\n\n%todo %headline")
  (setq org-timed-alert-warning-string (concat "%todo %headline\n at %alert-time\n "
                                          "it is now %current-time\n "
                                          "*THIS IS YOUR %warning-time MINUTE WARNING*"))
  (add-hook! 'org-mode-hook #'org-timed-alerts-mode))

(use-package codeium
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    ;(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    (add-hook 'python-mode-hook
         (lambda ()
             (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(setq lsp-package-path (executable-find "pyright"))

(envrc-global-mode)

(add-to-list 'company-backends 'company-nixos-options)

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

(set-file-template! "\\.asd" :trigger "__.asd" :mode 'lisp-mode)

(set-file-template! "\\package.lisp" :trigger "__package.lisp" :mode 'lisp-mode)

(map! :after 'lispyville
      :map 'lispyville-mode-map
      "C-w" #'lispyville-move-up)
(map! :after 'evil
      :map 'lispyville-mode-map
      "C-s" #'lispyville-move-down)

(use-package! flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(explain-pause-mode t)

;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;(setq url-proxy-services
;   '(("no_proxy" . "^\\(localhost\\|10.*\\|\\.(?!i2p)[a-zA-Z0-9-]{1,255}$\\)")
;     ("http" . "127.0.0.1:4444")
;     ("https" . "127.0.0.1:4444")
;))
;(setq elfeed-use-curl nil)

(defun open-popup-on-side-or-below (buffer &optional alist)
  (+popup-display-buffer-stacked-side-window-fn
   buffer (append `((side . ,(if (one-window-p)
                                 'right
                               'bottom)))
                  alist)))

(add-to-list 'display-buffer-alist
  (cons "*cheat.sh*" (cons #'open-popup-on-side-or-below nil)))
(map! :leader
      :prefix ("s" . "search")
      :desc "cheat sheat" "c" #'cheat-sh)

(setq bookmark-file "~/Documents/Emacs/bookmarks")

(global-activity-watch-mode)

(require 'ks)

    (setq mastodon-instance-url "https://pleroma.nobodyhasthe.biz"
          mastodon-active-user "nott")

(with-system "flake"
             (require 'elcord)
             (elcord-mode))

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

(midnight-delay-set 'midnight-delay "12:00am")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry 1360)

(setq ppackage-template "~/.dotfiles/lisp/template")
(setq ppackage-path "~/.dotfiles/lisp")
