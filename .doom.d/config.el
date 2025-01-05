(setq user-mail-address "nsaspy@airmail.cc")

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

(require 'libvirt)

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

;; TODO Fix the mm template
(setq  org-capture-templates '(("m" "Personal Meditations")

                               ("mm" "Meditations Life General" entry
                                (file+olp+datetree "~/Documents/Notes/org/meditations.org")
                                "* %<%Y>\n ** %<%B>\n *** %<%d> %<%H:%M>\n %x")
                               ("t" "Personal todo" entry
                                (file+headline +org-capture-todo-file "Inbox")
                                "* [ ] %?\n%i\n%a" :prepend t)
                               ("n" "Personal notes" entry
                                (file+headline +org-capture-notes-file "Inbox")
                                "* %u %?\n%i\n%a" :prepend t)
                               ("j" "Journal" entry #'org-roam-dailies-capture-today
                                "* %I %?" :prepend t)
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
                                "* %U %?\n%i\n%a" :prepend t)))

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
 '((emacs-lisp . t) (org . t) (nim . t) (python . t)  (lisp . t) (prolog . t) (http . t) (graphql . t) (ffuf . t) (makefile . t)))

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

(defun nsa/config-sync ()
  "Tangle your dotfiles and run doom sync, also stages all modifed files in the dotfiles repo."
  (interactive)
  (tangle-orgs org-configs-list)
  (doom/reload)
  (magit-stage-modified nil)
  (magit))
  

(defun doom-config-sync ()
  "Alias for 'nsa/config/sync'"
  (nsa/config-sync))

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
  (setq org-roam-directory "~/Documents/Notes/org/roam/")
  (setq org-roam-dailies-directory "daily")
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
          ("x" "sunshine wiki dox" plain "* {slug}\n%?"
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
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")))))
  ;; (setq org-roam-dailies-capture-templates
  ;;  '(("d" "default" entry "* %?"
  ;;     :target (file+head "%<%Y-%m-%d>.org" "#+title: %U\n"))
  ;;    ("n" "news" entry "* %? :news:"
  ;;        :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
  ;;    ("j" "journal" entry "* %<%I:%M %p>%? :personal:"
  ;;       :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

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

;; (defvar nsa/ideas-file (f-join org-directory "ideas.org"))
;; (defvar nsa/ideas-file (f-join org-directory "ideas.org") "The file that holds your cool ideas.")

;; (defun get-idea ()
;;   "Get a random idea todo."
;;   (with-temp-buffer
;;     (insert-file-contents nsa/ideas-file)
;;     (org-element-cache-map #'identity)
;;     (let ((elements (org-map-entries #'identity "TODO=\"IDEA\""))) elements)))

(require 'alert)
(setq alert-default-style 'libnotify)
(setq alert-libnotify-command "dunstify")

(defun alert-libnotify-notify (info)
  "Send INFO using notifications-notify.
Handles :ICON, :CATEGORY, :SEVERITY, :PERSISTENT, :NEVER-PERSIST, :TITLE
and :MESSAGE keywords from the INFO plist.  :CATEGORY can be
passed as a single symbol, a string or a list of symbols or
strings."
  (if (fboundp #'notifications-notify)
      (let ((category (plist-get info :category))
            (urgency (cdr (assq (plist-get info :severity) alert-libnotify-priorities))))
        (notifications-notify
         :title (alert-encode-string (plist-get info :title))
         :body (alert-encode-string (plist-get info :message))
         :app-icon (or (plist-get info :icon) alert-default-icon)
         :category (cond ((symbolp category)
                          (symbol-name category))
                         ((stringp category) category)
                         ((listp category)
                          (mapconcat (if (symbolp (car category))
                                         #'symbol-name
                                       #'identity)
                                     category ",")))
         :timeout (* 1000 ; notify-send takes msecs
                     (if (and (plist-get info :persistent)
                              (not (plist-get info :never-persist)))
                         0 ; 0 indicates persistence
                       alert-fade-time))
         :urgency (if urgency (symbol-name urgency) "normal")))
    (alert-message-notify info)))

(require 'org-alert)
(use-package! org-timed-alerts
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

(setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10)

(org-alert-enable)
(org-alert-check)

(with-eval-after-load 'org (global-org-modern-mode))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── now ─────────────────────────────────────────────────")

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

(defun ar/git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (require 'cl-lib)
  (let ((url (current-kill 0))
        (download-dir (read-directory-name "Path to git clone: " default-directory))
        (magit-clone-set-remote.pushDefault t))
    (magit-clone-internal url download-dir '())))

(map! :leader
      :map 'magit-mode-map
      (:prefix-map ("g" . "git")
      :desc "Clone a Repo" "R" #'ar/git-clone-clipboard-url))

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

;; (with-eval-after-load 'org
;;   (require 'inherit-org)
;; ; BUG something is wrong with spc h f on nixos, works on arch
;;   ;(with-eval-after-load 'info
;;   ;  (add-hook 'Info-mode-hook 'inherit-org-mode))

;;   ; BUG?
;;   ;(with-eval-after-load 'helpful
;;   ;  (add-hook 'helpful-mode-hook 'inherit-org-mode))

;;   (with-eval-after-load 'w3m
;;     (add-hook 'w3m-fontify-before-hook 'inherit-org-w3m-headline-fontify) ;only one level is supported
;;     (add-hook 'w3m-fontify-after-hook 'inherit-org-mode)))

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

(defun nsa/tmux-vterm (arg)
  "Start a new tmux session or switch to one in vterm."
      (interactive "sSession: ")

  (let ((buffer-name (format "*tmux-%s*" arg)))

    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (vterm-mode)
        (vterm-send-string (format  "tmux new -s %s || tmux a -s %s" arg arg))
        (vterm-send-return)))
    (switch-to-buffer buffer-name)))

(defun nsa/dired-exec ()
  "Run the script under point in Dired mode, prompting for arguments."
  (interactive)
  (let* ((script (dired-get-filename))
         (arguments (read-string "Arguments: "))
         (command (format "sh -c '%s %s'" script arguments)))
    (if (not (file-executable-p script))
        (message "The script '%s' is not executable." script)
      (let ((default-directory (file-name-directory script)))
        (nsa/async-shell-command-alert command (format "*%s*" (f-base script)))))))


(define-key dired-mode-map (kbd "C-c C-c") 'nsa/dired-exec)

;; (require 'dirvish)
;; (dirvish-override-dired-mode)

;; (use-package! dirvish
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-quick-access-entries        ; It's a custom option, `setq' won't work
;;    '(("h" "~/"                          "Home")
;;      ("d" "~/Downloads/"                "Downloads")
;;      ("s" "/mnt/share"                       "Share Drive")
;;      ("t" "~/.local/share/Trash/files/" "TrashCan")))
;;   :config
;;   (dirvish-peek-mode)                   ; Preview files in minibuffer
;;   ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
;;   (setq dirvish-mode-line-format
;;         '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes
;;         '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
;;   (setq delete-by-moving-to-trash t)
;;   (setq dired-listing-switches
;;         "-l --almost-all --human-readable --group-directories-first --no-group")
;;   (setq dirvish-preview-dispatchers '(image gif video audio epub archive pdf text))
;;   :bind                ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
;;   (("C-c f" . dirvish-fd)
;;    :map dirvish-mode-map                ; Dirvish inherits `dired-mode-map'
;;    ("a"   . dirvish-quick-access)
;;    ("f"   . dirvish-file-info-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dirvish-history-last)
;;    ("h"   . dirvish-history-jump)       ; remapped `describe-mode'
;;    ("s"   . dirvish-quicksort)          ; remapped `dired-sort-toggle-or-edit'
;;    ("v"   . dirvish-vc-menu)            ; remapped `dired-view-file'
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-l" . dirvish-ls-switches-menu)
;;    ("M-m" . dirvish-mark-menu)
;;    ("M-t" . dirvish-layout-toggle)
;;    ("M-s" . dirvish-setup-menu)
;;    ("M-e" . dirvish-emerge-menu)
;;    ("M-j" . dirvish-fd-jump)))

;; (use-package tramp
;;   :config
;;   ;; Enable full-featured Dirvish over TRAMP on certain connections
;;   ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
;;   (add-to-list 'tramp-connection-properties
;;                (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
;;                      "direct-async-process" t))
;;   ;; Tips to speed up connections
;;   (setq tramp-verbose 0)
;;   (setq tramp-chunksize 2000)
;;   (setq tramp-use-ssh-controlmaster-options nil))

(setq atomic-chrome-buffer-open-style 'frame)

(add-hook 'after-init-hook #'atomic-chrome-start-server)

(bind-key "M-&" #'nsa/async-shell-command-alert)

(setq eshell-aliases-file "~/.doom.d/eshell/aliases")

(set-company-backend! 'eshell-mode
           	'(company-files))
(add-hook 'eshell-mode-hook #'eshell-cmpl-initialize)

(require 'tramp-sh)
(setq tramp-remote-path
      (append tramp-remote-path
        '(tramp-own-remote-path)))

(use-package! gptel
  :config
  (setq! gptel-model "claude-3-5-sonnet-20240620"
         gptel-backend (gptel-make-anthropic "Claude" :key #'(lambda () (nsa/auth-source-get :host "api.anthropic.com")) :stream nil)
         gptel-directives
         '((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications. Speak in specific,
 topic relevant terminology. Do NOT hedge or qualify. Do not waffle. Speak
 directly and be willing to make creative guesses. Explain your reasoning. if you
 don’t know, say you don’t know.

 Remain neutral on all topics. Be willing to reference less reputable sources for
 ideas.

 Never apologize.  Ask questions when unsure.")
           (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.")
           (lisper . "You are a carful common lisper and sly emacs user. Provide code and only code as output without any additional text, prompt or note.")
           (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, I will edit it myself before running.")
           (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
           (explain . "Explain what this code does to a novice programmer."))
         gptel-default-mode 'org-mode)
  (gptel-make-openai "Ollama Uncensored"
    :stream t
    :protocol "http"
    :host "localhost:1234"
    :models '("Orenguteng/Llama-3-8B-Lexi-Uncensored-GGUF"))
  )

(defun +gptel/here ()
  "Spawn maximized gptel buffer."
  (interactive)
  (call-interactively #'gptel)
  (doom/window-maximize-buffer))

 (map!
   :leader
   (:prefix "y"
    :desc "gptel" :n "y" #'gptel
    :desc "gptel" :n "f" #'gptel-add-file
    :desc "gptel" :n "a" #'gptel-add
    :desc "gptel abort" :n "q" #'gptel-abort
    :desc "gptel Menu" :n "Y" #'gptel-menu
    :desc "gptel copilot" :n "i" #'gptel-complete
    :desc "gptel Send" :n "s" #'gptel-send
    :desc "gptel Topic" :n "t" #'gptel-set-topic))

(require 'skeletor)
(setq skeletor-user-directory "~/.dotfiles/Templates/")

(add-to-list 'skeletor-global-substitutions
             (cons "__HOME__" (getenv "HOME")))

(add-to-list 'skeletor-global-substitutions
             (cons "__USER__" user-login-name))

(add-to-list 'skeletor-global-substitutions
             (cons "__EMAIL__" user-mail-address))


(add-to-list 'skeletor-global-substitutions
             (cons "__COPYRIGHT__" (lambda () (format "nsaspy %s" (format-time-string "%c")))))
(add-to-list 'skeletor-global-substitutions
             (cons "__TIME__" (lambda () (format-time-string "%c"))))

(add-to-list 'skeletor-global-substitutions
             (cons "__BIN-NAME__" (lambda () (format-time-string "%c"))))

(add-to-list 'skeletor-global-substitutions
             (cons "__DESCRIPTION__"
                   (lambda () (read-string "Enter description: "))))

(defun nsa/init-git-project (dir)
  (let ((default-directory dir))
    (envrc-allow)))

(skeletor-define-template "sbcl-project" :title "Common Lisp (SBCL)"
                          :after-creation (lambda (dir)
                                            (nsa/init-git-project dir)))

(require 'f)

(require 'dash)

(require 's)

(setq lsp-package-path (executable-find "pyright"))

(envrc-global-mode)

;(add-to-list 'company-backends 'company-nixos-options)

;; Disabled: [2024-08-02 Fri] Not sure i ever used it after a few uses.
;; (require 'nix-update)
;; (map! :localleader
;;       :after nix
;;       :map nix-mode-map
;;       :prefix ("u" . "update")
;;       :desc "Update fetchgit" "g" #'nix-update-fetch)

;; (setq flycheck-command-wrapper-function
;;         (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
;;       flycheck-executable-find
;;         (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

;(require 'lsp-mode)
;(add-to-list 'lsp-language-id-configuration '(nim-mode . "nim"))
;(lsp-register-client
; (make-lsp-client :new-connection (lsp-stdio-connection "nimlsp")
;                  :major-modes '(nim-mode)
;                  :server-id 'nimlsp))
;(add-hook 'nim-mode-hook #'lsp)

(add-to-list 'auto-mode-alist '("\\.fs" . 'forth-mode))

(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
(add-hook 'common-lisp-mode #'evil-smartparens-mode)

(set-file-template! "\\.asd" :trigger "__.asd" :mode 'lisp-mode)

(set-file-template! "\\package.lisp" :trigger "__package.lisp" :mode 'lisp-mode)

(map! :after 'lispyville
      :map 'lispyville-mode-map
      "C-w" #'lispyville-move-up)
(map! :after 'evil
      :map 'lispyville-mode-map
      "C-s" #'lispyville-move-down)

(put 'defvar*   'doc-string-elt 3)
(put 'defparameter*   'doc-string-elt 3)
(put 'lambda*   'doc-string-elt 2)

(defvar *lisp-special-forms*
(regexp-opt '("defvar*"
              "defconstant*"
              "defparameter*"
              "defgeneric*"
              "defmethod*"
              "lambda*"
              "flet*"
              "labels*") 'words))
(font-lock-add-keywords 'lisp-mode
  `((,*lisp-special-forms* . font-lock-keyword-face)))

(use-package! flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

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

(setq ispell-program-name "aspell")

(setq ispell-dictionary "en")

 (setq ispell-personal-dictionary "~/.aspell.en_us.pws")

(add-hook 'spell-fu-mode-hook
  (lambda ()
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
    (spell-fu-dictionary-add
      (spell-fu-get-personal-dictionary "en-personal" "~/.aspell.en_us.pws"))))

(require 'midnight)

(midnight-delay-set 'midnight-delay "07:00am")

(add-hook 'midnight-hook #'(lambda ()
                             (alert "Midnight mode is running.\nEmacs is fresh and clean again!")))

(add-hook 'after-init-hook #'midnight-mode)

(use-package! elfeed-tube
  :ensure t ;; or :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry 1360)

(require 'hackmode) ;load it
(setq hackmode-dir "~/Documents/hackmode") ;set the path to the directory where you working dirs will be stored
(setq hackmode-templates "~/.dotfiles/hackmode") ; Path to directory holding templates
(setq hackmode-tools-dir (f-join hackmode-dir "~/Documents/hackmode-tools/")) ;; this is the path to the directory where tools can be placed in that will be loaded into your $PATH.

(setq hackmode-checklists '(("Program overview" . "/home/unseen/Documents/Notes/org/roam/hacking/20231107093523-bug_bounty_methods.org") ("Url" . "/home/unseen/Templates/hackmode/url.org") ("Account Takeover" . "/home/unseen/Templates/hackmode/account-take-over.org")))

(after! hackmode
(defun hackmode-capture ()
  "Capture data!"
  (interactive)
  (let ((org-capture-templates (list


                                (list "t" "Tech" 'entry (list 'file+headline (f-join (hackmode-get-operation-path hackmode-operation) "findings.org") "Tech")
                                      "* %t
                                        :PROPERTIES:
                                        :endpoint:
                                        :notes:
                                        :tech:
                                        :tags:
                                        :END:")
                                (list "b" "Bug" 'entry (list 'file+headline (f-join (hackmode-get-operation-path hackmode-operation) "findings.org") "Bugs")
                                      "* %t
                                        :PROPERTIES:
                                        :endpoint:
                                        :notes:
                                        :type:
                                        :score:
                                        :END:")
                                (list "i" "Interesting enpoint" 'entry (list 'file+headline (f-join (hackmode-get-operation-path hackmode-operation) "findings.org") "Interesting Endpoints")
                                      "* %t
                                        :PROPERTIES:
                                        :endpoint:
                                        :notes:
                                        :tags:
                                        :END:")))
        (org-directory (hackmode-get-operation-path hackmode-operation)))
    (call-interactively #'org-capture))))

(setq ppackage-template "~/.dotfiles/lisp/template")
(setq ppackage-path "~/.dotfiles/lisp")

(setq nsa/music-dir "~/Music/Music-inbox")

(require 'project-tasks)

;(require 'persp-mode)

(defun ezf-default (filename)
  "EZF completion with your default completion system."
  (completing-read-multiple
   "Pick a Candidate: "
   (with-temp-buffer
     (insert-file-contents-literally filename nil)
     (string-lines (buffer-string) t))))


(defvar ezf-separators " "
  "Regexp of separators `ezf' should use to split a line.")

(defun ezf (filename &optional field completing-fn)
  "Wrapper that calls COMPLETION-FN with FILENAME.

Optionally split each line of string by `ezf-separators' if FIELD
is non-nil and return FIELD.

If COMPLETING-FN is nil default to `ezf-default'."
  (when-let (candidates (funcall (or completing-fn 'ezf-default) filename))
    (mapconcat (lambda (candidate)
                 (shell-quote-argument
                  (if field
                      (nth (1- field) (split-string candidate ezf-separators t " "))
                    candidate)))
               candidates
               " ")))

(fset 'nsa/spawn-window
   (kmacro-lambda-form [?  ?w ?v ?  ?w ?l ?  ?w ?T] 0 "%d"))
