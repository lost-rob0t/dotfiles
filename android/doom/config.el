;;; config.el --- Termux Doom configuration -*- lexical-binding: t; -*-

(require 'seq)

(setq doom-theme 'doom-one
      display-line-numbers-type 'relative
      org-log-done 'time
      org-log-into-drawer t
      org-return-follows-link t
      org-use-speed-commands t)

(defconst star/termux-home
  (file-name-as-directory
   (or (getenv "HOME")
       "/data/data/com.termux/files/home")))

(defun star/termux--first-directory (&rest paths)
  "Return the first existing directory in PATHS."
  (seq-find #'file-directory-p paths))

(defun star/termux--first-file (&rest paths)
  "Return the first existing regular file in PATHS."
  (seq-find #'file-regular-p paths))

(defconst star/termux-org-root
  (file-name-as-directory
   (or (getenv "STAR_ORG_ROOT")
       (star/termux--first-directory
        (expand-file-name "Documents/Notes/org" star/termux-home)
        (expand-file-name "storage/shared/Documents/Notes/org" star/termux-home)
        (expand-file-name ".local/share/org" star/termux-home))
       (expand-file-name ".local/share/org" star/termux-home))))

(make-directory star/termux-org-root t)
(setq org-directory star/termux-org-root)

;; The desktop config runs its agenda updater while loading (after!
;; org-agenda), scanning nsa/org-agenda-directory.  Pre-bind it to the
;; phone's agenda directory so the same code path works here; the desktop
;; defvar keeps our value.
(make-directory (expand-file-name "agenda" star/termux-org-root) t)
(defvar nsa/org-agenda-directory (expand-file-name "agenda/" star/termux-org-root))

(defun star/termux-refresh-agenda-files ()
  "Refresh `org-agenda-files' from the Termux Org root."
  (interactive)
  (let ((agenda-root (expand-file-name "agenda" star/termux-org-root)))
    (setq org-agenda-files
          (if (file-directory-p agenda-root)
              (directory-files-recursively agenda-root "\\.org\\'")
            (directory-files-recursively star/termux-org-root "\\.org\\'")))))

(defconst star/termux-desktop-doomdir nil
  "Desktop Doom configuration directory reused by this profile, or nil.")

(defun star/termux--prune-uninstalled-babel (args)
  "Drop babel languages from ARGS whose ob-* library is not installed.

The desktop config enables languages such as graphql and ffuf whose ob-
libraries were never declared in .doom.d/packages.el; they exist on the
desktop only as old straight leftovers.  org-babel-do-load-languages
\=`require's each library, which would abort the whole desktop config
load on a fresh Android checkout."
  (cl-destructuring-bind (sym alist) args
    (list sym
          (mapcar (lambda (entry)
                    (if (and (cdr entry)
                             (not (locate-library
                                   (format "ob-%s" (car entry)))))
                        (cons (car entry) nil)
                      entry))
                  alist))))

(advice-add 'org-babel-do-load-languages :filter-args
            #'star/termux--prune-uninstalled-babel)

(defun star/termux-load-desktop-config ()
  "Load the desktop Doom config.el from the dotfiles checkout.

The desktop tree is only ever read.  Loading happens before the Android
overrides below, so those always win on the phone.  A desktop config that
fails to load on Android must not take down the whole startup."
  (let ((dir (or (getenv "STAR_DESKTOP_DOOMDIR")
                 (and (boundp 'doom-private-dir)
                      doom-private-dir
                      (let ((sibling
                             (expand-file-name "../../.doom.d" doom-private-dir)))
                        (and (file-regular-p (expand-file-name "config.el" sibling))
                             sibling)))
                 (seq-find (lambda (candidate)
                             (file-regular-p
                              (expand-file-name "config.el" candidate)))
                           '("~/.dotfiles/.doom.d" "~/dotfiles/.doom.d")))))
    (if (not dir)
        (message "Termux Doom: no desktop config found; running Termux-only setup")
      (setq star/termux-desktop-doomdir (directory-file-name dir))
      ;; The desktop config adds its personal lisp tree to `load-path'
      ;; through `nsa/lisp-directory', defaulting to ~/.dotfiles/lisp.  The
      ;; checkout may live elsewhere (e.g. ~/dotfiles), so pre-bind it from
      ;; the resolved checkout root; the desktop `defvar' keeps our value.
      ;; Without this the desktop config aborts before anything loads.
      (let ((checkout
             (directory-file-name
              (file-name-directory (directory-file-name dir)))))
        (when (file-directory-p (expand-file-name "lisp" checkout))
          (defvar nsa/lisp-directory (expand-file-name "lisp/" checkout))))
      (condition-case err
          (load (expand-file-name "config.el" dir) nil 'nomessage)
        (error (message "Termux Doom: desktop config failed to load: %S"
                        err))))))

(star/termux-load-desktop-config)

;;; Android overrides -- applied after the desktop config, always win.

;; Org roots follow the Termux Org root, not the desktop literals.
(setq org-directory star/termux-org-root)
(setq org-roam-directory (expand-file-name "roam" star/termux-org-root))
(star/termux-refresh-agenda-files)

;; Desktop config sync and Eshell aliases point at the reused checkout.
(when star/termux-desktop-doomdir
  (setq org-configs-list
        (list (expand-file-name "config.org" star/termux-desktop-doomdir)
              (expand-file-name "packages.org" star/termux-desktop-doomdir)))
  (after! eshell
    (setq eshell-aliases-file
          (expand-file-name "eshell/aliases" star/termux-desktop-doomdir))))

;; No dunst/dbus on Android: alert falls back to the echo area.
(after! alert
  (setq alert-default-style 'message))

;; No Brave on Android: termux-open-url hands URLs to the Android browser,
;; eww stays available for Wikipedia-style reading.
(defun star/termux-browse-url (url &optional _new-window)
  "Open URL with termux-open-url, falling back to eww."
  (if (and (executable-find "termux-open-url")
           (eq (call-process "termux-open-url" nil nil nil url) 0))
      url
    (eww-browse-url url)))

(setq browse-url-handlers
      '(("wikipedia\\.org" . eww-browse-url)
        ("." . star/termux-browse-url)))

;; Boot into the phone TODO list, not the desktop agenda.
;; `initial-buffer-choice' must return a live buffer; `org-todo-list'
;; only returns t.
(defun star/termux--startup-buffer ()
  "Open the Termux TODO list as the startup buffer."
  (star/termux-open-todos)
  (get-buffer "*Org Agenda*"))

(setq initial-buffer-choice #'star/termux--startup-buffer)

;;; Termux-native Org and languages.

(after! org
  (setq org-startup-indented t
        org-startup-folded 'content
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (add-to-list 'org-modules 'org-habit)

  (dolist (entry '((emacs-lisp . t)
                   (lisp . t)
                   (python . t)
                   (shell . t)))
    (setf (alist-get (car entry) org-babel-load-languages) (cdr entry)))

  (when (locate-library "ob-prolog")
    (setf (alist-get 'prolog org-babel-load-languages) t))
  (when (locate-library "ob-http")
    (setf (alist-get 'http org-babel-load-languages) t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

(after! sly
  (setq inferior-lisp-program "sbcl"))

(after! python
  (setq python-shell-interpreter "python"))

(after! prolog
  (setq prolog-system 'swi))

(use-package! nim-mode
  :mode "\\.nim\\'")

(use-package! org-ql
  :after org)

(use-package! gptel
  :commands (gptel gptel-send gptel-menu))

(defun star/termux-starintel-admin ()
  "Open the StarIntel admin Org workspace on Android."
  (interactive)
  (let* ((explicit (getenv "STARINTEL_ADMIN_ORG"))
         (server-root (expand-file-name "src/starintel-server" star/termux-home))
         (target
          (or (and explicit (file-regular-p explicit) explicit)
              (star/termux--first-file
               (expand-file-name "starintel-admin.org" star/termux-org-root)
               (expand-file-name "admin.org" server-root)
               (expand-file-name "docs/admin.org" server-root)
               (expand-file-name "TODO.org" server-root))
              (expand-file-name "starintel-admin.org" star/termux-org-root))))
    (unless (file-exists-p target)
      (with-temp-file target
        (insert "#+title: StarIntel Admin\n#+startup: overview\n\n"
                "* Inbox\n"
                "* Operations\n"
                "* Notes\n")))
    (find-file target)))

(defun star/termux-open-todos ()
  "Refresh agenda files and show the global TODO list."
  (interactive)
  (star/termux-refresh-agenda-files)
  (org-todo-list))

(defun star/termux-open-agenda ()
  "Refresh agenda files and show the agenda."
  (interactive)
  (star/termux-refresh-agenda-files)
  (org-agenda nil "a"))

(defun star/termux-ai ()
  "Open gptel inside Doom."
  (interactive)
  (call-interactively #'gptel))

(map! :leader
      (:prefix ("o" . "open")
       :desc "StarIntel admin" "A" #'star/termux-starintel-admin
       :desc "Org agenda" "a" #'star/termux-open-agenda
       :desc "Org TODOs" "t" #'star/termux-open-todos
       :desc "AI chat" "i" #'star/termux-ai))

(add-hook! 'org-mode-hook
  (visual-line-mode 1)
  (org-indent-mode 1))
