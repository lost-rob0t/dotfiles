;;; config.el --- Termux Doom configuration -*- lexical-binding: t; -*-

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

(defun star/termux-refresh-agenda-files ()
  "Refresh `org-agenda-files' from the Termux Org root."
  (interactive)
  (let ((agenda-root (expand-file-name "agenda" star/termux-org-root)))
    (setq org-agenda-files
          (if (file-directory-p agenda-root)
              (directory-files-recursively agenda-root "\\.org\\'")
            (directory-files-recursively star/termux-org-root "\\.org\\'")))))

(star/termux-refresh-agenda-files)

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

(setq initial-buffer-choice #'star/termux-open-todos)
