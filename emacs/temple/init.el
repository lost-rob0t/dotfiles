;;; Temple Oracle - Complete Android Config
;;; Packaged from temple.org and temple.el
;; -*- lexical-binding: t; -*-

;;; Android Detection
(defun android-p ()
  "Return t if Emacs is running on Android, nil otherwise."
  (or (eq system-type 'android)
      (featurep 'android)))

;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package via straight
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

;;; Basic Emacs Setup
(setq debug-on-error t)
(setq message-log-max 10000)
(setq
 visible-bell t
 inhibit-startup-message t
 inhibit-startup-screen t
 initial-scratch-message nil
 ring-bell-function 'ignore)

;; Display Line numbers
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

;; Disable toolbar and menus UNLESS it is android
(unless (android-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Package setup (backup to straight.el)
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA" . "https://melpa.org/packages/"))
      package-archive-priorities '(("MELPA Stable" . 10) ("GNU ELPA" . 5) ("MELPA" . 0)))
(package-initialize)

;; Bootstrap `use-package' (fallback)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Evil Mode
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

(use-package undo-tree :ensure t)
(use-package undo-fu :ensure t)

;;; Lines
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Lisp Support
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode scheme-mode clojure-mode) . paredit-mode)
  :config
  ;; Making paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-open-round 'delete-selection t)
  (put 'paredit-open-square 'delete-selection t)
  (put 'paredit-doublequote 'delete-selection t)
  (put 'paredit-newline 'delete-selection t))

;;; Code Navigation & Evaluation
;; Highlight the sexp at point
(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-colors '("#ff6c6b" "#98be65" "#da8548" "#51afef")))

;; Show matching parens
(use-package paren
  :config
  (setq show-paren-delay 0.0
        show-paren-style 'mixed
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1))

;;; Prolog Support
(use-package prolog
  :ensure t
  :mode (("\\.pl\\'" . prolog-mode)
         ("\\.pro\\'" . prolog-mode))
  :config
  (setq prolog-system 'swi)
  (setq prolog-program-name "swipl")
  (setq prolog-indent-width 4)
  (add-hook 'prolog-mode-hook 'prolog-electric-mode))

(use-package ediprolog
  :ensure t
  :after prolog
  :config
  (setq ediprolog-program "swipl")
  (add-hook 'prolog-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'ediprolog-dwim))))

;;; Development Tools
(use-package macrostep
  :ensure t
  :after elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c e m" . macrostep-expand)))

(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

;;; Code Folding
(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  (setq hs-hide-comments-when-hiding-all nil))

;;; Org Mode Setup
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory "~/Documents/Notes/org/roam/")
  (setq org-roam-dailies-directory "daily")
  (setq org-roam-complete-everywhere t)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
           :unnarrowed t)
          ("t" "temple" plain "* %?\n** Oracle\n\n** Reflection\n\n"
           :target (file+head "temple/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+FILETAGS: :temple:\n\n"))))
  :config
  (org-roam-db-autosync-mode))

(use-package org-ql :ensure t)

;;; Babel
(use-package ob-async :ensure t)
(use-package ob-prolog :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (prolog . t)))

(setq org-confirm-babel-evaluate nil)

;;; Org Tempo
(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("pl" . "src prolog"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

;;; Keybinding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; Set up SPC as the global leader key
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;;; Selection
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1))

;;; Projects
(use-package projectile
  :config
  (setq projectile-project-search-path
        '(("~/Documents/Projects" . 1))))

;;; Git
(use-package magit :ensure t)

;;; Languages
(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)

;;; Terminal
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/bin/bash"))

;;; AI Support
(use-package gptel
  :ensure t
  :config
  ;; Keybindings
  (global-set-key (kbd "C-c RET") #'gptel-send)
  (global-set-key (kbd "C-c C-<return>") #'gptel-menu))

;;; ===================================================================
;;; TEMPLE ORACLE SYSTEM (Inlined from temple.el)
;;; ===================================================================

;; Set the temple directory
(setq org-directory "~/Documents/Notes/org")
(setq temple-directory "~/Documents/Notes/org/Temple")

;;; Temple Core Variables
(defvar temple-prolog-file (expand-file-name "~/Documents/Notes/org/Temple/facts.pl"))
(defvar temple-prolog-binary "swipl"
  "Path to SWI-Prolog executable.")
(defvar temple-base-pl (expand-file-name "~/Documents/Notes/org/Temple/kb/base.pl")
  "Path to the main Prolog file that loads meanings.pl.")

;;; Temple Core Functions
(defun temple ()
  "Open the temple."
  (interactive)
  (find-file "~/Documents/Notes/org/Temple/temple.org"))

(defun temple-run-query (query)
  "Run a Prolog QUERY in temple-base-pl and return raw output as string."
  (let ((cmd (format "%s -q -s %s -g \"%s, writeln(Result), halt.\""
                     temple-prolog-binary
                     temple-base-pl
                     query)))
    (with-temp-buffer
      (shell-command cmd (current-buffer))
      (string-trim (buffer-string)))))

(defun temple-query-int-meaning-list (query)
  "Run QUERY returning a list of Number-Meaning pairs and parse it."
  (let ((output (temple-run-query query)))
    ;; parse [1-prompt,1-creative_block,...] into (1 prompt) style
    (if (string-match "\\[\\(.*\\)\\]" output)
        (mapcar (lambda (pair)
                  (let ((parts (split-string pair "-" t)))
                    (cons (string-to-number (car parts))
                          (string-trim (cadr parts)))))
                (split-string (match-string 1 output) "," t))
      ;; fallback: return empty
      nil)))

;;; Affirmations
(defun temple-get-random-affirmation ()
  "Query Prolog for a random affirmation and return it as a string."
  (let ((cmd (format "%s -q -s %s -g \"random_affirmation(Text), write(Text), halt.\""
                     temple-prolog-binary
                     temple-base-pl)))
    (with-temp-buffer
      (shell-command cmd (current-buffer))
      (string-trim (buffer-string)))))

(defun temple-show-affirmation ()
  "Display a random affirmation in the minibuffer."
  (interactive)
  (let ((affirmation (temple-get-random-affirmation)))
    (message "🕉️  %s" affirmation)))

(defun temple-insert-daily-affirmation (&optional n)
  "Insert N random affirmations into today's Org-roam daily node.
If N is nil, insert 1 affirmation."
  (interactive "p")
  (unless n (setq n 1))
  (org-roam-dailies-find-today)
  (goto-char (point-max))
  (insert (format "\n* Temple Affirmations (%s)\n"
                  (format-time-string "%H:%M")))
  (dotimes (_ n)
    (insert (format "- %s\n" (temple-get-random-affirmation)))))

;;; Divination
(defun temple-divine-with-question (question n-meanings n-affirmations)
  "Perform a full Temple divination with QUESTION, N-MEANINGS, and N-AFFIRMATIONS."
  (interactive
   (list (read-string "What do you seek guidance on? ")
         (read-number "Number of meanings: " 3)
         (read-number "Number of affirmations: " 1)))
  (org-roam-dailies-find-today)
  (goto-char (point-max))
  (insert (format "\n* %s :temple:\n" question))
  (insert (format "<%s>\n\n" (format-time-string "%Y-%m-%d %a %H:%M")))

  ;; Insert meanings
  (insert "** Numeric Oracle\n")
  (let ((results (temple-query-int-meaning-list
                  (format "random_meanings(%d, Result)" n-meanings))))
    (dolist (pair results)
      (insert (format "- *%d*: %s\n" (car pair) (cdr pair)))))

  ;; Insert affirmations
  (insert "\n** Affirmations\n")
  (dotimes (_ n-affirmations)
    (insert (format "- %s\n" (temple-get-random-affirmation))))

  ;; Insert reflection space
  (insert "\n** Reflection\n")
  (insert "#+begin_quote\n\n#+end_quote\n")
  (forward-line -1))

;;; Analysis Functions
(defun temple-review-past-divinations ()
  "Review all past Temple divinations using org-ql."
  (interactive)
  (require 'org-ql)
  (org-ql-search (org-roam-dailies--list-files)
    '(tags "temple")
    :sort 'date
    :title "Temple Divinations"))

(defun temple-search-divinations (query-text)
  "Search Temple divinations containing QUERY-TEXT."
  (interactive "sSearch divinations for: ")
  (require 'org-ql)
  (org-ql-search (org-roam-dailies--list-files)
    `(and (tags "temple")
          (regexp ,query-text))
    :sort 'date))

;;; AI System Configuration
(defvar temple-interpretation-system-prompt
  "You are an oracle interpreter for a personal divination system. The user receives:
- Numeric spreads with symbolic meanings
- Affirmations from a curated knowledge base
- A specific question they're seeking guidance on

Your role:
1. Synthesize the numeric meanings into coherent guidance
2. Connect the affirmations to the question's emotional/practical dimensions
3. Provide actionable, grounded interpretation
4. Identify patterns and connections the user might miss
5. Be direct, honest, and specific - avoid vague mysticism

The user is technically sophisticated (OSINT/pentesting background), values clarity and signal-to-noise ratio, and is working on sobriety (number 12 is their anchor). They use this system for cognitive navigation and decision-making.

Format your response as:
- **The Signal**: Core message in 1-2 sentences
- **Number Synthesis**: How the numbers interact/progress
- **Affirmation Integration**: How affirmations connect to the spread
- **Actionable Guidance**: Specific next steps
- **Pattern Notes**: Anything unusual or significant")

;;; AI Analysis Functions
(defun temple-recent-numbers* (days)
  "AI analyzes numbers from last N DAYS for patterns."
  (interactive "nAnalyze last N days: ")
  (let ((entries nil))
    ;; Collect all temple entries from timeframe
    (org-ql-select (org-roam-dailies--list-files)
      '(tags "temple")
      :action (lambda ()
                (let* ((element (org-element-at-point))
                       (begin (org-element-property :begin element))
                       (end (org-element-property :end element))
                       (content (when (and begin end)
                                  (buffer-substring-no-properties begin end))))
                  (when content
                    (push content entries)))))

    ;; Build prompt with all data
    (let* ((prompt (format "Analyze these divination entries from the last %d days:\n\n%s\n\nWhat patterns emerge? What's the oracle saying?"
                           days
                           (mapconcat 'identity (reverse entries) "\n---\n")))
           (buf (get-buffer-create "*Temple Recent Analysis*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "** Analysis: Last %d Days\n\n" days))
        (insert "Processing...\n\n"))
      (gptel-request prompt
        :system temple-interpretation-system-prompt
        :buffer buf
        :callback (lambda (response info)
                    (when response
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (delete-region (point-min) (point-max))
                        (insert (format "** Analysis: Last %d Days\n\n" days))
                        (insert response)
                        (org-mode)
                        (display-buffer buf))))))))

;;; Temple Setup Complete Message
(with-eval-after-load 'org-roam
  (message "🕉️  Temple Oracle system loaded and ready"))

;;; Temple Keybindings
(my/leader-keys
  "t" '(:ignore t :which-key "temple")
  "tt" '(temple :which-key "open temple")
  "td" '(temple-divine-with-question :which-key "divine")
  "ta" '(temple-show-affirmation :which-key "affirmation")
  "tA" '(temple-insert-daily-affirmation :which-key "insert affirmation")
  "tr" '(temple-review-past-divinations :which-key "review divinations")
  "ts" '(temple-search-divinations :which-key "search divinations")

  ;; AI analysis functions
  "t*" '(:ignore t :which-key "temple AI")
  "t*r" '(temple-recent-numbers* :which-key "recent numbers*"))

;;; Direct Keybindings (fallback)
(global-set-key (kbd "C-c t t") #'temple)
(global-set-key (kbd "C-c t d") #'temple-divine-with-question)
(global-set-key (kbd "C-c t a") #'temple-show-affirmation)
(global-set-key (kbd "C-c t A") #'temple-insert-daily-affirmation)
(global-set-key (kbd "C-c t r") #'temple-review-past-divinations)
(global-set-key (kbd "C-c t s") #'temple-search-divinations)

;;; ===================================================================
;;; TEMPLE SETUP COMPLETE
;;; ===================================================================

(message "🕉️  Temple Oracle - Android config loaded")

;;; init.el ends here