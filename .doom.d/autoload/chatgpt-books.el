;;; chatgpt-books.el --- Doom entrypoint for chatgpt-books -*- lexical-binding: t; -*-

(require 'seq)

(defgroup +chatgpt-books nil
  "Integration for the local chatgpt-books checkout."
  :group 'org)

(defcustom +chatgpt-books-root nil
  "Explicit path to the local chatgpt-books checkout.

When nil, `+chatgpt-books--find-root' tries `CHATGPT_BOOKS_ROOT', the
current directory hierarchy, and conventional project locations under HOME."
  :type '(choice (const :tag "Auto-detect" nil) directory)
  :group '+chatgpt-books)

(defun +chatgpt-books--checkout-p (root)
  "Return non-nil when ROOT looks like the chatgpt-books checkout."
  (and root
       (file-directory-p root)
       (file-readable-p (expand-file-name "AGENTS.md" root))
       (file-readable-p (expand-file-name "README.org" root))
       (file-readable-p (expand-file-name "elisp/chatgpt-books.el" root))))

(defun +chatgpt-books--candidate-roots ()
  "Return candidate chatgpt-books checkout roots in priority order."
  (delete-dups
   (delq nil
         (list +chatgpt-books-root
               (getenv "CHATGPT_BOOKS_ROOT")
               (locate-dominating-file default-directory "AGENTS.md")
               (expand-file-name "~/Documents/Projects/chatgpt-books")
               (expand-file-name "~/Documents/chatgpt-books")
               (expand-file-name "~/Projects/chatgpt-books")
               (expand-file-name "~/src/chatgpt-books")
               (expand-file-name "~/chatgpt-books")))))

(defun +chatgpt-books--find-root ()
  "Return the first valid local chatgpt-books checkout root."
  (seq-find #'+chatgpt-books--checkout-p
            (+chatgpt-books--candidate-roots)))

;;;###autoload
(defun +chatgpt-books/load ()
  "Load the project-local chatgpt-books Emacs package.

Dotfiles owns only checkout discovery and package loading.  The books
repository owns its Org-roam database, capture templates, export behavior,
and project-specific settings."
  (interactive)
  (let ((root (+chatgpt-books--find-root)))
    (unless root
      (user-error
       (concat "Cannot find chatgpt-books checkout; set CHATGPT_BOOKS_ROOT "
               "or customize +chatgpt-books-root")))
    (add-to-list 'load-path (expand-file-name "elisp" root))
    (require 'chatgpt-books)
    root))

;;;###autoload
(defun +chatgpt-books/open ()
  "Load chatgpt-books and open its project landing page."
  (interactive)
  (+chatgpt-books/load)
  (call-interactively #'chatgpt-books))

(provide '+chatgpt-books-autoloads)
;;; chatgpt-books.el ends here
