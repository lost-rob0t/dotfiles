(setq doom-theme 'doom-outrun-electric)

(setq display-line-numbers-type t)

(setq frame-resize-pixelwise t)

(map! :leader
:desc "Push current branch to remote branch"
"g p P" #'magit-push-current-to-pushremote)

(map! :leader
:desc "Pull current branch from remote"
"g p p" #'magit-pull-from-pushremote)

(require 'magit-todos)

(setq projectile-project-search-path
'(("~/Documents/Projects" . 1)))

(setq org-directory "~/Documents/Notes/org")

(setq time-stamp-active t
time-stamp-start "#\+LAST_MODIFIED:[ \t]*"
time-stamp-end "$"
time-stamp-format "[%Y-%02m-%02d %3a %02H:%02M]")
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

(setq org-agenda-files (directory-files-recursively "~/Documents/Notes/" "\.org$"))

(defun org-agenda-update-files ()
"Update the org-agenda-files"
(interactive)
(setq org-agenda-files (directory-files-recursively "~/Documents/Notes/" "\.org$")))
(map! :leader
:desc "Update agenda"
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

(setq-default org-super-agenda-groups
  '(
    (:and (:todo "IDEA" :name "Starintel Ideas" :tag ("starintel" "sit"))
          "Ideas for new projects or improvements at Starintel")
    (:and (:todo "TODO" :name "Starintel Bugs" :tag ("starintel-bug" "sib"))
          "Bugs that need to be fixed at Starintel")
    (:and (:todo "TODO" :name "Personal" :tag ("mow" "trash"))
          "Personal tasks and errands")
    (:and (:todo "TODO" :name "Read inbox" :tag ("book" "artical" "books"))
          "Items to read in the inbox, such as books or articles")))



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
