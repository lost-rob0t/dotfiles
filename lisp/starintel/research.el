;;; research.el --- Starintel research control surface -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dired)
(require 'org)
(require 'org-id)
(require 'subr-x)

(defgroup star/research nil
  "Emacs control surface around the Starintel research project."
  :group 'tools)

(defcustom star/research-project-root
  (expand-file-name "~/Documents/Projects/starintelV4/")
  "Starintel project root."
  :type 'directory)

(defcustom star/research-roam-directory
  (expand-file-name "roam/" star/research-project-root)
  "Project-local Org-roam directory."
  :type 'directory)

(defcustom star/research-skill-directory
  (expand-file-name "skills/" star/research-project-root)
  "Directory containing one subdirectory per operational skill."
  :type 'directory)

(defcustom star/research-skill-max-lines 80
  "Maximum preferred length of a SKILL.md file."
  :type 'integer)

(defconst star/research-intel-operations
  '("orient" "collect" "normalize" "resolve" "correlate"
    "analyze" "verify" "report")
  "Core intelligence operations allowed in minimal skill files.")

(defun star/research--require (feature)
  (unless (require feature nil t)
    (user-error "Required Emacs package is unavailable: %s" feature)))

(defun star/research--directory (directory)
  (unless (file-directory-p directory)
    (user-error "Directory does not exist: %s" directory))
  directory)

(defun star/research--roam-db-location ()
  (expand-file-name ".org-roam.db" star/research-roam-directory))

(defmacro star/research--with-roam (&rest body)
  `(let ((org-roam-directory (file-truename
                              (star/research--directory
                               star/research-roam-directory)))
         (org-roam-db-location (star/research--roam-db-location)))
     ,@body))

(defun star/research-open-project ()
  "Open the Starintel project root."
  (interactive)
  (dired (star/research--directory star/research-project-root)))

(defun star/research-open-roam ()
  "Open the project Org-roam directory."
  (interactive)
  (dired (star/research--directory star/research-roam-directory)))

(defun star/research-open-skills ()
  "Open the operational skill directory."
  (interactive)
  (dired (star/research--directory star/research-skill-directory)))

(defun star/research-roam-find ()
  "Find a node only in the Starintel project roam."
  (interactive)
  (star/research--require 'org-roam)
  (star/research--with-roam
   (call-interactively #'org-roam-node-find)))

(defun star/research-roam-insert ()
  "Insert a node link from the Starintel project roam."
  (interactive)
  (star/research--require 'org-roam)
  (star/research--with-roam
   (call-interactively #'org-roam-node-insert)))

(defun star/research-roam-sync ()
  "Synchronize the project-local Org-roam database."
  (interactive)
  (star/research--require 'org-roam)
  (star/research--with-roam
   (org-roam-db-sync))
  (message "Starintel Org-roam database synchronized"))

(defun star/research-org-ql (query)
  "Run QUERY against all Org files in the Starintel roam tree."
  (interactive
   (list (read (read-string "Org QL query: " "(todo)"))))
  (star/research--require 'org-ql)
  (org-ql-search
   (directory-files-recursively
    (star/research--directory star/research-roam-directory)
    "\\.org\\'")
   query
   :title "Starintel research"))

(defun star/research-todos ()
  "Show every active TODO in the Starintel roam tree."
  (interactive)
  (star/research-org-ql '(todo)))

(defun star/research--current-org-file ()
  (unless (and (derived-mode-p 'org-mode) buffer-file-name)
    (user-error "Current buffer is not a saved Org file"))
  buffer-file-name)

(defun star/research--asset-directory ()
  (let* ((file (star/research--current-org-file))
         (slug (file-name-base file)))
    (expand-file-name
     (concat "assets/" slug "/")
     (file-name-directory file))))

(defun star/research-open-assets ()
  "Create and open the asset directory for the current Org file."
  (interactive)
  (let ((directory (star/research--asset-directory)))
    (make-directory directory t)
    (dired directory)))

(defun star/research-link-path (path label)
  "Insert an Org link to PATH using LABEL."
  (interactive
   (let ((path (read-file-name "Path: " star/research-project-root nil t)))
     (list path
           (read-string "Label: " (file-name-nondirectory
                                    (directory-file-name path))))))
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not in Org mode"))
  (insert (org-link-make-string
           (concat "file:" (expand-file-name path))
           label)))

(defun star/research-copy-to-assets (path)
  "Copy PATH into the current Org file's asset directory and link it."
  (interactive (list (read-file-name "Copy to assets: " nil nil t)))
  (let* ((assets (star/research--asset-directory))
         (source (expand-file-name path))
         (target (expand-file-name
                  (file-name-nondirectory (directory-file-name source))
                  assets)))
    (make-directory assets t)
    (if (file-directory-p source)
        (copy-directory source target nil nil t)
      (copy-file source target t))
    (star/research-link-path target (file-name-nondirectory target))))

(defun star/research--skill-directories ()
  (let ((root (star/research--directory star/research-skill-directory)))
    (cl-remove-if-not
     (lambda (directory)
       (file-exists-p (expand-file-name "SKILL.md" directory)))
     (directory-files root t "^[^.].*" t))))

(defun star/research--skill-alist ()
  (mapcar
   (lambda (directory)
     (cons (file-name-nondirectory (directory-file-name directory))
           directory))
   (star/research--skill-directories)))

(defun star/research--read-skill ()
  (let* ((skills (star/research--skill-alist))
         (name (completing-read "Skill: " skills nil t)))
    (or (cdr (assoc name skills))
        (user-error "Unknown skill: %s" name))))

(defun star/research-skill-open ()
  "Open a minimal operational SKILL.md file."
  (interactive)
  (find-file (expand-file-name "SKILL.md" (star/research--read-skill))))

(defun star/research--slug (text)
  (let ((slug (downcase (string-trim text))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (string-trim slug "-" "-")))

(defun star/research-skill-new (name operation)
  "Create a minimal NAME skill for core intel OPERATION."
  (interactive
   (list (read-string "Skill name: ")
         (completing-read "Intel operation: "
                          star/research-intel-operations nil t)))
  (let* ((slug (star/research--slug name))
         (directory (expand-file-name
                     (file-name-as-directory slug)
                     star/research-skill-directory))
         (file (expand-file-name "SKILL.md" directory)))
    (when (string-empty-p slug)
      (user-error "Skill name produces an empty slug"))
    (when (file-exists-p file)
      (user-error "Skill already exists: %s" slug))
    (make-directory directory t)
    (with-temp-file file
      (insert "---\n"
              "name: " slug "\n"
              "operation: " operation "\n"
              "---\n\n"
              "# Goal\n\n"
              "# Input\n\n"
              "# Output\n\n"
              "# Rules\n\n"
              "Prefer executable files beside this document over added prose.\n"))
    (find-file file)))

(defun star/research--skill-operation (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^operation:[[:space:]]*\\(.+\\)$" nil t)
      (string-trim (match-string 1)))))

(defun star/research-skill-validate (&optional directory)
  "Validate one minimal operational skill DIRECTORY."
  (interactive)
  (let* ((directory (or directory (star/research--read-skill)))
         (file (expand-file-name "SKILL.md" directory))
         (operation (star/research--skill-operation file))
         errors)
    (with-temp-buffer
      (insert-file-contents file)
      (when (> (line-number-at-pos (point-max)) star/research-skill-max-lines)
        (push (format "exceeds %d lines" star/research-skill-max-lines) errors))
      (dolist (heading '("# Goal" "# Input" "# Output" "# Rules"))
        (goto-char (point-min))
        (unless (re-search-forward
                 (concat "^" (regexp-quote heading) "$") nil t)
          (push (format "missing %s" heading) errors))))
    (unless (member operation star/research-intel-operations)
      (push (format "invalid operation: %s" operation) errors))
    (if errors
        (user-error "%s: %s"
                    (file-name-nondirectory
                     (directory-file-name directory))
                    (string-join (nreverse errors) "; "))
      (message "Skill valid: %s" file))))

(defun star/research-skills-validate-all ()
  "Validate every operational skill."
  (interactive)
  (let (failures)
    (dolist (directory (star/research--skill-directories))
      (condition-case error
          (star/research-skill-validate directory)
        (error (push (error-message-string error) failures))))
    (if failures
        (user-error "%s" (string-join (nreverse failures) "\n"))
      (message "All Starintel skills are minimal and valid"))))

(defun star/research-skill-bootstrap-elisp (command-name)
  "Create an adjacent Elisp implementation for a selected skill."
  (interactive (list (read-string "Command suffix: ")))
  (let* ((directory (star/research--read-skill))
         (skill (file-name-nondirectory (directory-file-name directory)))
         (suffix (star/research--slug command-name))
         (command (intern (format "star/skill-%s-%s" skill suffix)))
         (file (expand-file-name (concat suffix ".el") directory)))
    (when (string-empty-p suffix)
      (user-error "Command suffix produces an empty slug"))
    (when (file-exists-p file)
      (user-error "Script already exists: %s" file))
    (with-temp-file file
      (insert ";;; " suffix ".el --- " skill " operation -*- lexical-binding: t; -*-\n\n"
              "(defun " (symbol-name command) " ()\n"
              "  (interactive)\n"
              "  (user-error \"Not implemented\"))\n\n"
              "(provide '" skill "-" suffix ")\n"))
    (find-file file)))

(defun star/research-skill-load-elisp ()
  "Load an Elisp implementation adjacent to a selected skill."
  (interactive)
  (let* ((directory (star/research--read-skill))
         (file (read-file-name "Elisp implementation: " directory nil t nil
                               (lambda (path)
                                 (or (file-directory-p path)
                                     (string-suffix-p ".el" path))))))
    (load-file file)
    (message "Loaded %s" file)))

(defun star/research-magit-status ()
  "Open Magit at the Starintel project root."
  (interactive)
  (star/research--require 'magit)
  (magit-status (star/research--directory star/research-project-root)))

(require 'transient)

(transient-define-prefix star/research-dispatch ()
  "Starintel research controls."
  [["Org-roam"
    ("f" "Find node" star/research-roam-find)
    ("i" "Insert node" star/research-roam-insert)
    ("s" "Sync database" star/research-roam-sync)
    ("q" "Org QL" star/research-org-ql)
    ("t" "TODOs" star/research-todos)]
   ["Context"
    ("l" "Link path" star/research-link-path)
    ("a" "Open assets" star/research-open-assets)
    ("c" "Copy to assets" star/research-copy-to-assets)]
   ["Skills"
    ("k" "Open skill" star/research-skill-open)
    ("n" "New minimal skill" star/research-skill-new)
    ("v" "Validate skill" star/research-skill-validate)
    ("V" "Validate all" star/research-skills-validate-all)
    ("b" "Bootstrap Elisp" star/research-skill-bootstrap-elisp)
    ("L" "Load Elisp" star/research-skill-load-elisp)]
   ["Project"
    ("p" "Open project" star/research-open-project)
    ("r" "Open roam" star/research-open-roam)
    ("S" "Open skills" star/research-open-skills)
    ("g" "Magit status" star/research-magit-status)]])

(provide 'starintel-research)
;;; research.el ends here
