;;; skill-lab.el --- Reviewable agent skill improvement lab -*- lexical-binding: t; -*-

(require 'diff-mode)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup skill-lab nil
  "Generate isolated skill improvement candidates."
  :group 'tools)

(defcustom skill-lab-repository nil
  "Editable canonical skills checkout.
When nil, use SKILL_LAB_REPO, OPENCODE_GLOBAL_SKILLS_CHECKOUT, then
~/Documents/AI/skills."
  :type '(choice (const :tag "Discover" nil) directory))

(defcustom skill-lab-script
  (expand-file-name "~/.dotfiles/scripts/skill-lab.py")
  "Skill lab driver."
  :type 'file)

(defcustom skill-lab-model "astra-medium"
  "Logical model passed to opencode-worker."
  :type 'string)

(defcustom skill-lab-full-validation nil
  "When non-nil, also run `nix flake check --no-build' on candidates."
  :type 'boolean)

(defvar skill-lab--process nil)
(defvar skill-lab--result nil)

(defun skill-lab--repo ()
  (file-name-as-directory
   (expand-file-name
    (or skill-lab-repository
        (getenv "SKILL_LAB_REPO")
        (getenv "OPENCODE_GLOBAL_SKILLS_CHECKOUT")
        "~/Documents/AI/skills"))))

(defun skill-lab--skills ()
  (let ((root (expand-file-name "skills" (skill-lab--repo))))
    (unless (file-directory-p root)
      (user-error "Skills directory not found: %s" root))
    (sort
     (seq-filter
      (lambda (name)
        (file-exists-p
         (expand-file-name (format "%s/SKILL.md" name) root)))
      (directory-files root nil "^[^.]" t))
     #'string<)))

(defun skill-lab--read-skill ()
  (let ((choice
         (completing-read
          "Improve skill: "
          (cons "auto" (skill-lab--skills))
          nil t nil nil "auto")))
    (if (string-empty-p choice) "auto" choice)))

(defun skill-lab--json-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-false nil)
          (json-null nil))
      (json-read))))

(defun skill-lab--value (key)
  (alist-get key skill-lab--result))

(defun skill-lab--load-latest ()
  (let ((buffer (generate-new-buffer " *skill-lab-latest*")))
    (unwind-protect
        (let ((status
               (call-process
                "python3" nil buffer nil skill-lab-script "latest")))
          (unless (zerop status)
            (user-error "No skill-lab result available"))
          (setq skill-lab--result (skill-lab--json-buffer buffer)))
      (kill-buffer buffer))))

(defun skill-lab--worktree ()
  (unless skill-lab--result
    (skill-lab--load-latest))
  (or (skill-lab--value 'worktree)
      (user-error "Latest skill-lab run has no candidate worktree")))

(defun skill-lab--insert-command (program &rest args)
  (apply #'call-process program nil t nil args))

(defun skill-lab-show-diff ()
  "Show the latest candidate, including untracked files, in `diff-mode'."
  (interactive)
  (let* ((root (file-name-as-directory (skill-lab--worktree)))
         (skill (skill-lab--value 'skill))
         (status (skill-lab--value 'status))
         (buffer (get-buffer-create "*skill-lab-diff*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# skill-lab: %s [%s]\n# %s\n\n" skill status root))
        (skill-lab--insert-command
         "git" "-C" root "diff" "--no-ext-diff" "--no-color" "--")
        (dolist
            (file
             (process-lines
              "git" "-C" root "ls-files" "--others" "--exclude-standard"))
          (skill-lab--insert-command
           "git" "-C" root "diff" "--no-index" "--no-color"
           "--" "/dev/null" file))
        (diff-mode)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun skill-lab-open-magit ()
  "Open Magit on the latest candidate worktree."
  (interactive)
  (require 'magit)
  (magit-status-setup-buffer (skill-lab--worktree)))

(defun skill-lab--sentinel (process _event)
  (when (memq (process-status process) '(exit signal))
    (setq skill-lab--process nil)
    (let ((stdout (process-buffer process))
          (stderr (process-get process 'skill-lab-stderr)))
      (condition-case error
          (progn
            (setq skill-lab--result (skill-lab--json-buffer stdout))
            (let ((status (skill-lab--value 'status)))
              (if (skill-lab--value 'worktree)
                  (progn
                    (message "Skill lab %s: %s" status (skill-lab--value 'skill))
                    (skill-lab-show-diff))
                (message "Skill lab %s: %s" status (skill-lab--value 'skill)))))
        (error
         (display-buffer stderr)
         (message "Skill lab failed: %s" (error-message-string error))))
      (when (buffer-live-p stdout)
        (kill-buffer stdout)))))

(defun skill-lab--start (skill goal)
  (when (process-live-p skill-lab--process)
    (user-error "A skill-lab run is already active"))
  (unless (file-readable-p skill-lab-script)
    (user-error "Skill lab driver not found: %s" skill-lab-script))
  (let* ((repo (skill-lab--repo))
         (stdout (generate-new-buffer " *skill-lab-result*"))
         (stderr (get-buffer-create "*skill-lab-log*"))
         (command
          (append
           (list "python3" skill-lab-script
                 "run" "--repo" repo
                 "--skill" skill
                 "--model" skill-lab-model)
           (when skill-lab-full-validation (list "--full"))
           (when (and goal (not (string-empty-p goal)))
             (list "--goal" goal)))))
    (with-current-buffer stderr
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (setq skill-lab--process
          (make-process
           :name "skill-lab"
           :buffer stdout
           :stderr stderr
           :command command
           :noquery t
           :sentinel #'skill-lab--sentinel))
    (process-put skill-lab--process 'skill-lab-stderr stderr)
    (message "Skill lab started: %s" skill)))

(defun skill-lab-improve (skill goal)
  "Generate a candidate improvement for SKILL.
With a prefix argument, prompt for an optional GOAL."
  (interactive
   (list
    (skill-lab--read-skill)
    (when current-prefix-arg
      (read-string "Improvement goal: "))))
  (skill-lab--start skill goal))

(defun skill-lab-auto-improve ()
  "Automatically choose an old skill and generate one candidate."
  (interactive)
  (skill-lab--start "auto" nil))

(defun skill-lab-reject ()
  "Delete the latest lab worktree and its `lab/' branch."
  (interactive)
  (let* ((root (skill-lab--worktree))
         (repo (skill-lab--repo)))
    (when (yes-or-no-p (format "Reject candidate and delete %s? " root))
      (let* ((log-buffer (get-buffer-create "*skill-lab-log*"))
             (status
              (call-process
               "python3" nil log-buffer t
               skill-lab-script "cleanup"
               "--repo" repo "--worktree" root)))
        (unless (zerop status)
          (user-error "Skill-lab cleanup failed; see *skill-lab-log*"))
        (setq skill-lab--result nil)
        (when-let ((buffer (get-buffer "*skill-lab-diff*")))
          (kill-buffer buffer))
        (message "Skill-lab candidate rejected")))))

(provide 'skill-lab)
;;; skill-lab.el ends here
