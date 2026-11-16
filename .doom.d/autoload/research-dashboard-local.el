;;; research-dashboard-local.el --- Local checkout support for research dashboard -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'research-dashboard)

(defcustom nsa/research-dashboard-local-checkouts nil
  "Local Git checkouts or directories to scan for research Org files.

Git checkouts infer repository identity from `remote.origin.url'.  A local
item shadows the same repository/path discovered from GitHub, so review and
approval operate on the checkout copy instead of stale remote content."
  :type '(repeat directory)
  :group 'nsa/research-dashboard)

(defvar nsa/research-dashboard-local--installed nil)
(defvar-local nsa/research-dashboard-local--files nil)

(defun nsa/research-dashboard-local--id (item)
  (concat (nsa/research-item-repo item) ":" (nsa/research-item-path item)))

(defun nsa/research-dashboard-local--git (directory &rest args)
  (when (executable-find "git")
    (with-temp-buffer
      (when (zerop (apply #'process-file "git" nil t nil
                          "-C" directory args))
        (string-trim (buffer-string))))))

(defun nsa/research-dashboard-local--git-root (directory)
  (let ((root (nsa/research-dashboard-local--git
               directory "rev-parse" "--show-toplevel")))
    (and root (file-directory-p root) (file-truename root))))

(defun nsa/research-dashboard-local--github-slug (remote)
  (when (and remote
             (string-match "github\\.com[:/]\\(.+\\)\\'" remote))
    (string-remove-suffix ".git" (match-string 1 remote))))

(defun nsa/research-dashboard-local--checkout-info (directory)
  (let* ((scan-root (file-truename directory))
         (repo-root (or (nsa/research-dashboard-local--git-root scan-root)
                        scan-root))
         (remote (nsa/research-dashboard-local--git
                  repo-root "config" "--get" "remote.origin.url"))
         (slug (nsa/research-dashboard-local--github-slug remote))
         (branch (or (nsa/research-dashboard-local--git
                      repo-root "symbolic-ref" "--quiet" "--short" "HEAD")
                     "local"))
         (commit (or (nsa/research-dashboard-local--git
                      repo-root "rev-parse" "HEAD")
                     "LOCAL")))
    (list :scan-root scan-root
          :repo-root repo-root
          :repo (or slug (concat "local:" (file-name-nondirectory repo-root)))
          :branch branch
          :commit commit)))

(defun nsa/research-dashboard-local--read (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nsa/research-dashboard-local--blob (file repo-root)
  (or (nsa/research-dashboard-local--git repo-root "hash-object" file)
      (concat "local:"
              (secure-hash 'sha256
                           (nsa/research-dashboard-local--read file)))))

(defun nsa/research-dashboard-local--eligible-file-p (file)
  (and (string-suffix-p ".org" file t)
       (not (member (downcase (file-name-nondirectory file))
                    nsa/research-dashboard--auxiliary-files))))

(defun nsa/research-dashboard-local--item (info file)
  (when (nsa/research-dashboard-local--eligible-file-p file)
    (let* ((content (nsa/research-dashboard-local--read file))
           (title (or (nsa/research-dashboard--keyword content "title")
                      (file-name-base file)))
           (lifecycle (or (nsa/research-dashboard--keyword content "status")
                          "MISSING"))
           (approval
            (upcase (or (nsa/research-dashboard--keyword content "approval_state")
                        "LEGACY"))))
      (when (or (string= approval "PENDING")
                (and (string= approval "LEGACY")
                     (nsa/research-dashboard--legacy-reviewable-p lifecycle)))
        (let* ((repo-root (plist-get info :repo-root))
               (path (file-relative-name file repo-root)))
          (nsa/research-item-create
           :repo (plist-get info :repo)
           :branch (plist-get info :branch)
           :path path
           :blob (nsa/research-dashboard-local--blob file repo-root)
           :title title
           :lifecycle lifecycle
           :approval approval
           :content content))))))

(defun nsa/research-dashboard-local--register (item file)
  (let ((id (nsa/research-dashboard-local--id item)))
    (puthash id file nsa/research-dashboard-local--files)
    (when (hash-table-p nsa/research-dashboard--seen)
      (puthash id t nsa/research-dashboard--seen))
    (setq nsa/research-dashboard--items
          (cons item
                (seq-remove
                 (lambda (existing)
                   (string= id (nsa/research-dashboard-local--id existing)))
                 nsa/research-dashboard--items)))))

(defun nsa/research-dashboard-local--scan-directory (directory)
  (condition-case error-data
      (let* ((info (nsa/research-dashboard-local--checkout-info directory))
             (scan-root (plist-get info :scan-root)))
        (dolist (file (directory-files-recursively scan-root "\\.org\\'"))
          (let ((item (nsa/research-dashboard-local--item info file)))
            (when item
              (nsa/research-dashboard-local--register item file)))))
    (error
     (push (format "Local research %s: %s"
                   directory (error-message-string error-data))
           nsa/research-dashboard--errors))))

(defun nsa/research-dashboard-local--scan-current-buffer ()
  (unless (hash-table-p nsa/research-dashboard-local--files)
    (setq nsa/research-dashboard-local--files (make-hash-table :test #'equal)))
  (clrhash nsa/research-dashboard-local--files)
  (dolist (directory nsa/research-dashboard-local-checkouts)
    (let ((expanded (expand-file-name directory)))
      (if (file-directory-p expanded)
          (nsa/research-dashboard-local--scan-directory expanded)
        (push (format "Local research directory missing: %s" expanded)
              nsa/research-dashboard--errors))))
  (nsa/research-dashboard--schedule-render))

(defun nsa/research-dashboard-local--file-for-item (item)
  (and (hash-table-p nsa/research-dashboard-local--files)
       (gethash (nsa/research-dashboard-local--id item)
                nsa/research-dashboard-local--files)))

(defun nsa/research-dashboard-local--refresh-advice (original &rest args)
  (prog1 (apply original args)
    (when (derived-mode-p 'nsa/research-dashboard-mode)
      (nsa/research-dashboard-local--scan-current-buffer))))

(defun nsa/research-dashboard-local--job-done-advice
    (original generation &optional item error-text)
  (if (and item (nsa/research-dashboard-local--file-for-item item))
      (funcall original generation nil error-text)
    (funcall original generation item error-text)))

(defun nsa/research-dashboard-local--render-advice (original &rest args)
  (prog1 (apply original args)
    (when (and (derived-mode-p 'nsa/research-dashboard-mode)
               nsa/research-dashboard-local-checkouts)
      (setq header-line-format
            (concat header-line-format
                    (format "   local:%d [d add dir] [D remove dir]"
                            (hash-table-count
                             nsa/research-dashboard-local--files)))))))

(defun nsa/research-dashboard-local--view-advice (original &rest args)
  (let* ((item (nsa/research-dashboard--at-point))
         (file (nsa/research-dashboard-local--file-for-item item)))
    (if (not file)
        (apply original args)
      (unless (file-exists-p file)
        (user-error "Local research file disappeared: %s" file))
      (pop-to-buffer (find-file-noselect file)))))

(defun nsa/research-dashboard-local--checkout-metadata (file)
  (let* ((directory (file-name-directory file))
         (repo-root (or (nsa/research-dashboard-local--git-root directory)
                        directory))
         (commit (or (nsa/research-dashboard-local--git
                      repo-root "rev-parse" "HEAD")
                     "LOCAL")))
    (cons commit (nsa/research-dashboard-local--blob file repo-root))))

(defun nsa/research-dashboard-local--assert-writable (file expected)
  (let ((visiting (get-file-buffer file)))
    (when (and visiting
               (with-current-buffer visiting (buffer-modified-p)))
      (user-error "Local research buffer has unsaved changes: %s" file)))
  (unless (string= expected (nsa/research-dashboard-local--read file))
    (user-error "Research changed since refresh; refresh first")))

(defun nsa/research-dashboard-local--write (file expected updated)
  (nsa/research-dashboard-local--assert-writable file expected)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region updated nil file nil 'silent))
  (let ((visiting (get-file-buffer file)))
    (when (and visiting (not (with-current-buffer visiting (buffer-modified-p))))
      (with-current-buffer visiting
        (revert-buffer :ignore-auto :noconfirm)))))

(defun nsa/research-dashboard-local--preview (item file state updated)
  (let ((buffer (get-buffer-create "*Research Approval Preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n%s\n\n"
                        (nsa/research-item-repo item)
                        (nsa/research-item-path item)
                        file))
        (dolist (field nsa/research-dashboard--fields)
          (insert (format "#+%s: %s\n" field
                          (nsa/research-dashboard--keyword updated field))))
        (insert "\nDelivery: direct local checkout write; Git is left dirty for review/commit.\n")
        (special-mode)))
    (display-buffer buffer)
    (yes-or-no-p
     (format "%s local %s? " state (file-name-nondirectory file)))))

(defun nsa/research-dashboard-local--decide (state item file)
  (when (nsa/research-item-busy item)
    (user-error "Decision already running"))
  (let* ((actor (read-string "Human decision-maker: "
                             (or nsa/research-dashboard--login user-login-name)))
         (evidence (read-string "Durable approval evidence: "
                                "human:emacs-research-dashboard-local")))
    (when (or (string-empty-p (string-trim actor))
              (string-empty-p (string-trim evidence)))
      (user-error "Actor and evidence must be nonempty"))
    (setf (nsa/research-item-busy item) t)
    (nsa/research-dashboard--render)
    (condition-case error-data
        (let* ((content (nsa/research-dashboard-local--read file))
               (metadata (nsa/research-dashboard-local--checkout-metadata file))
               (commit (car metadata))
               (blob (cdr metadata))
               (updated
                (nsa/research-dashboard--decision-content
                 content state actor evidence commit blob
                 (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))))
          (unless (string= content (nsa/research-item-content item))
            (user-error "Research changed since refresh; refresh first"))
          (if (not (nsa/research-dashboard-local--preview
                    item file state updated))
              (progn
                (setf (nsa/research-item-busy item) nil)
                (nsa/research-dashboard--render))
            (nsa/research-dashboard-local--write file content updated)
            (message "%s %s — updated local checkout; Git left dirty"
                     state file)
            (nsa/research-dashboard-refresh)))
      (error
       (setf (nsa/research-item-busy item) nil)
       (nsa/research-dashboard--render)
       (signal (car error-data) (cdr error-data))))))

(defun nsa/research-dashboard-local--decide-advice (original state)
  (let* ((item (nsa/research-dashboard--at-point))
         (file (nsa/research-dashboard-local--file-for-item item)))
    (if file
        (nsa/research-dashboard-local--decide state item file)
      (funcall original state))))

;;;###autoload
(defun nsa/research-dashboard-local-add-directory (directory)
  "Add DIRECTORY to the research dashboard's local sources and refresh."
  (interactive (list (read-directory-name "Add research checkout/directory: ")))
  (let ((directory (file-truename (expand-file-name directory))))
    (unless (file-directory-p directory)
      (user-error "Not a directory: %s" directory))
    (cl-pushnew directory nsa/research-dashboard-local-checkouts :test #'string=)
    (when (derived-mode-p 'nsa/research-dashboard-mode)
      (nsa/research-dashboard-refresh))
    (message "Research local source added: %s" directory)))

;;;###autoload
(defun nsa/research-dashboard-local-remove-directory (directory)
  "Remove DIRECTORY from the research dashboard's local sources and refresh."
  (interactive
   (list
    (completing-read "Remove research checkout/directory: "
                     nsa/research-dashboard-local-checkouts nil t)))
  (setq nsa/research-dashboard-local-checkouts
        (delete directory nsa/research-dashboard-local-checkouts))
  (when (derived-mode-p 'nsa/research-dashboard-mode)
    (nsa/research-dashboard-refresh))
  (message "Research local source removed: %s" directory))

(defun nsa/research-dashboard-local--install ()
  (unless nsa/research-dashboard-local--installed
    (advice-add 'nsa/research-dashboard-refresh :around
                #'nsa/research-dashboard-local--refresh-advice)
    (advice-add 'nsa/research-dashboard--job-done :around
                #'nsa/research-dashboard-local--job-done-advice)
    (advice-add 'nsa/research-dashboard--render :around
                #'nsa/research-dashboard-local--render-advice)
    (advice-add 'nsa/research-dashboard-view :around
                #'nsa/research-dashboard-local--view-advice)
    (advice-add 'nsa/research-dashboard--decide :around
                #'nsa/research-dashboard-local--decide-advice)
    (define-key nsa/research-dashboard-mode-map (kbd "d")
                #'nsa/research-dashboard-local-add-directory)
    (define-key nsa/research-dashboard-mode-map (kbd "D")
                #'nsa/research-dashboard-local-remove-directory)
    (setq nsa/research-dashboard-local--installed t)))

;;;###autoload
(defun nsa/research-dashboard-local-setup ()
  "Install local checkout support in the current research dashboard buffer."
  (nsa/research-dashboard-local--install)
  (setq-local nsa/research-dashboard-local--files
              (make-hash-table :test #'equal)))

;;;###autoload
(add-hook 'nsa/research-dashboard-mode-hook #'nsa/research-dashboard-local-setup)

(provide 'research-dashboard-local)
;;; research-dashboard-local.el ends here
