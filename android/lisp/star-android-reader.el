;;; star-android-reader.el --- Touch-first StarIntel Org reader -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Android UI for reviewing Auto Research Org files.  Heavy systems stay
;; lazy: Org-roam and gptel are configured only when their toolbar actions are
;; used.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'tool-bar)

(defgroup star/android nil
  "Android research-reader configuration."
  :group 'applications)

(defconst star/android-library-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defcustom star/android-config-root
  (file-name-directory
   (directory-file-name star/android-library-directory))
  "Root of the Android Emacs configuration."
  :type 'directory
  :group 'star/android)

(defcustom star/android-termux-home
  (if (file-directory-p "/data/data/com.termux/files/home/")
      "/data/data/com.termux/files/home/"
    (file-name-as-directory (expand-file-name "~")))
  "Termux home shared with native Android Emacs."
  :type 'directory
  :group 'star/android)

(defcustom star/android-dotfiles-root
  (file-name-as-directory
   (or (getenv "STAR_DOTFILES_ROOT")
       (expand-file-name ".dotfiles" star/android-termux-home)))
  "Dotfiles checkout containing the shared gptel configuration."
  :type 'directory
  :group 'star/android)

(defcustom star/android-research-root
  (file-name-as-directory
   (or (getenv "STAR_AUTO_RESEARCH_ROOT")
       (expand-file-name "src/auto-research" star/android-termux-home)))
  "Directory containing Auto Research repositories."
  :type 'directory
  :group 'star/android)

(defcustom star/android-primary-repository "starintel-auto-research"
  "Repository whose second-brain implementation owns Org-roam configuration."
  :type 'string
  :group 'star/android)

(defcustom star/android-reviewed-statuses
  '("APPROVED" "IMPLEMENTED" "REJECTED" "ARCHIVED")
  "Document statuses that should not appear in the unreviewed queue."
  :type '(repeat string)
  :group 'star/android)

(defconst star/android-dashboard-buffer "*Star Research*")

(defun star/android-primary-root ()
  "Return the primary Auto Research repository directory."
  (expand-file-name star/android-primary-repository
                    star/android-research-root))

(defun star/android-sync-program ()
  "Return the repository synchronization script."
  (expand-file-name "bin/sync-auto-research" star/android-config-root))

(defun star/android--keyword (keyword)
  "Return Org file KEYWORD value from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^#\\+%s:[ \t]*\\(.+\\)$" (regexp-quote keyword))
           nil t)
      (string-trim (match-string-no-properties 1)))))

(defun star/android--file-mtime (file)
  "Return FILE modification time as a float."
  (let ((attributes (file-attributes file)))
    (if attributes
        (float-time (file-attribute-modification-time attributes))
      0.0)))

(defun star/android--date-timestamp (date fallback)
  "Parse Org DATE to a timestamp, returning FALLBACK on failure."
  (or (and date
           (ignore-errors
             (float-time (org-time-string-to-time date))))
      fallback))

(defun star/android-document-metadata (file)
  "Read lightweight dashboard metadata from Org FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 32768)
    (let* ((title (or (star/android--keyword "title")
                      (file-name-base file)))
           (status (upcase (or (star/android--keyword "status") "UNKNOWN")))
           (date (or (star/android--keyword "last_modified")
                     (star/android--keyword "updated")
                     (star/android--keyword "created")))
           (mtime (star/android--file-mtime file)))
      (list :file file
            :title title
            :status status
            :date date
            :timestamp (star/android--date-timestamp date mtime)))))

(defun star/android-repositories ()
  "Return cloned Auto Research repository directories."
  (when (file-directory-p star/android-research-root)
    (cl-remove-if-not
     (lambda (directory)
       (and (file-directory-p (expand-file-name ".git" directory))
            (file-directory-p (expand-file-name "roam" directory))))
     (directory-files star/android-research-root t "^[^.].*" t))))

(defun star/android-documents ()
  "Return metadata for Org documents across Auto Research repositories."
  (cl-loop for repository in (star/android-repositories)
           for roam = (expand-file-name "roam" repository)
           append
           (mapcar #'star/android-document-metadata
                   (directory-files-recursively roam "\\.org\\'"))))

(defun star/android-newest-first (documents)
  "Return DOCUMENTS sorted newest first without mutating the caller's list."
  (sort (copy-sequence documents)
        (lambda (left right)
          (> (plist-get left :timestamp)
             (plist-get right :timestamp)))))

(defun star/android-reviewed-p (document)
  "Return non-nil when DOCUMENT has a terminal review status."
  (member (plist-get document :status) star/android-reviewed-statuses))

(defun star/android-index-p (document)
  "Return non-nil when DOCUMENT is under a repository indexes tree."
  (string-match-p "/roam/indexes/" (plist-get document :file)))

(defun star/android-open-document (button)
  "Open the Org file associated with BUTTON."
  (find-file (button-get button 'star-file)))

(defun star/android--insert-document (document)
  "Insert one dashboard row for DOCUMENT."
  (insert-text-button
   (plist-get document :title)
   'follow-link t
   'star-file (plist-get document :file)
   'action #'star/android-open-document)
  (insert (format "\n    %-12s %s\n\n"
                  (plist-get document :status)
                  (or (plist-get document :date) ""))))

(defun star/android-dashboard ()
  "Open the touch-friendly research review dashboard."
  (interactive)
  (let* ((documents (star/android-documents))
         (unreviewed
          (star/android-newest-first
           (cl-remove-if #'star/android-reviewed-p documents)))
         (indexes
          (star/android-newest-first
           (cl-remove-if-not #'star/android-index-p documents)))
         (buffer (get-buffer-create star/android-dashboard-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "STAR RESEARCH\n" 'face '(:height 1.7 :weight bold)))
        (insert (format "%d documents waiting for review\n\n" (length unreviewed)))
        (unless documents
          (insert "No research repositories found. Tap Sync.\n\n"))
        (insert (propertize "UNREVIEWED — NEWEST FIRST\n\n"
                            'face '(:weight bold)))
        (dolist (document unreviewed)
          (star/android--insert-document document))
        (insert (propertize "\nINDEXES — NEWEST FIRST\n\n"
                            'face '(:weight bold)))
        (dolist (document indexes)
          (star/android--insert-document document))
        (goto-char (point-min))
        (special-mode)
        (visual-line-mode 1)))
    (pop-to-buffer buffer)
    buffer))

(defun star/android-configure-roam ()
  "Configure Org-roam through starintel-auto-research's second-brain code."
  (interactive)
  (require 'org-roam)
  (let* ((root (star/android-primary-root))
         (library (expand-file-name "lisp/starintel" root))
         (second-brain (expand-file-name "second-brain.el" library)))
    (unless (file-exists-p second-brain)
      (user-error "Missing %s; sync research repositories first" second-brain))
    (add-to-list 'load-path library)
    (require 'second-brain)
    (starintel-second-brain-configure root t)))

(defun star/android-roam ()
  "Find a node in the canonical Auto Research Org-roam graph."
  (interactive)
  (star/android-configure-roam)
  (call-interactively #'org-roam-node-find))

(defun star/android-configure-gptel ()
  "Load the lightweight subset of the desktop gptel configuration."
  (interactive)
  (require 'gptel)
  (let ((llm-directory (expand-file-name "lisp/llm" star/android-dotfiles-root)))
    (unless (file-directory-p llm-directory)
      (user-error "Missing dotfiles LLM directory: %s" llm-directory))
    (add-to-list 'load-path llm-directory)
    (require 'ai)
    ;; Mirror ai-init.el's current OpenRouter default without loading its agent,
    ;; image-generation, MCP, Mara, and other desktop-only systems.
    (unless (assq 'openrouter/auto ai/llm-openrouter-models)
      (push '(openrouter/auto
              :description "OpenRouter Auto Router in pure-quality mode"
              :capabilities (reasoning media tool-use json url)
              :context-window 2000
              :request-params (:plugins [(:id "auto-router"
                                          :cost_quality_tradeoff 0)]))
            ai/llm-openrouter-models))
    (setq ai/llm-provider 'openrouter
          ai/llm-model 'openrouter/auto)
    (ai/llm-backend 'openrouter t)
    (ai/llm-apply-defaults)
    (require 'gptel-personas)
    (ai/gptel-apply-directives)
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
    (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")))

(defun star/android-gptel ()
  "Open gptel with the same backend, model, and directives as desktop Emacs."
  (interactive)
  (star/android-configure-gptel)
  (call-interactively #'gptel)
  (delete-other-windows))

(defun star/android--sync-finished (process _event)
  "Refresh reader state after synchronization PROCESS exits."
  (when (memq (process-status process) '(exit signal))
    (if (zerop (process-exit-status process))
        (progn
          (message "Auto Research synchronized")
          (when (featurep 'org-roam)
            (ignore-errors
              (star/android-configure-roam)
              (org-roam-db-sync)))
          (when (get-buffer star/android-dashboard-buffer)
            (star/android-dashboard)))
      (display-buffer (process-buffer process))
      (message "Auto Research sync failed; see *Star Sync*"))))

(defun star/android-sync ()
  "Synchronize dotfiles and all discovered Auto Research repositories."
  (interactive)
  (let ((script (star/android-sync-program)))
    (unless (file-readable-p script)
      (user-error "Missing sync script: %s" script))
    (let ((process
           (start-process "star-android-sync"
                          (get-buffer-create "*Star Sync*")
                          "bash" script)))
      (set-process-sentinel process #'star/android--sync-finished)
      process)))

(defun star/android-previous-buffer ()
  "Switch to the previous buffer."
  (interactive)
  (previous-buffer))

(defun star/android-next-buffer ()
  "Switch to the next buffer."
  (interactive)
  (next-buffer))

(defun star/android--toolbar-button (key label command help)
  "Add text toolbar KEY with LABEL invoking COMMAND and HELP text."
  (define-key-after
   tool-bar-map (vector key)
   `(menu-item ,label ,command :help ,help)))

(defun star/android-configure-touch-ui ()
  "Enable Android touch scrolling and the research toolbar."
  (setq tool-bar-position 'bottom
        tool-bar-style 'text
        touch-screen-display-keyboard t)
  (tool-bar-mode 1)
  (menu-bar-mode 1)
  (when (fboundp 'modifier-bar-mode)
    (modifier-bar-mode 1))
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (setq tool-bar-map (make-sparse-keymap))
  (star/android--toolbar-button 'dashboard "Dash" #'star/android-dashboard
                                "Research review dashboard")
  (star/android--toolbar-button 'sync "Sync" #'star/android-sync
                                "Pull dotfiles and Auto Research repositories")
  (star/android--toolbar-button 'roam "Roam" #'star/android-roam
                                "Find an Org-roam node")
  (star/android--toolbar-button 'gptel "GPT" #'star/android-gptel
                                "Open gptel")
  (star/android--toolbar-button 'previous "Prev" #'star/android-previous-buffer
                                "Previous buffer")
  (star/android--toolbar-button 'next "Next" #'star/android-next-buffer
                                "Next buffer"))

(defun star/android-org-reader-mode ()
  "Apply read-first defaults to the current Org buffer."
  (visual-line-mode 1)
  (org-indent-mode 1)
  (setq-local line-spacing 0.15)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun star/android-initialize ()
  "Initialize the minimal Android research reader."
  (setq org-startup-folded 'content
        org-startup-indented t
        org-startup-with-inline-images t
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-confirm-babel-evaluate t)
  (add-hook 'org-mode-hook #'star/android-org-reader-mode)
  (when (display-graphic-p)
    (set-face-attribute 'default nil :height 140))
  (star/android-configure-touch-ui)
  (add-hook 'emacs-startup-hook #'star/android-dashboard)
  (run-with-idle-timer
   3 nil
   (lambda ()
     (when (and (executable-find "bash")
                (executable-find "git")
                (executable-find "gh")
                (file-readable-p (star/android-sync-program)))
       (star/android-sync)))))

(provide 'star-android-reader)
;;; star-android-reader.el ends here
