;;; dired-loadbearing.el --- Ask Claude if files are load-bearing via gptel  -*- lexical-binding: t; -*-

;; Requirements: gptel package
;; Install: M-x package-install RET gptel

;; Usage: In dired, move cursor to a file/directory and run:
;; M-x dired-ask-claude-loadbearing

(require 'gptel)
(require 'dired)
(require 'json)

(defvar dired-claude-archive-root "~/.archive"
  "Root directory for archiving files.")

(defvar dired-claude-git-archive "git"
  "Subdirectory for git repos (relative to archive root).")

(defvar dired-claude-history-archive "history"
  "Subdirectory for prehistoric files (relative to archive root).")

(defvar dired-claude-user-context
  "I do OSINT/bug bounty/pentesting work. I code in Common Lisp, Python, and Prolog. I use Emacs and Qtile.

CRITICAL: NEVER suggest deleting git repos or projects from 2016-2022. This is my hacker prehistory and learning period - preserve it as digital archaeology even if it looks unused."
  "Context about your work to help Claude assess files.")

(defun dired-get-file-metadata (file)
  "Get metadata for FILE to send to Claude."
  (let* ((attrs (file-attributes file))
         (size (file-attribute-size attrs))
         (atime (file-attribute-access-time attrs))
         (mtime (file-attribute-modification-time attrs))
         (days-since-access (/ (float-time (time-since atime)) 86400))
         (days-since-modified (/ (float-time (time-since mtime)) 86400))
         (is-dir (file-directory-p file)))
    (if is-dir
        (dired-get-directory-metadata file days-since-access days-since-modified)
      (list :path file
            :size size
            :is-directory nil
            :days-since-access (round days-since-access)
            :days-since-modified (round days-since-modified)))))

(defun dired-get-directory-metadata (dir days-since-access days-since-modified)
  "Get detailed metadata for directory DIR including contents analysis."
  (let* ((is-git (file-exists-p (expand-file-name ".git" dir)))
         (git-info (when is-git (dired-get-git-info dir)))
         (file-count
          (string-to-number
           (string-trim
            (shell-command-to-string
             (format "find %s -type f 2>/dev/null | wc -l"
                     (shell-quote-argument dir))))))
         (total-size
          (string-to-number
           (string-trim
            (shell-command-to-string
             (format "du -sb %s 2>/dev/null | cut -f1"
                     (shell-quote-argument dir))))))
         (recent-files
          (string-to-number
           (string-trim
            (shell-command-to-string
             (format "find %s -type f -mtime -30 2>/dev/null | wc -l"
                     (shell-quote-argument dir))))))
         (file-types (dired-get-file-types dir)))
    (list :path dir
          :size total-size
          :is-directory t
          :days-since-access (round days-since-access)
          :days-since-modified (round days-since-modified)
          :file-count file-count
          :recent-files recent-files
          :is-git-repo is-git
          :git-info git-info
          :file-types file-types)))

(defun dired-get-git-info (dir)
  "Get git repository info for DIR."
  (let* ((default-directory dir)
         (last-commit-date
          (ignore-errors
            (string-trim
             (shell-command-to-string
              "git log -1 --format=%cd --date=short 2>/dev/null"))))
         (first-commit-date
          (ignore-errors
            (string-trim
             (shell-command-to-string
              "git log --reverse --format=%cd --date=short 2>/dev/null | head -1"))))
         (days-since-commit
          (when (and last-commit-date (> (length last-commit-date) 0))
            (ignore-errors
              (/ (float-time
                  (time-subtract (current-time)
                                 (date-to-time last-commit-date)))
                 86400))))
         (branch
          (ignore-errors
            (string-trim
             (shell-command-to-string
              "git branch --show-current 2>/dev/null"))))
         (remote
          (ignore-errors
            (string-trim
             (shell-command-to-string
              "git remote get-url origin 2>/dev/null"))))
         (status-output
          (ignore-errors
            (shell-command-to-string
             "git status --porcelain 2>/dev/null")))
         (is-dirty
          (and status-output (> (length status-output) 0)))
         (is-prehistoric
          (when (and first-commit-date (> (length first-commit-date) 0))
            (ignore-errors
              (let ((year (string-to-number (substring first-commit-date 0 4))))
                (and (>= year 2016) (<= year 2022)))))))
    (list :last-commit-date (or last-commit-date "")
          :first-commit-date (or first-commit-date "")
          :days-since-commit (when days-since-commit (round days-since-commit))
          :branch (or branch "")
          :remote (or remote "")
          :is-dirty is-dirty
          :is-prehistoric is-prehistoric)))

(defun dired-get-file-types (dir)
  "Get common file types in DIR."
  (let* ((extensions
          (split-string
           (shell-command-to-string
            (format "find %s -type f 2>/dev/null | sed 's/.*\\.//' | sort | uniq -c | sort -rn | head -5"
                    (shell-quote-argument dir)))
           "\n" t))
         (cleaned
          (mapcar (lambda (line)
                    (string-trim line))
                  extensions)))
    (mapconcat #'identity cleaned ", ")))

(defun dired-build-claude-prompt (metadata)
  "Build prompt for Claude based on file METADATA."
  (let* ((path (plist-get metadata :path))
         (size (plist-get metadata :size))
         (is-dir (plist-get metadata :is-directory))
         (days-access (plist-get metadata :days-since-access))
         (days-modified (plist-get metadata :days-since-modified))
         (type (if is-dir "directory" "file")))
    (if is-dir
        (dired-build-directory-prompt metadata)
      (format "Context: %s

File to assess:
- Path: %s
- Type: file
- Size: %s bytes
- Last accessed: %d days ago
- Last modified: %d days ago

Question: Is this file load-bearing or can I delete/archive it?

Respond with ONLY a JSON object (no markdown, no code blocks):
{
  \"verdict\": \"KEEP\" or \"DELETE\" or \"QUARANTINE\",
  \"reason\": \"one sentence explanation\",
  \"confidence\": \"high\" or \"medium\" or \"low\"
}

Rules:
- KEEP: Active work, tools I use, evidence/deliverables, recent files
- QUARANTINE: Haven't touched in 6+ months, might need later, test by moving
- DELETE: Duplicates, old downloads, clear garbage, very old unused stuff
- Consider my work context when deciding
- If unsure, prefer QUARANTINE over DELETE"
              dired-claude-user-context
              path
              size
              days-access
              days-modified))))

(defun dired-build-directory-prompt (metadata)
  "Build detailed prompt for directory METADATA."
  (let* ((path (plist-get metadata :path))
         (size (plist-get metadata :size))
         (days-access (plist-get metadata :days-since-access))
         (days-modified (plist-get metadata :days-since-modified))
         (file-count (plist-get metadata :file-count))
         (recent-files (plist-get metadata :recent-files))
         (is-git (plist-get metadata :is-git-repo))
         (git-info (plist-get metadata :git-info))
         (file-types (plist-get metadata :file-types))
         (is-prehistoric (when git-info (plist-get git-info :is-prehistoric))))
    (format "Context: %s

Directory to assess:
- Path: %s
- Total size: %s bytes (%s MB)
- Contains: %d files
- Files modified in last 30 days: %d
- Last accessed: %d days ago
- Directory modified: %d days ago
- Common file types: %s
%s%s

Question: Is this directory load-bearing or can I delete/archive it?

Consider:
- For git repos: Is this a clone of something public? Can I re-clone it?
- For old projects: Any unique work or just a copy?
- For backup directories: Is this a duplicate of something else?
- Recent activity matters - if files were modified recently, it's being used

Respond with ONLY a JSON object (no markdown, no code blocks):
{
  \"verdict\": \"KEEP\" or \"DELETE\" or \"QUARANTINE\",
  \"reason\": \"one sentence explanation considering contents and historical value\",
  \"confidence\": \"high\" or \"medium\" or \"low\"
}

Rules:
- KEEP: Active projects, unique work, current tools, recent commits
- QUARANTINE: Old projects with possible unique work, inactive repos, uncertain value
- DELETE: Public repo clones (can re-clone), clear duplicates, very old unused stuff
- **ABSOLUTE RULE: NEVER DELETE anything from 2016-2022 (hacker prehistory/learning period)**
- For prehistoric repos: Always KEEP or at worst QUARANTINE - this is digital archaeology
- For git repos: if it's a public clone with no local commits and NOT prehistoric, lean toward DELETE
- If directory has recent files (last 30 days), lean toward KEEP
- If unsure, prefer QUARANTINE over DELETE"
            dired-claude-user-context
            path
            size
            (/ size 1048576.0)
            file-count
            recent-files
            days-access
            days-modified
            file-types
            (if is-git
                (let ((last-commit (plist-get git-info :last-commit-date))
                      (first-commit (plist-get git-info :first-commit-date))
                      (days-since (plist-get git-info :days-since-commit))
                      (branch (plist-get git-info :branch))
                      (remote (plist-get git-info :remote))
                      (dirty (plist-get git-info :is-dirty)))
                  (format "\nGit repository info:
- First commit: %s
- Last commit: %s (%s days ago)
- Branch: %s
- Remote: %s
- Has uncommitted changes: %s"
                          (if (> (length first-commit) 0) first-commit "unknown")
                          last-commit
                          (or days-since "unknown")
                          (if (> (length branch) 0) branch "none")
                          (if (> (length remote) 0) remote "none")
                          (if dirty "YES" "no")))
              "")
            (if is-prehistoric
                "\n**PREHISTORIC REPO (2016-2022): This is hacker learning history - PRESERVE IT**"
              ""))))

(defun dired-parse-claude-response (response-text)
  "Parse Claude's RESPONSE-TEXT into verdict."
  (condition-case err
      (let* ((cleaned (string-trim response-text))
             ;; Remove markdown code blocks if present
             (cleaned (replace-regexp-in-string "```json\n?" "" cleaned))
             (cleaned (replace-regexp-in-string "```\n?" "" cleaned))
             (cleaned (string-trim cleaned))
             (json-object-type 'alist)
             (json-array-type 'list)
             (json (json-read-from-string cleaned)))
        json)
    (error
     (message "Failed to parse Claude response: %s\nResponse was: %s" err response-text)
     nil)))

(defun dired-mark-based-on-verdict (verdict file)
  "Mark FILE in dired based on VERDICT from Claude."
  (let ((decision (alist-get 'verdict verdict))
        (reason (alist-get 'reason verdict))
        (confidence (alist-get 'confidence verdict)))
    ;; Make sure we're in a dired buffer
    (when (derived-mode-p 'dired-mode)
      (save-excursion
        (goto-char (point-min))
        (if (dired-goto-file file)
            (progn
              (cond
               ((string= decision "DELETE")
                (dired-mark 1)
                (message "✓ MARKED FOR DELETION: %s (Confidence: %s)" reason confidence))
               ((string= decision "QUARANTINE")
                (dired-flag-file-deletion 1)
                (message "⚠ FLAGGED FOR QUARANTINE: %s (Confidence: %s)" reason confidence))
               ((string= decision "KEEP")
                (dired-unmark 1)
                (message "✓ KEEP: %s (Confidence: %s)" reason confidence))
               (t
                (message "❌ Unknown verdict: %s" decision)))
              ;; Force dired to redisplay marks
              (dired-move-to-filename))
          (message "❌ Could not find file in dired: %s" (file-name-nondirectory file)))))))

;;;###autoload
(defun dired-ask-claude-loadbearing ()
  "Ask Claude if the file at point is load-bearing using gptel.
Marks files based on Claude's assessment."
  (interactive)
  (let* ((file (dired-get-filename))
         (metadata (dired-get-file-metadata file))
         (prompt (dired-build-claude-prompt metadata))
         (dired-buffer (current-buffer)))
    (message "Asking Claude about: %s..." (file-name-nondirectory file))
    (gptel-request
        prompt
      :callback
      (lambda (response info)
        (if (not response)
            (message "Claude API error: %s" info)
          (let ((verdict (dired-parse-claude-response response)))
            (if verdict
                (progn
                  ;; Switch back to dired buffer to mark file
                  (with-current-buffer dired-buffer
                    (dired-mark-based-on-verdict verdict file))
                  ;; Refresh dired to show marks
                  (with-current-buffer dired-buffer
                    (revert-buffer)))
              (message "Failed to get clear verdict from Claude"))))))))

;;;###autoload
(defun dired-ask-claude-bulk ()
  "Ask Claude about all marked files (not directories) in dired.
Processes each marked file sequentially with delay."
  (interactive)
  (let* ((all-marked (dired-get-marked-files))
         (files (seq-filter (lambda (f) (not (file-directory-p f))) all-marked))
         (dired-buffer (current-buffer)))
    (cond
     ((null all-marked)
      (message "No files marked"))
     ((null files)
      (message "No files in marked items - use C-c D for directories"))
     (t
      (message "Asking Claude about %d files..." (length files))
      (dired-ask-claude-bulk-process files 0 dired-buffer)))))

;;;###autoload
(defun dired-ask-claude-bulk-directories ()
  "Ask Claude about all marked directories in dired.
Processes each marked directory sequentially with delay."
  (interactive)
  (let* ((all-marked (dired-get-marked-files))
         (marked-dirs (seq-filter #'file-directory-p all-marked))
         (dired-buffer (current-buffer)))
    (cond
     ((null all-marked)
      (message "No files marked"))
     ((null marked-dirs)
      (message "No directories in marked files - use C-c F for files"))
     (t
      (message "Asking Claude about %d marked directories..." (length marked-dirs))
      (dired-ask-claude-bulk-directories-process marked-dirs 0 dired-buffer)))))

(defun dired-ask-claude-bulk-directories-process (dirs index dired-buffer)
  "Process DIRS list at INDEX, asking Claude about each directory."
  (when (< index (length dirs))
    (let* ((dir (nth index dirs))
           (metadata (dired-get-file-metadata dir))
           (prompt (dired-build-claude-prompt metadata)))
      (message "Processing directory %d/%d: %s"
               (1+ index)
               (length dirs)
               (file-name-nondirectory dir))
      (gptel-request
          prompt
        :callback
        (lambda (response info)
          (if (not response)
              (message "Claude API error on %s: %s" dir info)
            (let ((verdict (dired-parse-claude-response response)))
              (when verdict
                ;; Switch back to dired buffer to mark file
                (with-current-buffer dired-buffer
                  (dired-mark-based-on-verdict verdict dir)))))
          ;; Process next directory after 2 second delay (rate limiting)
          (run-with-timer 2 nil
                          #'dired-ask-claude-bulk-directories-process
                          dirs
                          (1+ index)
                          dired-buffer))))))

;;;###autoload
(defun dired-ask-claude-smart-bulk ()
  "Smart bulk ask: directories get directory analysis, files get file analysis.
Processes all marked items with appropriate analysis for each type."
  (interactive)
  (let* ((all-marked (dired-get-marked-files))
         (marked-dirs (seq-filter #'file-directory-p all-marked))
         (marked-files (seq-filter (lambda (f) (not (file-directory-p f))) all-marked))
         (dired-buffer (current-buffer)))
    (cond
     ((null all-marked)
      (message "No files marked"))
     (t
      (let ((total-items (+ (length marked-dirs) (length marked-files))))
        (message "Asking Claude about %d items (%d directories, %d files)..."
                 total-items (length marked-dirs) (length marked-files))
        ;; Process directories first, then files
        (dired-ask-claude-smart-bulk-process marked-dirs marked-files 0 0 dired-buffer))))))

(defun dired-ask-claude-smart-bulk-process (dirs files dir-index file-index dired-buffer)
  "Process DIRS and FILES lists, handling directories first then files."
  (cond
   ;; Still processing directories
   ((< dir-index (length dirs))
    (let* ((dir (nth dir-index dirs))
           (metadata (dired-get-file-metadata dir))
           (prompt (dired-build-claude-prompt metadata)))
      (message "Processing directory %d/%d: %s"
               (1+ dir-index)
               (+ (length dirs) (length files))
               (file-name-nondirectory dir))
      (gptel-request
          prompt
        :callback
        (lambda (response info)
          (if (not response)
              (message "Claude API error on %s: %s" dir info)
            (let ((verdict (dired-parse-claude-response response)))
              (when verdict
                (with-current-buffer dired-buffer
                  (dired-mark-based-on-verdict verdict dir)))))
          ;; Continue with next directory
          (run-with-timer 2 nil
                          #'dired-ask-claude-smart-bulk-process
                          dirs files (1+ dir-index) file-index dired-buffer)))))

   ;; Processing files
   ((< file-index (length files))
    (let* ((file (nth file-index files))
           (metadata (dired-get-file-metadata file))
           (prompt (dired-build-claude-prompt metadata)))
      (message "Processing file %d/%d: %s"
               (+ (length dirs) (1+ file-index))
               (+ (length dirs) (length files))
               (file-name-nondirectory file))
      (gptel-request
          prompt
        :callback
        (lambda (response info)
          (if (not response)
              (message "Claude API error on %s: %s" file info)
            (let ((verdict (dired-parse-claude-response response)))
              (when verdict
                (with-current-buffer dired-buffer
                  (dired-mark-based-on-verdict verdict file)))))
          ;; Continue with next file
          (run-with-timer 2 nil
                          #'dired-ask-claude-smart-bulk-process
                          dirs files dir-index (1+ file-index) dired-buffer)))))

   ;; All done
   (t
    (message "✓ Completed analysis of %d directories and %d files"
             (length dirs) (length files))
    (with-current-buffer dired-buffer
      (revert-buffer)))))

(defun dired-ask-claude-bulk-process (files index dired-buffer)
  "Process FILES list at INDEX, asking Claude about each one."
  (when (< index (length files))
    (let* ((file (nth index files))
           (metadata (dired-get-file-metadata file))
           (prompt (dired-build-claude-prompt metadata)))
      (message "Processing %d/%d: %s"
               (1+ index)
               (length files)
               (file-name-nondirectory file))
      (gptel-request
          prompt
        :callback
        (lambda (response info)
          (if (not response)
              (message "Claude API error on %s: %s" file info)
            (let ((verdict (dired-parse-claude-response response)))
              (when verdict
                ;; Switch back to dired buffer to mark file
                (with-current-buffer dired-buffer
                  (dired-mark-based-on-verdict verdict file)))))
          ;; Process next file after 2 second delay (rate limiting)
          (run-with-timer 2 nil
                          #'dired-ask-claude-bulk-process
                          files
                          (1+ index)
                          dired-buffer))))))

;;;###autoload
(defun dired-ask-claude-directory ()
  "Ask Claude about the entire directory at point."
  (interactive)
  (let* ((dir (dired-get-filename))
         (is-dir (file-directory-p dir)))
    (if (not is-dir)
        (message "Not a directory: %s" dir)
      (let* ((metadata (dired-get-file-metadata dir))
             (prompt (dired-build-claude-prompt metadata)))
        (message "Asking Claude about directory: %s..." (file-name-nondirectory dir))
        (gptel-request
            prompt
          :callback
          (lambda (response info)
            (if (not response)
                (message "Claude API error: %s" info)
              (let ((verdict (dired-parse-claude-response response)))
                (if verdict
                    (progn
                      (dired-mark-based-on-verdict verdict dir)
                      (dired-revert))
                  (message "Failed to get clear verdict from Claude"))))))))))

;; Keybindings - add to your config:
;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "C-c a") #'dired-ask-claude-loadbearing)
;;   (define-key dired-mode-map (kbd "C-c A") #'dired-ask-claude-bulk)
;;   (define-key dired-mode-map (kbd "C-c D") #'dired-ask-claude-directory)
;;   (define-key dired-mode-map (kbd "C-c M") #'dired-archive-marked-files))

;;; Archive Functions

(defun dired-is-prehistoric-file (file)
  "Check if FILE is from the prehistoric period (2016-2022)."
  (when (file-directory-p file)
    (let ((git-dir (expand-file-name ".git" file)))
      (when (file-exists-p git-dir)
        (let* ((default-directory file)
               (first-commit-date
                (ignore-errors
                  (string-trim
                   (shell-command-to-string
                    "git log --reverse --format=%cd --date=short 2>/dev/null | head -1")))))
          (when (and first-commit-date (> (length first-commit-date) 0))
            (let ((year (string-to-number (substring first-commit-date 0 4))))
              (and (>= year 2016) (<= year 2022)))))))))

(defun dired-get-archive-path (file)
  "Get the archive destination path for FILE."
  (let* ((archive-root (expand-file-name dired-claude-archive-root))
         (is-prehistoric (dired-is-prehistoric-file file))
         (home-dir (expand-file-name "~"))
         (relative-from-home (file-relative-name file home-dir)))
    (if is-prehistoric
        ;; Prehistoric files: ~/.archive/history/relative-path-from-home
        (expand-file-name relative-from-home
                          (expand-file-name dired-claude-history-archive archive-root))
      ;; Regular git repos: ~/.archive/git/basename
      (expand-file-name (file-name-nondirectory file)
                        (expand-file-name dired-claude-git-archive archive-root)))))

(defun dired-safe-archive-file (source dest)
  "Safely archive SOURCE to DEST using copy-then-delete approach.
Returns t on success, nil on failure."
  (condition-case err
      (progn
        ;; Create destination directory if needed
        (let ((dest-dir (file-name-directory dest)))
          (unless (file-exists-p dest-dir)
            (make-directory dest-dir t)))

        ;; Check if destination already exists
        (when (file-exists-p dest)
          (let ((backup-dest (format "%s.backup-%s" dest
                                     (format-time-string "%Y%m%d-%H%M%S"))))
            (message "Destination exists, backing up to: %s" backup-dest)
            (rename-file dest backup-dest)))

        ;; Copy the file/directory
        (if (file-directory-p source)
            (copy-directory source dest nil t t)
          (copy-file source dest t))

        ;; Verify the copy succeeded
        (if (file-exists-p dest)
            (progn
              ;; Copy succeeded, now remove original
              (if (file-directory-p source)
                  (delete-directory source t)
                (delete-file source))
              (message "✓ Archived: %s → %s"
                       (file-name-nondirectory source)
                       (file-relative-name dest "~"))
              t)
          (error "Copy verification failed")))
    (error
     (message "❌ Archive failed for %s: %s"
              (file-name-nondirectory source)
              (error-message-string err))
     nil)))

;;;###autoload
(defun dired-archive-marked-files ()
  "Archive all marked files to appropriate archive directories.
Prehistoric files (2016-2022) go to ~/.archive/history/rel-path
Other files go to ~/.archive/git/"
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (null marked-files)
        (message "No files marked for archiving")
      (let ((success-count 0)
            (total-count (length marked-files)))
        (message "Archiving %d marked files..." total-count)
        (dolist (file marked-files)
          (let* ((dest (dired-get-archive-path file))
                 (is-prehistoric (dired-is-prehistoric-file file)))
            (message "Archiving %s%s..."
                     (file-name-nondirectory file)
                     (if is-prehistoric " [PREHISTORIC]" ""))
            (when (dired-safe-archive-file file dest)
              (setq success-count (1+ success-count)))))
        (message "✓ Archived %d/%d files successfully" success-count total-count)
        ;; Refresh dired to show files are gone
        (revert-buffer)))))

;;;###autoload
(defun dired-archive-file-at-point ()
  "Archive the file at point to appropriate archive directory."
  (interactive)
  (let* ((file (dired-get-filename))
         (dest (dired-get-archive-path file))
         (is-prehistoric (dired-is-prehistoric-file file)))
    (message "Archiving %s%s to %s..."
             (file-name-nondirectory file)
             (if is-prehistoric " [PREHISTORIC]" "")
             (file-relative-name dest "~"))
    (when (dired-safe-archive-file file dest)
      (revert-buffer))))

;;;###autoload
(defun dired-preview-archive-destinations ()
  "Show where marked files would be archived without actually moving them."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (null marked-files)
        (message "No files marked")
      (with-output-to-temp-buffer "*Archive Preview*"
        (princ "Archive destinations for marked files:\n\n")
        (dolist (file marked-files)
          (let* ((dest (dired-get-archive-path file))
                 (is-prehistoric (dired-is-prehistoric-file file))
                 (basename (file-name-nondirectory file)))
            (princ (format "%s%s\n  → %s\n\n"
                           basename
                           (if is-prehistoric " [PREHISTORIC - 2016-2022]" " [REGULAR]")
                           dest))))))))

;; Keybindings - add to your config:
(with-eval-after-load 'dired
  ;; Ask Claude functions
  (define-key dired-mode-map (kbd "C-c a") #'dired-ask-claude-loadbearing)      ; Single file/dir at point
  (define-key dired-mode-map (kbd "C-c A") #'dired-ask-claude-smart-bulk)       ; Smart bulk (dirs + files)
  (define-key dired-mode-map (kbd "C-c D") #'dired-ask-claude-bulk-directories) ; Bulk directories only
  (define-key dired-mode-map (kbd "C-c F") #'dired-ask-claude-bulk)             ; Bulk files only
  (define-key dired-mode-map (kbd "C-c d") #'dired-ask-claude-directory)        ; Single directory analysis
  ;; Archive functions
  (define-key dired-mode-map (kbd "C-c M") #'dired-archive-marked-files)        ; Archive marked
  (define-key dired-mode-map (kbd "C-c m") #'dired-archive-file-at-point)       ; Archive current
  (define-key dired-mode-map (kbd "C-c p") #'dired-preview-archive-destinations)) ; Preview destinations

(provide 'dired-loadbearing)
;;; dired-loadbearing.el ends here
