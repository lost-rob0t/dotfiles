;;; org-vector.el --- Org-Roam vector search integration -*- lexical-binding: t; -*-

;; Author: N545PY
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ai, org, vector, search
;; URL: https://example.com/org-vector

;;; Commentary:
;; A simple interface to the vectored-notes project.
;; Provides interactive search against an Org-Roam vector store
;; and displays results in a right-side popup buffer with clickable links.
;; Now with async support and telemetry error filtering.

;;; Code:

(defgroup ai nil
  "Customization group for AI-related packages."
  :group 'applications)

(defcustom org-vector-command "org-vector"
  "Name or path of the `org-vector` CLI command."
  :type 'string
  :group 'ai)

(defcustom org-vector-dir "~/Documents/Notes/org/roam/"
  "Path to your Org-Roam directory."
  :type 'directory
  :group 'ai)

(defcustom org-vector-db "~/.cache/org-vector/"
  "Path to store vector embeddings."
  :type 'directory
  :group 'ai)

(defcustom org-vector-model "nomic-embed-text"
  "Ollama embeddings model to use."
  :type 'string
  :group 'ai)

(defcustom org-vector-url "http://localhost:11434"
  "Ollama API endpoint URL."
  :type 'string
  :group 'ai)

(defvar org-vector--current-process nil
  "Current running org-vector process.")

(defvar org-vector--embed-process nil
  "Current running org-vector embedding process.")

(defun org-vector--filter-output (output)
  "Filter unwanted telemetry errors from OUTPUT."
  (let ((lines (split-string output "\n")))
    (string-join
     (seq-filter (lambda (line)
                   (not (or (string-match-p "Failed to send telemetry event" line)
                            (string-match-p "capture() takes 1 positional argument" line))))
                 lines)
     "\n")))

(defun org-vector--kill-current-process ()
  "Kill the current org-vector process if it's running."
  (when (and org-vector--current-process
             (process-live-p org-vector--current-process))
    (kill-process org-vector--current-process)
    (setq org-vector--current-process nil)))

(defun org-vector--kill-embed-process ()
  "Kill the current embedding process if it's running."
  (when (and org-vector--embed-process
             (process-live-p org-vector--embed-process))
    (kill-process org-vector--embed-process)
    (setq org-vector--embed-process nil)))

(defun org-vector--setup-results-buffer (query)
  "Create and setup the results buffer for QUERY."
  (let ((buf (get-buffer-create "*Org Vector Results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "* Vector Search: %s\n\n" query))
        (insert "Searching... Please wait.\n\n")
        (insert "Use \\[org-vector-cancel-search] to cancel the search.\n")
        (goto-char (point-min))))
    (display-buffer buf
                    '((display-buffer-in-side-window)
                      (side . right)
                      (window-width . 0.35)))
    buf))

(defun org-vector--validate-setup ()
  "Validate that the required files and directories exist."
  (let ((errors '()))
    (unless (file-directory-p (expand-file-name org-vector-dir))
      (push (format "Org directory not found: %s" org-vector-dir) errors))
    (unless (file-directory-p (expand-file-name org-vector-db))
      (push (format "Vector DB directory not found: %s" org-vector-db) errors))
    (unless (executable-find org-vector-command)
      (push (format "org-vector command not found or not executable: %s"
                    org-vector-command)
            errors))
    errors))

(defun org-vector--process-finished (process event query)
  "Handle completion of org-vector PROCESS with EVENT for QUERY."
  (let ((buf (get-buffer "*Org Vector Results*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (cond
           ((string-match-p "finished" event)
            (let* ((raw-output (with-current-buffer (process-buffer process)
                                 (buffer-string)))
                   (filtered-output (org-vector--filter-output raw-output)))
              (erase-buffer)
              (org-mode)
              (insert (format "* Vector Search: %s\n\n" query))
              (if (string-empty-p (string-trim filtered-output))
                  (insert "No results found.\n\n")
                (insert filtered-output)
                (insert "\n"))
              (insert "Search completed. Use \\[org-vector-search] to search again.\n")
              (goto-char (point-min))
              (message "Vector search completed")))
           ((string-match-p "exited abnormally\\|failed" event)
            (let* ((raw-error-output (with-current-buffer (process-buffer process)
                                       (buffer-string)))
                   (filtered-error-output (org-vector--filter-output raw-error-output)))
              (erase-buffer)
              (org-mode)
              (insert (format "* Vector Search: %s\n\n" query))
              (insert "** Error occurred during search:\n\n")
              (if (string-empty-p (string-trim filtered-error-output))
                  (insert "Unknown error - check org-vector CLI and dependencies.\n\n")
                (insert (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n\n" filtered-error-output)))
              (insert "** Troubleshooting:\n")
              (insert "- Check that Ollama is running\n")
              (insert "- Verify that the model is downloaded\n")
              (insert "- Ensure the vector database has been created\n")
              (insert "- Check Python / org-vector dependencies\n\n")
              (insert "Use \\[org-vector-search] to try again.\n")
              (goto-char (point-min))
              (message "Vector search failed - check *Org Vector Results* buffer")))
           (t
            (erase-buffer)
            (org-mode)
            (insert (format "* Vector Search: %s\n\n" query))
            (insert (format "Search was interrupted: %s\n\n" (string-trim event)))
            (insert "Use \\[org-vector-search] to try again.\n")
            (goto-char (point-min))
            (message "Vector search was interrupted"))))))
    ;; Clean up process reference
    (when (eq process org-vector--current-process)
      (setq org-vector--current-process nil))))

(defun org-vector--embed-finished (process event)
  "Handle completion of embedding PROCESS with EVENT."
  (let ((buf (get-buffer "*org-vector-embed*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((raw-output (buffer-string))
               (filtered-output (org-vector--filter-output raw-output)))
          (erase-buffer)
          (insert filtered-output))))
    (cond
     ((string-match-p "finished" event)
      (message "Embedding completed successfully"))
     ((string-match-p "exited abnormally\\|failed" event)
      (message "Embedding failed - check *org-vector-embed* buffer"))
     (t
      (message "Embedding process interrupted")))
    ;; Clean up process reference
    (when (eq process org-vector--embed-process)
      (setq org-vector--embed-process nil))))

(defun org-vector--run-search-async (query)
  "Run an async vector search for QUERY."
  (let ((errors (org-vector--validate-setup)))
    (if errors
        (let ((buf (get-buffer-create "*Org Vector Results*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (org-mode)
              (insert (format "* Vector Search: %s\n\n" query))
              (insert "** Configuration Errors:\n\n")
              (dolist (error errors)
                (insert (format "- %s\n" error)))
              (insert "\n** Fix these issues and try again.\n")
              (goto-char (point-min))))
          (display-buffer buf
                          '((display-buffer-in-side-window)
                            (side . right)
                            (window-width . 0.35)))
          (message "Configuration errors - check *Org Vector Results* buffer"))
      ;; Kill any existing process
      (org-vector--kill-current-process)
      ;; Setup results buffer with loading message
      (org-vector--setup-results-buffer query)
      (message "Starting vector search...")
      ;; Start async process
      (let* ((process-buffer (generate-new-buffer " *org-vector-process*"))
             (process (make-process
                       :name "org-vector"
                       :buffer process-buffer
                       :command (list org-vector-command
                                      "emacs"
                                      "-d" (expand-file-name org-vector-dir)
                                      "-p" (expand-file-name org-vector-db)
                                      "-m" org-vector-model
                                      "-u" org-vector-url
                                      "-q" query)
                       :sentinel (lambda (proc event)
                                   (org-vector--process-finished proc event query)
                                   ;; Clean up process buffer
                                   (when (buffer-live-p (process-buffer proc))
                                     (kill-buffer (process-buffer proc)))))))
        (setq org-vector--current-process process)))))

(defun org-vector-search (query)
  "Interactively run a vector search for QUERY and display results in a side buffer."
  (interactive "sVector search query: ")
  (if (string-empty-p (string-trim query))
      (message "Query cannot be empty")
    (org-vector--run-search-async query)))

(defun org-vector-search-at-point ()
  "Run vector search using the word or region at point."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'word t))))
    (if query
        (org-vector-search query)
      (call-interactively #'org-vector-search))))

(defun org-vector-cancel-search ()
  "Cancel the current vector search if running."
  (interactive)
  (if org-vector--current-process
      (progn
        (org-vector--kill-current-process)
        (let ((buf (get-buffer "*Org Vector Results*")))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n** Search cancelled by user.\n")))))
        (message "Vector search cancelled"))
    (message "No search currently running")))

(defun org-vector-cancel-embed ()
  "Cancel the current embedding process if running."
  (interactive)
  (if org-vector--embed-process
      (progn
        (org-vector--kill-embed-process)
        (message "Embedding process cancelled"))
    (message "No embedding process currently running")))

(defun org-vector-embed-files ()
  "Run the embedding process to index org files."
  (interactive)
  (let ((errors (org-vector--validate-setup)))
    (if errors
        (message "Configuration errors: %s" (mapconcat #'identity errors "; "))
      ;; Kill any existing embedding process
      (org-vector--kill-embed-process)
      (message "Starting embedding process...")
      (let* ((process-buffer (get-buffer-create "*org-vector-embed*"))
             (process (make-process
                       :name "org-vector-embed"
                       :buffer process-buffer
                       :command (list org-vector-command
                                      "embed"
                                      "-d" (expand-file-name org-vector-dir)
                                      "-p" (expand-file-name org-vector-db)
                                      "-m" org-vector-model
                                      "-u" org-vector-url)
                       :sentinel #'org-vector--embed-finished)))
        (setq org-vector--embed-process process)
        (display-buffer process-buffer)))))

(provide 'org-vector)
;;; org-vector.el ends here
