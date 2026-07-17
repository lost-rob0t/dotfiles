;;; org-vector.el --- Org-Roam vector search integration -*- lexical-binding: t; -*-

;; Author: N545PY
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (transient "0.4") (gptel "0.9"))
;; Keywords: ai, org, vector, search, gptel
;; URL: https://example.com/org-vector

;;; Commentary:
;; A modern interface to the vectored-notes project.
;; Provides interactive search against an Org-Roam vector store,
;; background indexing service, and gptel tool integration.

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup org-vector nil
  "Org-Roam vector search integration."
  :group 'applications
  :prefix "org-vector-")

(defcustom org-vector-dir "~/Documents/Notes/org/roam/"
  "Path to your Org-Roam directory."
  :type 'directory
  :group 'org-vector)

(defcustom org-vector-db "~/.cache/org-vector/"
  "Path to store vector embeddings."
  :type 'directory
  :group 'org-vector)

(defcustom org-vector-model "all-MiniLM-L6-v2"
  "Embeddings model to use."
  :type 'string
  :group 'org-vector)

(defcustom org-vector-collection-name "org-roam"
  "ChromaDB collection name for embeddings."
  :type 'string
  :group 'org-vector)

(defcustom org-vector-ingestion-instructions nil
  "Instruction prefix/template for document embeddings.
If nil, uses model-specific defaults:
- nomic-embed: \='search_document:\='
- e5: \='passage:\='
- default: descriptive prefix"
  :type '(choice (const :tag "Auto (model-specific)" nil)
                 (string :tag "Custom instructions"))
  :group 'org-vector)

(defcustom org-vector-query-instructions nil
  "Instruction prefix/template for query embeddings.
If nil, uses model-specific defaults:
- nomic-embed: \='search_query:\='
- e5: \='query:\='
- default: descriptive prefix"
  :type '(choice (const :tag "Auto (model-specific)" nil)
                 (string :tag "Custom instructions"))
  :group 'org-vector)

(defcustom org-vector-log-level "ERROR"
  "Log level for the Python backend."
  :type '(choice (const "DEBUG")
                 (const "INFO")
                 (const "WARNING")
                 (const "ERROR")
                 (const "CRITICAL"))
  :group 'org-vector)

(defcustom org-vector-log-to-file nil
  "Whether to log Python backend output to a file."
  :type 'boolean
  :group 'org-vector)

(defcustom org-vector-service-debounce-seconds 2.0
  "Debounce window before syncing after file events (serve mode)."
  :type 'number
  :group 'org-vector)

(defcustom org-vector-service-poll-timeout-ms 1000
  "Inotify poll timeout in milliseconds (serve mode)."
  :type 'integer
  :group 'org-vector)

(defvar org-vector--search-process nil
  "Current running org-vector search process.")

(defvar org-vector--embed-process nil
  "Current running org-vector embedding process.")

(defvar org-vector--service-process nil
  "Current running org-vector background service process.")

(defvar org-vector--last-results nil
  "Cache of last search results for gptel tool.")

;; Utility functions

(defun org-vector--ensure-directories ()
  "Ensure required directories exist, creating them if needed."
  (let ((db-dir (expand-file-name org-vector-db)))
    (unless (file-directory-p db-dir)
      (make-directory db-dir t)
      (message "Created vector database directory: %s" db-dir))))

(defun org-vector--resolve-command ()
  "Return the resolved `org-vector` executable path."
  (or (executable-find "org-vector")
      (error "org-vector command not found in PATH")))

(defun org-vector--filter-output (output)
  "Filter unwanted telemetry errors from OUTPUT."
  (let ((lines (split-string output "\n")))
    (string-join
     (seq-filter (lambda (line)
                   (not (or (string-match-p "Failed to send telemetry event" line)
                            (string-match-p "capture() takes 1 positional argument" line)
                            (string-match-p "anonymized_telemetry" line))))
                 lines)
     "\n")))

(defun org-vector--kill-process-if-live (process-var)
  "Kill process stored in PROCESS-VAR if it's live, then set to nil."
  (when-let ((proc (symbol-value process-var)))
    (when (process-live-p proc)
      (kill-process proc))
    (set process-var nil)))

(defun org-vector--build-base-args (&optional mode)
  "Build base argument list for org-vector command.
If MODE is specified, adds it as the first positional argument."
  (let ((args (list)))
    (when mode
      (push mode args))
    (push "-d" args)
    (push (expand-file-name org-vector-dir) args)
    (push "-p" args)
    (push (expand-file-name org-vector-db) args)
    (push "-m" args)
    (push org-vector-model args)
    (push "-c" args)
    (push org-vector-collection-name args)
    (push "--log-level" args)
    (push org-vector-log-level args)
    (when org-vector-log-to-file
      (push "--log-to-file" args))
    (when org-vector-ingestion-instructions
      (push "--ingestion-instructions" args)
      (push org-vector-ingestion-instructions args))
    (when org-vector-query-instructions
      (push "--query-instructions" args)
      (push org-vector-query-instructions args))
    (nreverse args)))

;; Process management

(defun org-vector-stop-all ()
  "Stop all org-vector processes (search, embed, service)."
  (interactive)
  (org-vector--kill-process-if-live 'org-vector--search-process)
  (org-vector--kill-process-if-live 'org-vector--embed-process)
  (org-vector--kill-process-if-live 'org-vector--service-process)
  (message "All org-vector processes stopped"))

;; Search functionality

(defun org-vector--setup-results-buffer (query)
  "Create and setup the results buffer for QUERY."
  (let ((buf (get-buffer-create "*Org Vector Results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "* Vector Search: %s\n\n" query))
        (insert "Searching... Please wait.\n\n")
        (insert "Use M-x org-vector-stop-search to cancel.\n")
        (goto-char (point-min))))
    (display-buffer buf
                    '((display-buffer-in-side-window)
                      (side . right)
                      (window-width . 0.35)))
    buf))

(defun org-vector--search-finished (process event query)
  "Handle completion of search PROCESS with EVENT for QUERY."
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
              (insert "Search completed. Use M-x org-vector-search to search again.\n")
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
                  (insert "Unknown error - check Python script and dependencies.\n\n")
                (insert (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n\n" filtered-error-output)))
              (insert "** Troubleshooting:\n")
              (insert "- Verify that the model is downloaded\n")
              (insert "- Ensure the vector database has been created (run M-x org-vector-embed)\n")
              (insert "- Check Python dependencies\n\n")
              (insert "Use M-x org-vector-search to try again.\n")
              (goto-char (point-min))
              (message "Vector search failed - check *Org Vector Results* buffer")))
           (t
            (erase-buffer)
            (org-mode)
            (insert (format "* Vector Search: %s\n\n" query))
            (insert (format "Search was interrupted: %s\n\n" (string-trim event)))
            (insert "Use M-x org-vector-search to try again.\n")
            (goto-char (point-min))
            (message "Vector search was interrupted"))))))
    ;; Clean up process reference
    (when (eq process org-vector--search-process)
      (setq org-vector--search-process nil))
    ;; Clean up process buffer
    (when-let ((proc-buf (process-buffer process)))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(defun org-vector--run-search-async (query)
  "Run an async vector search for QUERY."
  (org-vector--ensure-directories)
  (org-vector--kill-process-if-live 'org-vector--search-process)
  
  ;; Setup results buffer with loading message
  (org-vector--setup-results-buffer query)
  (message "Starting vector search...")
  
  ;; Start async process
  (let* ((process-buffer (generate-new-buffer " *org-vector-search*"))
          (command (append (list (org-vector--resolve-command))
                          (org-vector--build-base-args "emacs")
                          (list "-q" query)))
         (process (make-process
                   :name "org-vector-search"
                   :buffer process-buffer
                   :command command
                   :sentinel (lambda (proc event)
                              (org-vector--search-finished proc event query)))))
    (setq org-vector--search-process process)))

;;;###autoload
(defun org-vector-search (query)
  "Interactively run a vector search for QUERY and display results in a side buffer."
  (interactive "sVector search query: ")
  (if (string-empty-p (string-trim query))
      (message "Query cannot be empty")
    (org-vector--run-search-async query)))

;;;###autoload
(defun org-vector-search-at-point ()
  "Run vector search using the word or region at point."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'word t))))
    (if query
        (org-vector-search query)
      (call-interactively #'org-vector-search))))

;;;###autoload
(defun org-vector-stop-search ()
  "Cancel the current vector search if running."
  (interactive)
  (if org-vector--search-process
      (progn
        (org-vector--kill-process-if-live 'org-vector--search-process)
        (let ((buf (get-buffer "*Org Vector Results*")))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n** Search cancelled by user.\n")))))
        (message "Vector search cancelled"))
    (message "No search currently running")))

;; Embedding functionality

(defun org-vector--embed-finished (process event)
  "Handle completion of embedding PROCESS with EVENT."
  (cond
   ((string-match-p "finished" event)
    (message "Embedding completed successfully"))
   ((string-match-p "exited abnormally\\|failed" event)
    (message "Embedding failed - check *org-vector-embed* buffer"))
   (t
    (message "Embedding process interrupted")))
  ;; Clean up process reference
  (when (eq process org-vector--embed-process)
    (setq org-vector--embed-process nil)))

;;;###autoload
(defun org-vector-embed ()
  "Run the embedding process to index org files."
  (interactive)
  (org-vector--ensure-directories)
  (org-vector--kill-process-if-live 'org-vector--embed-process)
  
  (message "Starting embedding process...")
  (let* ((process-buffer (get-buffer-create "*org-vector-embed*"))
          (command (append (list (org-vector--resolve-command))
                          (org-vector--build-base-args "embed")))
         (process (make-process
                   :name "org-vector-embed"
                   :buffer process-buffer
                   :command command
                   :sentinel #'org-vector--embed-finished)))
    (setq org-vector--embed-process process)
    (display-buffer process-buffer)))

;;;###autoload
(defun org-vector-stop-embed ()
  "Cancel the current embedding process if running."
  (interactive)
  (if org-vector--embed-process
      (progn
        (org-vector--kill-process-if-live 'org-vector--embed-process)
        (message "Embedding process cancelled"))
    (message "No embedding process currently running")))

;; Background service functionality

(defun org-vector--service-filter (process string)
  "Filter for service PROCESS output STRING."
  (when-let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (org-vector--filter-output string))))))

(defun org-vector--service-sentinel (process event)
  "Sentinel for service PROCESS with EVENT."
  (message "Org-vector service: %s" (string-trim event))
  (when (eq process org-vector--service-process)
    (setq org-vector--service-process nil)))

;;;###autoload
(defun org-vector-start-service ()
  "Start the background indexing service (inotify watcher)."
  (interactive)
  (org-vector--ensure-directories)
  
  (when (and org-vector--service-process
             (process-live-p org-vector--service-process))
    (error "Service is already running"))
  
  (let* ((process-buffer (get-buffer-create "*org-vector-service*"))
          (command (append (list (org-vector--resolve-command))
                          (org-vector--build-base-args "serve")
                          (list "--debounce-seconds"
                                (number-to-string org-vector-service-debounce-seconds)
                                "--poll-timeout-ms"
                                (number-to-string org-vector-service-poll-timeout-ms))))
         (process (make-process
                   :name "org-vector-service"
                   :buffer process-buffer
                   :command command
                   :filter #'org-vector--service-filter
                   :sentinel #'org-vector--service-sentinel)))
    (setq org-vector--service-process process)
    (display-buffer process-buffer)
    (message "Started org-vector background service")))

;;;###autoload
(defun org-vector-stop-service ()
  "Stop the background indexing service."
  (interactive)
  (if org-vector--service-process
      (progn
        (org-vector--kill-process-if-live 'org-vector--service-process)
        (message "Stopped org-vector background service"))
    (message "No service currently running")))

;;;###autoload
(defun org-vector-service-status ()
  "Show status of the background service."
  (interactive)
  (if (and org-vector--service-process
           (process-live-p org-vector--service-process))
      (message "Org-vector service is running (PID: %s)" 
               (process-id org-vector--service-process))
    (message "Org-vector service is not running")))

;; gptel tool integration

(defun org-vector--query-sync (query &optional k)
  "Query the vector store synchronously for QUERY, returning K results.
Returns a list of result documents with metadata."
  (let* ((temp-file (make-temp-file "org-vector-"))
         (k-val (or k 5))
          (command (append (list (org-vector--resolve-command))
                          (org-vector--build-base-args "json")
                          (list "-q" query
                                "-k" (number-to-string k-val))))
         (output "")
         (exit-code nil))
    
    (with-temp-file temp-file
      (insert ""))
    
    (setq exit-code
          (with-temp-buffer
            (apply #'call-process
                   (car command)
                   nil t nil
                   (cdr command))
            (setq output (buffer-string))
            (point)))
    
    (unwind-protect
        (if (zerop exit-code)
            (let ((results '())
                  (lines (split-string output "\n")))
              (dolist (line lines)
                (when (and (not (string-empty-p line))
                          (string-prefix-p "{" line))
                  (condition-case err
                      (push (json-read-from-string line) results)
                    (error nil))))
              (setq org-vector--last-results results)
              results)
          (error "Query failed: %s" (org-vector--filter-output output)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun org-vector--format-result-for-gptel (result)
  "Format a single RESULT for gptel context."
  (let* ((metadata (cdr (assoc 'metadata result)))
         (filepath (cdr (assoc 'filepath metadata)))
         (title (cdr (assoc 'title metadata)))
         (tags (cdr (assoc 'tags metadata)))
         (path (cdr (assoc 'path metadata)))
         (page-content (cdr (assoc 'page_content result)))
         (distance (cdr (assoc 'distance metadata))))
    (format "** %s\n- File: [[file:%s][%s]]\n- Path: %s\n- Tags: %s\n- Distance: %s\n\n%s"
            (or title "Untitled")
            filepath
            (file-name-nondirectory filepath)
            path
            (if (and tags (not (string-empty-p tags))) tags "none")
            (or distance "N/A")
            page-content)))

;;;###autoload
(defun org-vector-gptel-query (query)
  "Query vector store and insert results at point as org entries.
This is useful for augmenting gptel prompts with relevant notes."
  (interactive "sQuery for related notes: ")
  (message "Querying vector store...")
  (let ((results (org-vector--query-sync query)))
    (if results
        (progn
          (insert (format "* Related Notes for: %s\n\n" query))
          (dolist (result results)
            (insert (org-vector--format-result-for-gptel result))
            (insert "\n"))
          (message "Inserted %d results" (length results)))
      (message "No results found"))))

;; gptel tool registration (when gptel is available)

(with-eval-after-load 'gptel
  (when (fboundp 'gptel-make-tool)
    (gptel-make-tool
     :function (lambda (query &optional k)
                (let ((results (org-vector--query-sync query (or (and k (string-to-number k)) 5))))
                  (if results
                      (mapconcat #'org-vector--format-result-for-gptel results "\n")
                    "No results found.")))
     :name "org_vector_search"
     :category "org"
     :args '((:name "query" :type string
              :description "Search query for org-roam notes")
             (:name "k" :type string :optional t
              :description "Number of results to return (default 5)"))
     :description "Search the org-roam vector store for semantically similar notes. Returns formatted org entries with file links and content.")
    
    (gptel-make-tool
     :function (lambda ()
                (org-vector-embed)
                "Embedding process started. Check *org-vector-embed* buffer for progress.")
     :name "org_vector_sync"
     :category "org"
     :description "Sync/embed all org-roam files into the vector store. Run this after creating new notes or before searching.")))

;; Transient menu

;;;###autoload
(with-eval-after-load 'transient
  (transient-define-prefix org-vector-menu ()
    "Org-Vector search and indexing menu."
    ["Org-Vector: Vector Search for Org-Roam"
     ["Search"
      ("s" "Search..." org-vector-search)
      ("S" "Search at point" org-vector-search-at-point)
      ("c" "Cancel search" org-vector-stop-search)]
     ["Index"
      ("i" "Index/embed files" org-vector-embed)
      ("C" "Cancel embedding" org-vector-stop-embed)]
     ["Service"
      ("b" "Start background service" org-vector-start-service)
      ("e" "Stop background service" org-vector-stop-service)
      ("t" "Service status" org-vector-service-status)]
     ["Tools"
      ("g" "Insert gptel results" org-vector-gptel-query)
      ("k" "Kill all processes" org-vector-stop-all)]
     ["Settings"
      ("o" "Customize" (lambda () (interactive) (customize-group 'org-vector)))]]))

;;;###autoload
(defun org-vector-transient ()
  "Open the org-vector transient menu."
  (interactive)
  (if (fboundp 'org-vector-menu)
      (org-vector-menu)
    (error "Transient not available. Please install the transient package")))

(provide 'org-vector)
;;; org-vector.el ends here
