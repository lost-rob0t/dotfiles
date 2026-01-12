(require 'gptel)
(require 'async)
(require 'org)
(require 'org-ql)
(require 'alert)
(require 'org-ql-view)



;; Core configuration
(defcustom ai/todo-agenda-file (expand-file-name "todo.org" org-directory)
  "Primary AI task management file."
  :group 'LLM)


(defcustom ai/todo-default-query '(and (todo) (ts))
  "Default org-ql query for active tasks."
  :group 'LLM)

(defvar ai/todo-model 'claude-3-5-haiku-20241022 ;; Pretty cheap
  "The Model used to write todo text")

;; Entry extraction pipeline
(defun ai/todo-extract-entry-data (element)
  "Extract structured data from org ELEMENT for LLM processing.
Returns plist with :heading, :todo, :tags, :priority, :deadline, :scheduled."
  (let* ((headline (org-element-property :raw-value element))
         (todo (org-element-property :todo-keyword element))
         (tags (org-element-property :tags element))
         (priority (org-element-property :priority element))
         (deadline (org-element-property :deadline element))
         (scheduled (org-element-property :scheduled element)))
    (list :heading headline
          :todo todo
          :tags tags
          :priority priority
          :deadline deadline
          :scheduled scheduled)))

(defun ai/todo-format-timestamp (ts-element)
  "Format org timestamp ELEMENT for readable display."
  (when ts-element
    (format-time-string "%Y-%m-%d %a"
                        (org-timestamp-to-time ts-element))))

(defun ai/todo-format-entry-string (entry-data)
  "Convert ENTRY-DATA plist to formatted string for LLM consumption."
  (let* ((heading (plist-get entry-data :heading))
         (todo (plist-get entry-data :todo))
         (tags (plist-get entry-data :tags))
         (priority (plist-get entry-data :priority))
         (deadline (ai/todo-format-timestamp (plist-get entry-data :deadline)))
         (scheduled (ai/todo-format-timestamp (plist-get entry-data :scheduled))))
    (concat
     ;; Core task information
     (format "- %s%s"
             (if todo (concat "[" todo "] ") "")
             heading)
     ;; Priority indicator
     (when priority (format " [#%c]" priority))
     ;; Tags
     (when tags (format " :%s:" (string-join tags ":")))
     ;; Scheduling information
     (when scheduled (format " (scheduled: %s)" scheduled))
     (when deadline (format " (deadline: %s)" deadline)))))

;; Query execution pipeline
(defun ai/todo-query-entries (query &optional file)
  "Execute org-ql QUERY on FILE, returning raw elements.
FILE defaults to `ai/todo-agenda-file'."
  (org-ql-select (or file ai/todo-agenda-file)
    query
    :action 'element))

(defun ai/todo-process-entries (elements)
  "Transform org ELEMENTS into LLM-ready strings."
  (mapcar (lambda (element)
            (-> element
                ai/todo-extract-entry-data
                ai/todo-format-entry-string))
          elements))

;; Main interface functions
(defun ai/todo-list-todos-string (&optional query file)
  "Generate LLM-formatted string of todos from org file.
QUERY defaults to `ai/todo-default-query'.
FILE defaults to `ai/todo-agenda-file'."
  (let* ((query (or query ai/todo-default-query))
         (elements (ai/todo-query-entries query file))
         (formatted-entries (ai/todo-process-entries elements)))
    (if formatted-entries
        (string-join formatted-entries "\n")
      "No matching entries found.")))

(defun ai/todo-list-todos-with-context (&optional query file context)
  "Generate contextualized todo list for LLM with optional CONTEXT string."
  (let ((todo-list (ai/todo-list-todos-string query file)))
    (concat
     (when context (format "%s\n\n" context))
     "Current Tasks:\n"
     todo-list)))

;; Specialized query builders
(defun ai/todo-urgent-tasks ()
  "Get urgent tasks (priority A or deadline within 3 days)."
  (ai/todo-list-todos-string '(or (priority "A")
                                  (deadline :to 3))))

(defun ai/todo-project-tasks (project-tag)
  "Get all tasks tagged with PROJECT-TAG."
  (ai/todo-list-todos-string `(tags ,project-tag)))

(defun ai/todo-blocked-tasks ()
  "Get tasks that are blocked or waiting."
  (ai/todo-list-todos-string '(todo "WAITING" "BLOCKED")))

;; Integration helpers
(defun ai/todo-prepare-llm-context (&optional custom-query)
  "Prepare comprehensive task context for LLM analysis.
Includes urgent tasks, blocked items, and general todos."
  (concat
   "=== TASK ANALYSIS REQUEST ===\n\n"
   "URGENT ITEMS:\n" (ai/todo-urgent-tasks) "\n\n"
   "BLOCKED ITEMS:\n" (ai/todo-blocked-tasks) "\n\n"
   "ALL ACTIVE TASKS:\n" (ai/todo-list-todos-string custom-query) "\n\n"
   "Please analyze priorities, dependencies, and suggest next actions."))

;; Example usage and testing
(defun ai/todo-demo-queries ()
  "Demonstrate various query capabilities."
  (interactive)
  (let ((results (list
                  (cons "All Todos" (ai/todo-list-todos-string))
                  (cons "Urgent" (ai/todo-urgent-tasks))
                  (cons "Blocked" (ai/todo-blocked-tasks)))))
    (with-current-buffer (get-buffer-create "*AI Todo Demo*")
      (erase-buffer)
      (dolist (result results)
        (insert (format "\n=== %s ===\n%s\n" (car result) (cdr result))))
      (pop-to-buffer (current-buffer)))))


;;;;;;;;;;;;
;;;;;;;;;;;;
;; AI Todo CRUD System Extension
;; Extends the existing ai/todo system with full CRUD operations

(require 'org-id)
(require 'org-element)

;; =============================================================================
;; CORE UTILITIES
;; =============================================================================

(defun ai/todo-generate-id ()
  "Generate unique ID for todo items."
  (org-id-new))

(defun ai/todo-with-file (file function)
  "Execute FUNCTION in FILE buffer, saving afterwards."
  (with-current-buffer (find-file-noselect file)
    (let ((result (funcall function)))
      (save-buffer)
      result)))

(defun ai/todo-find-heading-by-path (path &optional file)
  "Find org heading by PATH (list of heading names) in FILE.
PATH example: '(\"PROFESSIONAL DEVELOPMENT\" \"Bug bounty research\")
Returns point of heading or nil if not found."
  (ai/todo-with-file (or file ai/todo-agenda-file)
                     (lambda ()
                       (save-excursion
                         (goto-char (point-min))
                         (let ((current-path path)
                               (found-point nil))
                           (while (and current-path (not found-point))
                             (let ((heading (car current-path)))
                               (if (re-search-forward
                                    (format "^\\*+ %s\\s-*\\(?:$\\|:.*:$\\)"
                                            (regexp-quote heading)) nil t)
                                   (progn
                                     (setq current-path (cdr current-path))
                                     (when (null current-path)
                                       (setq found-point (point-at-bol))))
                                 (setq current-path nil))))
                           found-point)))))

(defun ai/todo-find-heading-by-name (name &optional file)
  "Find first org heading matching NAME in FILE."
  (ai/todo-with-file (or file ai/todo-agenda-file)
                     (lambda ()
                       (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward
                                (format "^\\*+ %s\\s-*\\(?:$\\|:.*:$\\)"
                                        (regexp-quote name)) nil t)
                           (point-at-bol))))))

(defun ai/todo-get-heading-level (pos)
  "Get heading level at position POS."
  (save-excursion
    (goto-char pos)
    (org-outline-level)))

(defun ai/todo-format-todo-line (todo-data)
  "Format TODO-DATA plist into org-mode todo line.
TODO-DATA keys: :heading :todo :priority :tags :scheduled :deadline :properties"
  (let* ((heading (plist-get todo-data :heading))
         (todo (plist-get todo-data :todo))
         (priority (plist-get todo-data :priority))
         (tags (plist-get todo-data :tags))
         (scheduled (plist-get todo-data :scheduled))
         (deadline (plist-get todo-data :deadline))
         (properties (plist-get todo-data :properties))
         (level (plist-get todo-data :level)))

    (concat
     ;; Heading with todo and priority
     (make-string (or level 2) ?*)
     " "
     (when todo (concat todo " "))
     (when priority (format "[#%s] " priority))
     heading
     (when tags (format " :%s:" (string-join tags ":")))
     "\n"

     ;; Scheduling information
     (when scheduled (format "   SCHEDULED: %s\n" scheduled))
     (when deadline (format "   DEADLINE: %s\n" deadline))

     ;; Properties drawer
     (when properties
       (concat "   :PROPERTIES:\n"
               (mapconcat (lambda (prop)
                            (format "   :%s: %s" (car prop) (cdr prop)))
                          properties "\n")
               "\n   :END:\n")))))
(after! gptel
  (gptel-make-tool
   :name "todo_query"
   :function #'(lambda (query)
                 (condition-case err
                     (let ((parsed (car (read-from-string query))))
                       (when (and (listp parsed) (symbolp (car parsed)))
                         (ai/todo-list-todos-string parsed  ai/todo-agenda-file)))
                   (invalid-read-syntax)))



   :include t
   :category "Productivity"
   :args '((:name "query"
            :type string))
   :description "org-ql query as Lisp expression string. Format: '(predicate args...)' or combine with (and ...) (or ...) (not ...).

Common predicates:
- (todo) - any TODO item
- (todo \"NEXT\" \"WAITING\") - specific TODO states
- (done) - completed items
- (tags \"project\" \"urgent\") - items with these tags
- (priority \"A\") or (priority >= \"B\") - by priority
- (deadline :to 7) - deadline within 7 days
- (scheduled :to 3) - scheduled within 3 days
- (ts :from -7 :to 0) - timestamped in last 7 days
- (heading \"pattern\") - heading matches pattern
- (level 2) - specific outline level

Examples:
- '(and (todo) (tags \"project\"))' - active todos tagged 'project'
- '(or (priority \"A\") (deadline :to 3))' - high priority or due soon
- '(and (todo \"NEXT\") (not (tags \"someday\")))' - NEXT items not marked someday
- '(todo)' - all TODO items

Use single quotes around strings within the query."))

(after! gptel
  (gptel-make-tool
   :name "todo_today"
   :function #'(lambda ()
                 (ai/todo-list-todos-string '(and (and (not (done) ) (not (todo "IDEA"))) (ts today)) ai/todo-agenda-file))



   :include t
   :category "Productivity"
   :args '()
   :description "This tool will return active todos for today only."))
