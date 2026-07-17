(require 'org-ql)
(require 'gptel)
(require 'subr-x)

(let ((ai-lib (expand-file-name "ai.el"
                                (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p ai-lib)
    (load ai-lib nil t)))

(let ((todo-diff-lib (expand-file-name "todo-diff.el"
                                      (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p todo-diff-lib)
    (load todo-diff-lib nil t)))

;;; ai-todo.el --- AI-powered todo management with tool calling

;; SETUP INSTRUCTIONS:
;; 1. Set your OpenRouter API key:
;;    (setenv "OPENROUTER_API_KEY" "your-key-here")
;;    Or store it in auth-source for host "openrouter.ai"
;;
;; 2. Optionally customize your todo file:
;;    (setq ai/todo-file (expand-file-name "~/my-todos.org"))
;;
;; 3. Use the functions:
;;    M-x ai/todo-quick-add    - Quick natural language todo creation
;;    M-x ai/todo-chat         - Interactive chat session
;;    M-x ai/todo-process-with-ai - Synchronous processing

(defgroup ai/todo nil
  "AI-powered todo management tools."
  :prefix "ai/todo-"
  :group 'ai)

(defcustom ai/todo-file (expand-file-name "todo.org" org-directory)
  "The file in which todo's will be placed"
  :group 'ai/todo
  :type 'string)

(defcustom ai/todo-states '("TODO" "NEXT" "WAITING" "SOMEDAY" "DONE" "CANCELED")
  "Valid org-mode todo states to use."
  :group 'ai/todo
  :type '(repeat string))


(defun ai/todo--get-context-info ()
  "Get current context information for the AI."
  (let* ((current-time (format-time-string "%Y-%m-%d %A %H:%M"))
         (clock-info (ai/todo--get-clock-info))
         (file-path ai/todo-file))
    (format "Current date/time: %s
Todo file: %s
%s" current-time file-path clock-info)))

(defun ai/todo--get-clock-info ()
  "Get information about currently clocked task."
  (if (org-clock-is-active)
      (let* ((marker org-clock-marker)
             (buffer (marker-buffer marker))
             (heading (with-current-buffer buffer
                        (save-excursion
                          (goto-char marker)
                          (org-get-heading t t t t)))))
        (format "Currently clocked in: %s (started at %s)"
                heading
                (format-time-string "%H:%M" org-clock-start-time)))
    "No task currently clocked in."))

(defun ai/todo--resolve-file (&optional todo-file)
  "Return absolute todo file path for TODO-FILE or `ai/todo-file'."
  (expand-file-name (or todo-file ai/todo-file)))

(defun ai/todo--ensure-file (file)
  "Ensure FILE and its parent directory exist with Org headers."
  (let ((dir (file-name-directory file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (unless (file-exists-p file)
    (with-temp-file file
      (insert "#+TITLE: Todo List\n")
      (insert "#+TODO: TODO NEXT WAITING SOMEDAY | DONE CANCELED\n\n"))))

(defun ai/todo--normalize-datetime (value)
  "Normalize VALUE datetime string in YYYY-MM-DD or YYYY-MM-DD HH:MM format."
  (when value
    (let ((trimmed (string-trim value)))
      (if (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\'" trimmed)
          trimmed
        (error "Invalid datetime format: %s" value)))))

(defun ai/todo--find-heading-marker (todo-title &optional todo-file)
  "Return marker for TODO-TITLE in TODO-FILE, or nil if not found."
  (let ((file (ai/todo--resolve-file todo-file)))
    (unless (file-exists-p file)
      (error "Todo file does not exist: %s" file))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (catch 'found
         (org-map-entries
          (lambda ()
            (when (string= (org-get-heading t t t t) todo-title)
              (throw 'found (copy-marker (point)))))
          nil
          'file)
         nil)))))


(defun ai/todo-write (title &optional todo-file tags priority deadline scheduled)
  "Write a new todo item to org file.
TITLE is the todo heading text.
TODO-FILE is the target org file path (defaults to ~/todo.org).
TAGS is a space-separated string of tags.
PRIORITY is A, B, or C.
DEADLINE is a datetime string in YYYY-MM-DD or YYYY-MM-DD HH:MM format.
SCHEDULED is a datetime string in YYYY-MM-DD or YYYY-MM-DD HH:MM format."
  (let* ((file (ai/todo--resolve-file todo-file))
         (tags-list (when tags (split-string tags " " t)))
         (priority-value (and priority (upcase priority)))
         (scheduled-value (ai/todo--normalize-datetime scheduled))
         (deadline-value (ai/todo--normalize-datetime deadline))
         (todo-text (concat "* TODO ")))

    (when (and priority-value (not (member priority-value '("A" "B" "C"))))
      (error "Priority must be one of A, B, or C"))

    ;; Add priority inline if specified
    (when priority-value
      (setq todo-text (concat todo-text "[#" priority-value "] ")))

    ;; Add title
    (setq todo-text (concat todo-text title))

    ;; Add tags if provided
    (when tags-list
      (setq todo-text (concat todo-text " :" (string-join tags-list ":") ":")))

    (ai/todo--ensure-file file)

    ;; Append the todo
    (with-current-buffer (find-file-noselect file)
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert todo-text "\n")

      ;; Add scheduled date/time if specified
      (when scheduled-value
        (insert (format "SCHEDULED: <%s>\n" scheduled-value)))

      ;; Add deadline if specified
      (when deadline-value
        (insert (format "DEADLINE: <%s>\n" deadline-value)))

      (insert "\n")
      (save-buffer))

    (format "Added todo '%s' to %s" title file)))

(defun ai/todo-list (&optional todo-file query)
  "List todos from org file using org-ql query.
TODO-FILE is the org file path (defaults to ~/todo.org).
QUERY is an org-ql query string (defaults to listing all todos)."
  (let* ((file (expand-file-name (or todo-file ai/todo-file)))
         (query-sexp (if query
                         (read query)
                       '(todo))))

    (unless (file-exists-p file)
      (error "Todo file does not exist: %s" file))

    (let ((results (org-ql-select file query-sexp
                     :action (lambda ()
                               ;; Extract data while positioned at the heading
                               (let* ((element (org-element-at-point))
                                      (title (org-get-heading t t t t))
                                      (todo-keyword (org-element-property :todo-keyword element))
                                      (tags (org-get-tags))
                                      (priority (org-element-property :priority element))
                                      (scheduled (org-element-property :scheduled element))
                                      (deadline (org-element-property :deadline element)))
                                 (list :title title
                                       :todo-keyword todo-keyword
                                       :tags tags
                                       :priority priority
                                       :scheduled scheduled
                                       :deadline deadline))))))
      (if results
          (mapconcat
           (lambda (item)
             (let* ((title (plist-get item :title))
                    (todo-keyword (plist-get item :todo-keyword))
                    (tags (plist-get item :tags))
                    (priority (plist-get item :priority))
                    (scheduled (plist-get item :scheduled))
                    (deadline (plist-get item :deadline)))
               (format "- [%s] %s%s%s%s%s"
                       (or todo-keyword "TODO")
                       (or title "")
                       (if (and tags (> (length tags) 0))
                           (format " :%s:" (string-join tags ":"))
                         "")
                       (if (and priority (characterp priority))
                           (format " [#%c]" priority)
                         "")
                       (if scheduled
                           (condition-case nil
                               (format " SCHEDULED: %s"
                                       (org-timestamp-format scheduled "<%Y-%m-%d %a>"))
                             (error ""))
                         "")
                       (if deadline
                           (condition-case nil
                               (format " DEADLINE: %s"
                                       (org-timestamp-format deadline "<%Y-%m-%d %a>"))
                             (error ""))
                         ""))))
           results "\n")
        "No todos found matching the query."))))

(defun ai/todo-clock-in (todo-title &optional todo-file)
  "Clock in to a todo item by title.
TODO-TITLE is the title of the todo to clock in to.
TODO-FILE is the org file path (defaults to ai/todo-file)."
  (if-let ((marker (ai/todo--find-heading-marker todo-title todo-file)))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (org-clock-in)
          (save-buffer))
        (format "Clocked in to: %s" todo-title))
    (format "Todo not found: %s" todo-title)))

(defun ai/todo-clock-out ()
  "Clock out from the currently active clock."
  (if (org-clock-is-active)
      (progn
        (org-clock-out)
        (format "Clocked out at %s" (format-time-string "%H:%M")))
    "No active clock to clock out from."))

(defun ai/todo-update-state (todo-title new-state &optional todo-file)
  "Update the state of a todo item by title.
TODO-TITLE is the title of the todo to update.
NEW-STATE is the new todo state (TODO, DONE, CANCELED, etc.).
TODO-FILE is the org file path (defaults to ai/todo-file)."
  (let ((target-state (upcase new-state))
        (old-state nil))
    (unless (member target-state ai/todo-states)
      (error "Invalid todo state: %s" new-state))

    (if-let ((marker (ai/todo--find-heading-marker todo-title todo-file)))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (setq old-state (org-get-todo-state))
            (org-todo target-state)
            (save-buffer))
          (format "Updated '%s' from %s to %s"
                  todo-title
                  (if (stringp old-state)
                      (substring-no-properties old-state)
                    "<none>")
                  target-state))
      (format "Todo not found: %s" todo-title))))

(defun ai/todo-get-context ()
  "Get current context about todos and time for the AI."
  (ai/todo--get-context-info))

;; ============================================================================
;; Tool Definitions
;; ============================================================================

(defvar ai/todo-write-tool
  (gptel-make-tool
   :name "write_todo"
   :function #'ai/todo-write
   :category "productivity"
   :include t
   :args '((:name "title"
            :type string
            :description "The todo item title/heading text. Keep it concise and descriptive. Do NOT include temporal information like 'tonight', 'tomorrow', 'urgent' in the title - use scheduled/deadline fields instead.")
           (:name "todo_file"
            :type string
            :optional t
            :description "Path to the todo org file. Only use this when the user tells you the file path. Defaults to user's selected file")
           (:name "tags"
            :type string
            :optional t
            :description "Space-separated tags to add to the todo item. Use ONLY meaningful category tags like 'work', 'personal', 'project-name'. Do NOT use temporal tags like 'tonight', 'urgent', 'asap' - use scheduled times and priorities instead.")
           (:name "priority"
            :type string
            :optional t
            :enum ["A" "B" "C"]
            :description "Priority level: A (highest), B (medium), or C (lowest)")
           (:name "deadline"
            :type string
            :optional t
            :description "Deadline datetime in format YYYY-MM-DD or YYYY-MM-DD HH:MM (24-hour time). Example: '2025-10-28 23:59' for deadline at 11:59 PM")
           (:name "scheduled"
            :type string
            :optional t
            :description "Scheduled datetime in format YYYY-MM-DD or YYYY-MM-DD HH:MM (24-hour time). This is when the user should START or be REMINDED about the task. Examples: '2025-10-28 21:00' for 9 PM tonight, '2025-10-30 07:00' for 7 AM on the 30th."))
   :description "Create a new todo item in an org file.
Writes a new TODO heading with optional tags, priority, deadline, and scheduled date/time.

CRITICAL FORMATTING RULES:
1. Title: Keep concise, descriptive, NO temporal words (tonight, urgent, asap, etc.)
2. Tags: Use ONLY categorical tags (work, personal, project-name). NO temporal tags.
3. Scheduled: Use datetime WITH TIME for specific reminders (YYYY-MM-DD HH:MM)
4. Priority: Use A/B/C to indicate urgency instead of words like 'urgent'

EXAMPLES OF CORRECT USAGE:
- Title: 'Finish AI todo system', tags: 'ai development', scheduled: '2025-10-28 21:00', priority: 'A'
- Title: 'Work on Prolog', tags: 'prolog programming', scheduled: '2025-10-28 22:00'
- Title: 'Start work schedule 7-2pm', tags: 'work schedule', scheduled: '2025-10-30 07:00', priority: 'A'

Creates the todo file if it doesn't exist. Returns confirmation of the created todo."))

(defvar ai/todo-list-tool
  (gptel-make-tool
   :name "list_todos"
   :function #'ai/todo-list
   :category "productivity"
   :include t
   :args '((:name "todo_file"
            :type string
            :optional t
            :description "Path to the todo org file. Only use this when the user tells you the file path. Defaults to user's selected file")
           (:name "query"
            :type string
            :optional t
            :description "org-ql query as a string (e.g., '(todo)' for all todos, '(and (todo) (tags \"work\"))' for work todos, '(deadline :to today)' for items due today). Defaults to listing all todos."))
   :description "List and query todo items from an org file using org-ql.
Returns a formatted list of matching todo items with their status, tags, priority, and dates.
Supports powerful org-ql queries to filter todos by status, tags, dates, priorities, etc."))

(defvar ai/todo-clock-in-tool
  (gptel-make-tool
   :name "clock_in_todo"
   :function #'ai/todo-clock-in
   :category "productivity"
   :include t
   :args '((:name "todo_title"
            :type string
            :description "The title of the todo item to clock in to")
           (:name "todo_file"
            :type string
            :optional t
            :description "Path to the todo org file. Only use this when the user tells you the file path. Defaults to user's selected file"))
   :description "Clock in to a specific todo item by its title.
Finds the todo item in the org file and starts org-mode time tracking for that task.
Returns confirmation when successfully clocked in."))

(defvar ai/todo-clock-out-tool
  (gptel-make-tool
   :name "clock_out_todo"
   :function #'ai/todo-clock-out
   :category "productivity"
   :include t
   :args nil
   :description "Clock out from the currently active org-mode clock.
Stops time tracking for the current task and records the time spent.
Returns confirmation with the clock-out time."))

(defvar ai/todo-update-state-tool
  (gptel-make-tool
   :name "update_todo_state"
   :function #'ai/todo-update-state
   :category "productivity"
   :include t
   :args '((:name "todo_title"
            :type string
            :description "The title of the todo item to update")
           (:name "new_state"
            :type string
            :enum ["TODO" "NEXT" "DONE" "CANCELED" "WAITING" "SOMEDAY"]
            :description "The new todo state")
           (:name "todo_file"
            :type string
            :optional t
            :description "Path to the todo org file. Only use this when the user tells you the file path. Defaults to user's selected file"))
   :description "Update the state of a todo item by finding its title and changing the TODO keyword.
States: TODO (not started), NEXT (ready to work on), WAITING (blocked/waiting), SOMEDAY (maybe later), DONE (completed), CANCELED (won't do).
Returns confirmation of the state change."))

(defvar ai/todo-get-context-tool
  (gptel-make-tool
   :name "get_todo_context"
   :function #'ai/todo-get-context
   :category "productivity"
   :include t
   :args nil
   :description "Get current date, time, and clock status.
Returns the current date/time, todo file location, and information about any currently clocked-in task.
Call this at the start of conversations to understand the current context."))

(defvar ai/todo-tools nil
  "Tool names used by todo-agent preset.")

(setq ai/todo-tools
      '("write_todo"
        "list_todos"
        "clock_in_todo"
        "clock_out_todo"
        "get_todo_context"
        "read_todo_heading"
        "read_todo_numbered"
        "get_todo_lines"
        "preview_todo_changes"
        "apply_todo_line_changes"
        "todo_search_replace"
        "apply_todo_patch"))

;; ============================================================================
;; System Prompt
;; ============================================================================

(defvar ai/todo-system-prompt
  "You are a helpful AI assistant for managing org-mode todos.

CRITICAL ORG-MODE FORMATTING RULES:
1. ALWAYS call get_todo_context() FIRST to know the current date/time, always account for existing task and work around it.
2. Use proper org-mode TODO states: TODO, NEXT, WAITING, SOMEDAY, DONE, CANCELED
   - TODO: Tasks not yet started
   - NEXT: Tasks ready to work on immediately
   - WAITING: Tasks blocked or waiting on something
   - SOMEDAY: Tasks to do eventually
   - DONE: Completed tasks
   - CANCELED: Tasks that won't be done

3. TITLE FORMATTING:
   - Keep titles concise and descriptive
   - NEVER include temporal information in titles (NO 'tonight', 'tomorrow', 'urgent', 'ASAP', etc.)
   - NEVER include time information in titles (NO '7-2pm', '9pm', etc.)
   - Good: 'Finish AI todo system', 'Work on Prolog', 'Review code'
   - Bad: 'Finish AI todo system - TONIGHT', 'Work on Prolog URGENT', 'Review code ASAP'

4. TAG USAGE:
   - Use ONLY categorical/topic tags: 'work', 'personal', 'project-name', 'coding', 'meeting'
   - NEVER use temporal tags: NO 'tonight', 'urgent', 'asap', 'tomorrow', 'today'
   - NEVER use time-based tags: NO 'morning', 'evening', '7pm'
   - Good tags: 'ai_development', 'prolog_programming', 'work_schedule'
   - Bad tags: 'tonight', 'urgent', 'asap', '7-2pm' '-' 'work-schedule'


5. DATETIME FORMATTING:
   - ALWAYS include TIME when the user specifies or implies a specific time
   - Format: YYYY-MM-DD HH:MM (24-hour time)
   - SCHEDULED: When to START or be REMINDED
   - DEADLINE: When it must be DONE BY
   - Examples:
     * 'tonight at 9pm' → scheduled: '2025-10-28 21:00'
     * 'tomorrow morning at 7am' → scheduled: '2025-10-29 07:00'
     * 'by Friday 5pm' → deadline: '2025-11-01 17:00'
     * '7-2pm shift' → scheduled: '2025-10-30 07:00'

6. PRIORITY USAGE:
   - Use [#A], [#B], [#C] for urgency instead of words
   - A = High priority / urgent
   - B = Medium priority
   - C = Low priority

CORRECT EXAMPLES:
User says: \"Add todo: Finish AI system tonight at 9pm, high priority, tag it ai and development\"
You create:
  title: 'Finish AI todo system'
  tags: 'ai development'
  scheduled: '2025-10-28 21:00'
  priority: 'A'

User says: \"Remind me to start work at 7am on Wednesday\"
You create:
  title: 'Start work'
  tags: 'work'
  scheduled: '2025-10-30 07:00'
  priority: 'A'

User says: \"I need to finish the report by Friday evening, it's urgent\"
You create:
  title: 'Finish report'
  tags: 'work'
  deadline: '2025-11-01 18:00'
  priority: 'A'


User says: \"I need to finish the programming task by Friday evening, it's urgent, i need to add module loading its not so urgent right now \"
You create:
  title: 'Finish Module loading'
  tags: 'work' 'programming' 'module_loading'
  deadline: '2025-11-01 18:00'
  priority: 'B'


INCORRECT EXAMPLES (DO NOT DO THIS):
❌ title: 'Finish AI system - TONIGHT', tags: 'ai development tonight urgent'
❌ title: 'Start work 7-2pm', tags: 'work morning 7am'
❌ title: 'Report URGENT', scheduled: '2025-10-28' (missing time)

Remember: ALL temporal information goes in scheduled/deadline fields WITH TIMES, NOT in titles or tags!

You have access to tools for creating, listing, editing todos, and managing time tracking.
For edits, prefer: read_todo_numbered -> preview_todo_changes -> apply_todo_line_changes.
Use todo_search_replace for scoped text updates inside a todo heading.
Use apply_todo_patch only for complex multiline changes.
Be sure to list the current open todos before creating new ones, If one exists, edit it instead")




;; ============================================================================
;; Preset Configuration (OPTIONAL - functions set up tools directly)
;; ============================================================================

;; This preset can be used if you want to use gptel manually with these tools
;; The functions above (quick-add, chat, process) set up tools automatically
(gptel-make-preset 'todo-agent
  :description "Manage todos, clock in and out, with date/time awareness"
  :backend (ai/llm-openrouter-backend :stream t :name "OpenRouter")
  :model (ai/llm-resolve-model)
  :system ai/todo-system-prompt
  :stream t
  :tools ai/todo-tools
  :temperature 1.0
  :max-tokens nil
  :use-context 'system
  :track-media nil
  :include-reasoning t)

;; ============================================================================
;; Main Interface Functions
;; ============================================================================

;;;###autoload
(defun ai/todo-quick-add (input)
  "Quick add todos using natural language INPUT.
Creates todos, sets priorities, schedules, and deadlines based on the input.
Example: 'Add todo: write report by Friday, high priority'"
  (interactive "sQuick add todo: ")
  (let* ((gptel-default-preset 'todo-agent)
         (backend (ai/llm-openrouter-backend :stream nil :name "OpenRouter"))
         (gptel-backend backend)
         (gptel-model (ai/llm-resolve-model)))
    (message "Processing: %s" input)
    (gptel-request
        (format "%s\n\nPlease execute the necessary tool calls to complete this request. After calling tools, confirm what was created." input)
      :system ai/todo-system-prompt
      :tools ai/todo-tools
      :callback (lambda (response info)
                  (if response
                      (message "✓ Done: %s" response)
                    (message "Error: %s" (plist-get info :status))))))
  )

;;;###autoload
(defun ai/todo-chat ()
  "Start an interactive chat session with the AI todo assistant.
Use this for complex todo management, planning, and time tracking."
  (interactive)
  (let* ((buffer-name "*AI Todo Chat*")
         (buffer (get-buffer-create buffer-name))
         (backend (ai/llm-openrouter-backend :stream t :name "OpenRouter")))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (goto-char (point-max))
      (unless (bobp)
        (insert "\n\n"))
      (insert "* AI Todo Session - " (format-time-string "%Y-%m-%d %H:%M") "\n\n")
      (insert "Hello! I'm your AI todo assistant. I can help you:\n")
      (insert "- Create and manage todos\n")
      (insert "- Set priorities, deadlines, and schedules\n")
      (insert "- Track time with clock in/out\n")
      (insert "- Create reminders for events\n")
      (insert "- Update task states (TODO, NEXT, WAITING, DONE, etc.)\n\n")
      (insert "What would you like to do?\n\n")
      ;; Set up gptel with tools
      (setq-local gptel-backend backend)
      (setq-local gptel-model (ai/llm-resolve-model))
      (setq-local gptel--system-message ai/todo-system-prompt)
      (setq-local gptel-tools ai/todo-tools))
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (gptel-mode 1)
    (message "AI Todo Chat started. Type your message and press C-c RET to send.")))

;;;###autoload
(defun ai/todo-process-with-ai (input)
  "Process INPUT with AI and execute the resulting todo operations.
This is a synchronous version that waits for the response."
  (interactive "sWhat do you want to do? ")
  (let* ((backend (ai/llm-openrouter-backend :stream nil :name "OpenRouter"))
         (gptel-backend backend)
         (gptel-model (ai/llm-resolve-model))
         (response-received nil)
         (response-text nil))
    (message "Processing: %s" input)
    (gptel-request
        (format "%s\n\nPlease execute the necessary tool calls to complete this request. After calling tools, confirm what was created." input)
      :system ai/todo-system-prompt
      :tools ai/todo-tools
      :callback
      (lambda (response info)
        (setq response-text response)
        (setq response-received t)))
    ;; Wait for response (with timeout)
    (let ((max-wait 60)
          (waited 0))
      (while (and (not response-received) (< waited max-wait))
        (sleep-for 0.1)
        (setq waited (+ waited 0.1))))
    (if response-text
        (progn
          (message "✓ Done: %s" response-text)
          response-text)
      (message "No response received from AI")
      nil)))

;; ============================================================================
;; Utility Functions
;; ============================================================================

;;;###autoload
(defun ai/todo-show-file ()
  "Open the todo file in a buffer."
  (interactive)
  (find-file ai/todo-file))

;;;###autoload
(defun ai/todo-show-agenda ()
  "Show org-agenda for the todo file."
  (interactive)
  (let ((org-agenda-files (list ai/todo-file)))
    (org-agenda nil "a")))

(provide 'ai-todo)
