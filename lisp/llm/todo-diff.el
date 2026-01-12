;;; todo-diff.el --- Todo update tools mirroring diff operations -*- lexical-binding: t; -*-

;; Author: nsaspy
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (gptel "0.9") (org "9.0"))

;;; Commentary:
;; Todo update tools that mirror the diff/patch operations from agent.el
;; These tools allow precise updates to todo items similar to how diffs update files.

;;; Code:

(require 'org)
(require 'org-element)
(require 'gptel)

;;; Helper Functions

(defun ai/todo--find-heading-by-title (title &optional file)
  "Find heading with TITLE in FILE (defaults to `ai/todo-file').
Returns marker to the heading or nil if not found."
  (let ((file (or file ai/todo-file)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (format "^\\*+ .*%s" (regexp-quote title)) nil t)
            (copy-marker (line-beginning-position))))))))

(defun ai/todo--get-heading-lines (title &optional file show-numbers)
  "Get lines of heading with TITLE from FILE.
If SHOW-NUMBERS is non-nil, prepend line numbers.
Returns cons cell (start-line . lines-string)."
  (let* ((file (or file ai/todo-file))
         (marker (ai/todo--find-heading-by-title title file)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (let* ((start-pos (point))
                 (start-line (line-number-at-pos))
                 (end-pos (save-excursion
                            (org-end-of-subtree t t)
                            (point)))
                 (content (buffer-substring-no-properties start-pos end-pos))
                 (lines (split-string content "\n" nil)))
            (cons start-line
                  (if show-numbers
                      (mapconcat
                       (lambda (line)
                         (prog1
                             (format "%d: %s" start-line line)
                           (setq start-line (1+ start-line))))
                       lines "\n")
                    content))))))))

(defun ai/todo--apply-line-updates (title updates &optional file)
  "Apply line UPDATES to heading with TITLE in FILE.
UPDATES is a list of (line-number . new-content) pairs."
  (let* ((file (or file ai/todo-file))
         (marker (ai/todo--find-heading-by-title title file)))
    (unless marker
      (error "Todo heading not found: %s" title))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((start-line (line-number-at-pos))
               (end-pos (save-excursion
                          (org-end-of-subtree t t)
                          (point)))
               (changes-made 0))
          (dolist (update updates)
            (let* ((target-line (car update))
                   (new-content (cdr update))
                   (relative-line (- target-line start-line)))
              (when (>= relative-line 0)
                (goto-char marker)
                (forward-line relative-line)
                (when (< (point) end-pos)
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert new-content)
                  (setq changes-made (1+ changes-made))))))
          (save-buffer)
          (format "Updated %d line(s) in todo '%s'" changes-made title))))))

(defun ai/todo--search-replace-in-heading (title search replace &optional file)
  "Search for SEARCH and replace with REPLACE in heading TITLE in FILE."
  (let* ((file (or file ai/todo-file))
         (marker (ai/todo--find-heading-by-title title file)))
    (unless marker
      (error "Todo heading not found: %s" title))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((start-pos (point))
               (end-pos (save-excursion
                          (org-end-of-subtree t t)
                          (point)))
               (count 0))
          (while (search-forward search end-pos t)
            (replace-match replace nil t)
            (setq count (1+ count)))
          (when (> count 0)
            (save-buffer))
          (format "Replaced %d occurrence(s) of '%s' with '%s' in todo '%s'"
                  count search replace title))))))

(defun ai/todo--preview-line-changes (title updates &optional file)
  "Preview line UPDATES to heading with TITLE without applying.
Returns a diff-style preview string."
  (let* ((file (or file ai/todo-file))
         (heading-data (ai/todo--get-heading-lines title file t)))
    (unless heading-data
      (error "Todo heading not found: %s" title))
    (let* ((start-line (car heading-data))
           (current-content (cdr heading-data))
           (lines (split-string current-content "\n" t))
           (preview-parts '()))
      (push (format "Preview changes to: %s" title) preview-parts)
      (push "" preview-parts)
      (dolist (update updates)
        (let* ((line-num (car update))
               (new-content (cdr update))
               (relative-line (- line-num start-line)))
          (when (and (>= relative-line 0) (< relative-line (length lines)))
            (let ((old-content (nth relative-line lines)))
              (push (format "Line %d:" line-num) preview-parts)
              (push (format "- %s" (string-trim old-content)) preview-parts)
              (push (format "+ %s" new-content) preview-parts)
              (push "" preview-parts)))))
      (string-join (nreverse preview-parts) "\n"))))

(defun ai/todo--get-lines-range (title start-line end-line &optional file)
  "Get lines START-LINE through END-LINE from heading TITLE in FILE."
  (let* ((file (or file ai/todo-file))
         (heading-data (ai/todo--get-heading-lines title file nil))
         (marker (ai/todo--find-heading-by-title title file)))
    (unless marker
      (error "Todo heading not found: %s" title))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((heading-start-line (line-number-at-pos))
               (relative-start (- start-line heading-start-line))
               (relative-end (- end-line heading-start-line)))
          (forward-line relative-start)
          (let ((start-pos (line-beginning-position)))
            (forward-line (- relative-end relative-start))
            (buffer-substring-no-properties start-pos (line-end-position))))))))

(defun ai/todo--apply-structured-patch (title patch &optional file)
  "Apply structured PATCH to heading TITLE in FILE.
PATCH is a string in unified diff format or structured update format."
  (let* ((file (or file ai/todo-file))
         (marker (ai/todo--find-heading-by-title title file)))
    (unless marker
      (error "Todo heading not found: %s" title))
    (with-temp-file (concat file ".patch")
      (insert patch))
    (with-current-buffer (marker-buffer marker)
      (let* ((start-pos (marker-position marker))
             (end-pos (save-excursion
                        (goto-char marker)
                        (org-end-of-subtree t t)
                        (point)))
             (result (shell-command-on-region
                      start-pos end-pos
                      (format "patch -p0 - < %s.patch" file)
                      t t)))
        (save-buffer)
        (delete-file (concat file ".patch"))
        (if (zerop result)
            (format "Successfully applied patch to todo '%s'" title)
          (format "Patch application had issues: %s" result))))))

(defun ai/todo--read-heading-numbered (title &optional file)
  "Read heading TITLE from FILE with line numbers prepended."
  (let ((heading-data (ai/todo--get-heading-lines title file t)))
    (if heading-data
        (cdr heading-data)
      (error "Todo heading not found: %s" title))))

;;; Tool Definitions

(gptel-make-tool
 :function (lambda (title updates &optional file)
             (ai/todo--apply-line-updates 
              title
              (mapcar (lambda (u) (cons (plist-get u :line) (plist-get u :content)))
                      updates)
              file))
 :name "apply_todo_line_changes"
 :category "todo_editing"
 :args '((:name "title" :type string
          :description "Title of the todo heading to update")
         (:name "updates" :type array
          :description "Array of updates, each with :line (number) and :content (string)")
         (:name "file" :type string :optional t
          :description "Todo file path (defaults to ai/todo-file)"))
 :description "Apply line-by-line changes to a specific todo heading.
Similar to apply_line_changes but operates on todo headings instead of files.
Updates are applied by line number within the heading's subtree.")

(gptel-make-tool
 :function #'ai/todo--search-replace-in-heading
 :name "todo_search_replace"
 :category "todo_editing"
 :args '((:name "title" :type string
          :description "Title of the todo heading")
         (:name "search" :type string
          :description "Text to search for")
         (:name "replace" :type string
          :description "Replacement text")
         (:name "file" :type string :optional t
          :description "Todo file path (defaults to ai/todo-file)"))
 :description "Search and replace text within a specific todo heading.
Similar to search_replace but operates on todo headings.
Replaces all occurrences of search string with replacement within the heading's subtree.")

(gptel-make-tool
 :function (lambda (title updates &optional file)
             (ai/todo--preview-line-changes
              title
              (mapcar (lambda (u) (cons (plist-get u :line) (plist-get u :content)))
                      updates)
              file))
 :name "preview_todo_changes"
 :category "todo_editing"
 :args '((:name "title" :type string
          :description "Title of the todo heading")
         (:name "updates" :type array
          :description "Array of updates to preview, each with :line and :content")
         (:name "file" :type string :optional t
          :description "Todo file path (defaults to ai/todo-file)"))
 :description "Preview line changes to a todo heading without applying them.
Shows before/after comparison of the changes that would be made.
Use this before apply_todo_line_changes to verify correctness.")

(gptel-make-tool
 :function #'ai/todo--get-lines-range
 :name "get_todo_lines"
 :category "todo_editing"
 :args '((:name "title" :type string
          :description "Title of the todo heading")
         (:name "start_line" :type integer
          :description "Starting line number (absolute in file)")
         (:name "end_line" :type integer
          :description "Ending line number (absolute in file)")
         (:name "file" :type string :optional t
          :description "Todo file path (defaults to ai/todo-file)"))
 :description "Get a range of lines from a todo heading.
Returns the text between start_line and end_line within the heading's subtree.
Useful for inspecting specific sections before making changes.")

(gptel-make-tool
 :function #'ai/todo--apply-structured-patch
 :name "apply_todo_patch"
 :category "todo_editing"
 :args '((:name "title" :type string
          :description "Title of the todo heading")
         (:name "patch" :type string
          :description "Unified diff format patch or structured update")
         (:name "file" :type string :optional t
          :description "Todo file path (defaults to ai/todo-file)"))
 :description "Apply a structured patch to a todo heading.
Accepts unified diff format patches to make complex changes to todo entries.
Use for multi-line edits that are easier to express as diffs.")

(gptel-make-tool
 :function #'ai/todo--read-heading-numbered
 :name "read_todo_numbered"
 :category "todo_editing"
 :args '((:name "title" :type string
          :description "Title of the todo heading to read")
         (:name "file" :type string :optional t
          :description "Todo file path (defaults to ai/todo-file)"))
 :description "Read a todo heading with line numbers prepended to each line.
Essential for making precise line-based edits - use this first to see line numbers,
then use apply_todo_line_changes or other tools to make targeted updates.
Line numbers are absolute file positions for accurate targeting.")

(gptel-make-tool
 :function (lambda (title &optional file)
             (let ((heading-data (ai/todo--get-heading-lines title file nil)))
               (if heading-data
                   (cdr heading-data)
                 (error "Todo heading not found: %s" title))))
 :name "read_todo_heading"
 :category "todo_editing"
 :args '((:name "title" :type string
          :description "Title of the todo heading to read")
         (:name "file" :type string :optional t
          :description "Todo file path (defaults to ai/todo-file)"))
 :description "Read the complete content of a todo heading including its subtree.
Returns all content under the heading without line numbers.
Use read_todo_numbered if you need line numbers for editing.")

;;; Extended Tool List

(defvar ai/todo-edit-tools
  (list 'apply_todo_line_changes
        'todo_search_replace
        'preview_todo_changes
        'get_todo_lines
        'apply_todo_patch
        'read_todo_numbered
        'read_todo_heading)
  "List of todo editing tool names that mirror diff operations.")

(provide 'todo-diff)
;;; todo-diff.el ends here
