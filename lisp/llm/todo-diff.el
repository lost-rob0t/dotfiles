;;; todo-diff.el --- Transactional Org subtree editing tools -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)
(require 'json)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup ai/todo-edit nil
  "Safe exact-match and diff-based editing for Org todo subtrees."
  :group 'ai/todo
  :prefix "ai/todo-edit-")

(defun ai/todo-edit--object (&rest pairs)
  "Return an alist from alternating string keys and values in PAIRS."
  (let (result)
    (while pairs
      (push (cons (pop pairs) (pop pairs)) result))
    (nreverse result)))

(defun ai/todo-edit--json (&rest pairs)
  "Serialize alternating string keys and values in PAIRS as JSON."
  (json-serialize (apply #'ai/todo-edit--object pairs)
                  :null-object nil :false-object :json-false))

(defun ai/todo-edit--arg (object key &optional default)
  "Read KEY from plist or alist OBJECT."
  (let* ((name (substring (symbol-name key) 1))
         (symbol (intern name)))
    (cond
     ((and (listp object) (keywordp (car object)))
      (if (plist-member object key) (plist-get object key) default))
     ((listp object)
      (let ((cell (or (assoc key object) (assoc symbol object) (assoc name object))))
        (if cell (cdr cell) default)))
     (t default))))

(defun ai/todo-edit--resolve-file (&optional file)
  "Resolve FILE or `ai/todo-file' to an absolute path."
  (let ((candidate (or file (and (boundp 'ai/todo-file) ai/todo-file))))
    (unless candidate (error "No todo file configured"))
    (expand-file-name candidate)))

(defun ai/todo-edit--heading-markers (title file)
  "Return markers for exact heading TITLE in FILE."
  (with-current-buffer (find-file-noselect file)
    (unless (derived-mode-p 'org-mode) (org-mode))
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (markers)
       (org-map-entries
        (lambda ()
          (when (string= title (org-get-heading t t t t))
            (push (copy-marker (line-beginning-position)) markers)))
        nil 'file)
       (nreverse markers)))))

(defun ai/todo-edit--heading-marker (title &optional file)
  "Return the unique marker for exact heading TITLE in FILE."
  (let* ((file (ai/todo-edit--resolve-file file))
         (markers (ai/todo-edit--heading-markers title file)))
    (pcase (length markers)
      (0 (error "Todo heading not found: %s" title))
      (1 (car markers))
      (_ (error "Todo heading is ambiguous (%d exact matches): %s"
                (length markers) title)))))

(defun ai/todo-edit--bounds (marker)
  "Return subtree bounds for MARKER as a cons cell."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (cons (line-beginning-position)
           (save-excursion
             (org-end-of-subtree t t)
             (point))))))

(defun ai/todo-edit--subtree (marker)
  "Return MARKER's complete subtree text."
  (with-current-buffer (marker-buffer marker)
    (pcase-let ((`(,start . ,end) (ai/todo-edit--bounds marker)))
      (buffer-substring-no-properties start end))))

(defun ai/todo-edit--count (needle haystack)
  "Count non-overlapping exact NEEDLE occurrences in HAYSTACK."
  (when (string-empty-p needle) (error "old_text must not be empty"))
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun ai/todo-edit--replace (content old-text new-text replace-all)
  "Replace exact OLD-TEXT with NEW-TEXT in CONTENT."
  (let ((count (ai/todo-edit--count old-text content)))
    (cond
     ((zerop count) (error "old_text was not found in the todo subtree"))
     ((and (> count 1) (not replace-all))
      (error "old_text matched %d times; include more context or set replace_all" count))
     (replace-all
      (cons (replace-regexp-in-string (regexp-quote old-text) new-text content t t)
            count))
     (t
      (let ((position (string-match (regexp-quote old-text) content)))
        (cons (concat (substring content 0 position)
                      new-text
                      (substring content (+ position (length old-text))))
              1))))))

(defun ai/todo-edit--diff (title old-content new-content)
  "Return unified diff between OLD-CONTENT and NEW-CONTENT for TITLE."
  (let ((old-file (make-temp-file "todo-old-"))
        (new-file (make-temp-file "todo-new-")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old-content))
          (with-temp-file new-file (insert new-content))
          (with-temp-buffer
            (let ((status (call-process "diff" nil t nil "-u"
                                        "--label" (format "a/todo:%s" title)
                                        "--label" (format "b/todo:%s" title)
                                        old-file new-file)))
              (unless (memq status '(0 1))
                (error "diff failed: %s" (string-trim (buffer-string))))
              (buffer-string))))
      (delete-file old-file)
      (delete-file new-file))))

(defun ai/todo-edit--replace-subtree (marker content)
  "Replace MARKER's subtree with CONTENT as one undoable operation."
  (with-current-buffer (marker-buffer marker)
    (pcase-let ((`(,start . ,end) (ai/todo-edit--bounds marker)))
      (atomic-change-group
        (goto-char start)
        (delete-region start end)
        (insert content))
      (save-buffer))))

(defun ai/todo-read-heading (title &optional file numbered)
  "Read exact todo heading TITLE from FILE.
When NUMBERED is non-nil, return absolute line numbers."
  (let* ((marker (ai/todo-edit--heading-marker title file))
         (content (ai/todo-edit--subtree marker)))
    (if numbered
        (with-current-buffer (marker-buffer marker)
          (let* ((start-line (line-number-at-pos marker))
                 (lines (split-string content "\n" nil))
                 (line start-line))
            (ai/todo-edit--json
             "ok" t "title" title "start_line" start-line
             "content"
             (mapconcat (lambda (text)
                          (prog1 (format "%6d\t%s" line text)
                            (setq line (1+ line))))
                        lines "\n"))))
      (ai/todo-edit--json "ok" t "title" title "content" content))))

(defun ai/todo-edit-exact (title old-text new-text &optional file replace-all preview)
  "Replace exact OLD-TEXT with NEW-TEXT inside todo TITLE."
  (let* ((marker (ai/todo-edit--heading-marker title file))
         (original (ai/todo-edit--subtree marker))
         (replacement (ai/todo-edit--replace original old-text new-text replace-all))
         (updated (car replacement))
         (diff (ai/todo-edit--diff title original updated)))
    (unless preview (ai/todo-edit--replace-subtree marker updated))
    (ai/todo-edit--json "ok" t "title" title
                        "replacements" (cdr replacement)
                        "preview" (if preview t :json-false)
                        "diff" diff)))

(defun ai/todo-multi-edit (title edits &optional file preview)
  "Apply sequential exact-match EDITS to todo TITLE transactionally."
  (let* ((marker (ai/todo-edit--heading-marker title file))
         (original (ai/todo-edit--subtree marker))
         (updated original)
         (replacements 0))
    (dolist (edit edits)
      (let* ((old-text (ai/todo-edit--arg edit :old_text))
             (new-text (ai/todo-edit--arg edit :new_text ""))
             (replace-all (ai/todo-edit--arg edit :replace_all nil))
             (result (ai/todo-edit--replace updated old-text new-text replace-all)))
        (setq updated (car result)
              replacements (+ replacements (cdr result)))))
    (let ((diff (ai/todo-edit--diff title original updated)))
      (unless preview (ai/todo-edit--replace-subtree marker updated))
      (ai/todo-edit--json "ok" t "title" title
                          "edits" (length edits)
                          "replacements" replacements
                          "preview" (if preview t :json-false)
                          "diff" diff))))

(defun ai/todo-replace-subtree (title content &optional file preview)
  "Replace todo TITLE's complete subtree with CONTENT."
  (let* ((marker (ai/todo-edit--heading-marker title file))
         (original (ai/todo-edit--subtree marker))
         (diff (ai/todo-edit--diff title original content)))
    (unless preview (ai/todo-edit--replace-subtree marker content))
    (ai/todo-edit--json "ok" t "title" title
                        "preview" (if preview t :json-false)
                        "diff" diff)))

(defun ai/todo-edit--line-updates (title updates &optional file preview)
  "Compatibility line editor for todo TITLE using absolute line UPDATES."
  (let* ((marker (ai/todo-edit--heading-marker title file))
         (original (ai/todo-edit--subtree marker))
         (start-line (with-current-buffer (marker-buffer marker)
                       (line-number-at-pos marker)))
         (trailing-newline (string-suffix-p "\n" original))
         (lines (vconcat (split-string original "\n" nil)))
         (count (length lines)))
    (when (and trailing-newline (> count 0) (string-empty-p (aref lines (1- count))))
      (setq lines (seq-subseq lines 0 (1- count))
            count (1- count)))
    (dolist (update updates)
      (let* ((absolute-line (ai/todo-edit--arg update :line))
             (content (ai/todo-edit--arg update :content))
             (index (- absolute-line start-line)))
        (unless (and (integerp absolute-line) (<= 0 index) (< index count))
          (error "Line %S is outside todo subtree lines %d..%d"
                 absolute-line start-line (+ start-line count -1)))
        (aset lines index content)))
    (let ((updated (concat (string-join (append lines nil) "\n")
                           (if trailing-newline "\n" ""))))
      (if preview
          (ai/todo-edit--json "ok" t "title" title "preview" t
                              "diff" (ai/todo-edit--diff title original updated))
        (ai/todo-edit--replace-subtree marker updated)
        (ai/todo-edit--json "ok" t "title" title
                            "changes" (length updates)
                            "diff" (ai/todo-edit--diff title original updated))))))

(defun ai/todo-apply-line-changes (title updates &optional file)
  "Compatibility wrapper for absolute line updates. Prefer `todo_multi_edit'."
  (ai/todo-edit--line-updates title updates file nil))

(defun ai/todo-preview-line-changes (title updates &optional file)
  "Preview compatibility absolute line updates."
  (ai/todo-edit--line-updates title updates file t))

(defun ai/todo-search-replace (title search replace &optional file)
  "Replace every exact SEARCH occurrence with REPLACE in todo TITLE."
  (ai/todo-edit-exact title search replace file t nil))

(defun ai/todo-get-lines (title start-line end-line &optional file)
  "Return absolute START-LINE through END-LINE from todo TITLE."
  (let* ((marker (ai/todo-edit--heading-marker title file))
         (bounds (ai/todo-edit--bounds marker)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (car bounds))
        (let ((heading-start (line-number-at-pos)))
          (unless (and (<= heading-start start-line end-line)
                       (<= end-line (line-number-at-pos (cdr bounds))))
            (error "Requested lines are outside the todo subtree"))
          (forward-line (- start-line heading-start))
          (let ((start (line-beginning-position)))
            (forward-line (1+ (- end-line start-line)))
            (ai/todo-edit--json
             "ok" t "title" title "start_line" start-line "end_line" end-line
             "content" (buffer-substring-no-properties start (min (point) (cdr bounds))))))))))

(defun ai/todo-apply-patch (title patch &optional file preview)
  "Apply unified PATCH to todo TITLE using isolated temporary files.
The patch is never run against the actual Org file."
  (unless (executable-find "patch")
    (error "The patch executable is required"))
  (let* ((marker (ai/todo-edit--heading-marker title file))
         (original (ai/todo-edit--subtree marker))
         (input (make-temp-file "todo-patch-input-"))
         (patch-file (make-temp-file "todo-patch-"))
         (output (make-temp-file "todo-patch-output-"))
         (log (generate-new-buffer " *todo-patch-log*")))
    (unwind-protect
        (progn
          (delete-file output)
          (with-temp-file input (insert original))
          (with-temp-file patch-file (insert patch))
          (let ((status (process-file "patch" nil log nil
                                      "--batch" "--forward" "--silent"
                                      "-o" output input patch-file)))
            (unless (zerop status)
              (error "Patch failed: %s"
                     (with-current-buffer log (string-trim (buffer-string)))))
            (let* ((updated (with-temp-buffer
                              (insert-file-contents output)
                              (buffer-string)))
                   (diff (ai/todo-edit--diff title original updated)))
              (unless preview (ai/todo-edit--replace-subtree marker updated))
              (ai/todo-edit--json "ok" t "title" title
                                  "preview" (if preview t :json-false)
                                  "diff" diff))))
      (dolist (temporary (list input patch-file output))
        (when (file-exists-p temporary) (delete-file temporary)))
      (kill-buffer log))))

(defun ai/todo-edit--register (name function description args &optional confirm)
  "Register todo tool NAME using FUNCTION, DESCRIPTION, and ARGS."
  (when (fboundp 'gptel-get-tool)
    (ignore-errors (setf (gptel-get-tool name) nil)))
  (apply #'gptel-make-tool
         (append (list :name name :function function
                       :category "todo_editing" :description description
                       :args args)
                 (when confirm (list :confirm t)))))

(ai/todo-edit--register
 "todo_read" #'ai/todo-read-heading
 "Read one exact todo heading and its subtree."
 '((:name "title" :type string :description "Exact heading title")
   (:name "file" :type string :optional t :description "Todo Org file")
   (:name "numbered" :type boolean :optional t :description "Include absolute line numbers")))

(ai/todo-edit--register
 "todo_edit" #'ai/todo-edit-exact
 "Replace exact text inside one todo subtree and return a unified diff."
 '((:name "title" :type string :description "Exact heading title")
   (:name "old_text" :type string :description "Exact text with enough context to be unique")
   (:name "new_text" :type string :description "Replacement text")
   (:name "file" :type string :optional t :description "Todo Org file")
   (:name "replace_all" :type boolean :optional t :description "Replace every exact match")
   (:name "preview" :type boolean :optional t :description "Return diff without writing")) t)

(ai/todo-edit--register
 "todo_multi_edit" #'ai/todo-multi-edit
 "Apply multiple exact edits to one todo subtree as one transaction."
 '((:name "title" :type string :description "Exact heading title")
   (:name "edits" :type array
          :items (:type object
                  :properties (:old_text (:type string :description "Exact text to replace")
                               :new_text (:type string :description "Replacement text")
                               :replace_all (:type boolean :description "Replace all exact matches"))
                  :required ["old_text" "new_text"]
                  :additionalProperties :json-false)
          :description "Ordered exact-match edits")
   (:name "file" :type string :optional t :description "Todo Org file")
   (:name "preview" :type boolean :optional t :description "Return diff without writing")) t)

(ai/todo-edit--register
 "todo_replace_subtree" #'ai/todo-replace-subtree
 "Replace a complete todo subtree and return a unified diff."
 '((:name "title" :type string :description "Exact heading title")
   (:name "content" :type string :description "Complete replacement subtree")
   (:name "file" :type string :optional t :description "Todo Org file")
   (:name "preview" :type boolean :optional t :description "Return diff without writing")) t)

(ai/todo-edit--register
 "apply_todo_line_changes" #'ai/todo-apply-line-changes
 "Compatibility line-number editor. Prefer todo_multi_edit."
 '((:name "title" :type string :description "Exact heading title")
   (:name "updates" :type array
          :items (:type object
                  :properties (:line (:type integer :description "Absolute line number")
                               :content (:type string :description "Complete replacement line"))
                  :required ["line" "content"]
                  :additionalProperties :json-false)
          :description "Absolute line replacements")
   (:name "file" :type string :optional t :description "Todo Org file")) t)

(ai/todo-edit--register
 "preview_todo_changes" #'ai/todo-preview-line-changes
 "Preview compatibility line-number changes."
 '((:name "title" :type string :description "Exact heading title")
   (:name "updates" :type array
          :items (:type object
                  :properties (:line (:type integer :description "Absolute line number")
                               :content (:type string :description "Complete replacement line"))
                  :required ["line" "content"]
                  :additionalProperties :json-false)
          :description "Absolute line replacements")
   (:name "file" :type string :optional t :description "Todo Org file")))

(ai/todo-edit--register
 "todo_search_replace" #'ai/todo-search-replace
 "Replace every exact string occurrence within one todo subtree."
 '((:name "title" :type string :description "Exact heading title")
   (:name "search" :type string :description "Exact text to replace")
   (:name "replace" :type string :description "Replacement text")
   (:name "file" :type string :optional t :description "Todo Org file")) t)

(ai/todo-edit--register
 "get_todo_lines" #'ai/todo-get-lines
 "Read an inclusive absolute line range within one todo subtree."
 '((:name "title" :type string :description "Exact heading title")
   (:name "start_line" :type integer :description "First absolute line")
   (:name "end_line" :type integer :description "Last absolute line")
   (:name "file" :type string :optional t :description "Todo Org file")))

(ai/todo-edit--register
 "apply_todo_patch" #'ai/todo-apply-patch
 "Apply a unified patch to an isolated copy of a todo subtree, then replace the subtree atomically."
 '((:name "title" :type string :description "Exact heading title")
   (:name "patch" :type string :description "Unified patch generated against the subtree")
   (:name "file" :type string :optional t :description "Todo Org file")
   (:name "preview" :type boolean :optional t :description "Return diff without writing")) t)

(ai/todo-edit--register
 "read_todo_numbered" (lambda (title &optional file)
                         (ai/todo-read-heading title file t))
 "Read one exact todo subtree with absolute line numbers."
 '((:name "title" :type string :description "Exact heading title")
   (:name "file" :type string :optional t :description "Todo Org file")))

(ai/todo-edit--register
 "read_todo_heading" #'ai/todo-read-heading
 "Read one exact todo subtree."
 '((:name "title" :type string :description "Exact heading title")
   (:name "file" :type string :optional t :description "Todo Org file")))

(defvar ai/todo-edit-tools
  '("todo_read" "todo_edit" "todo_multi_edit" "todo_replace_subtree"
    "apply_todo_line_changes" "preview_todo_changes" "todo_search_replace"
    "get_todo_lines" "apply_todo_patch" "read_todo_numbered"
    "read_todo_heading")
  "Tool names for safe Org todo editing.")

(provide 'todo-diff)
;;; todo-diff.el ends here
