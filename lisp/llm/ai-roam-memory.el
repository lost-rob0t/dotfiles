;;; ai-roam-memory.el --- Layered memory notes for the roam AI -*- lexical-binding: t; -*-

;;; Commentary:

;; Layered memory for the roam AI:
;;
;; Layer 1 - roam memory notes.  `ai/roam-memory-write' stores a
;; durable org note per subject under `ai/roam-memory-dir' (by default
;; "llm/memory/" inside the roam graph); `ai/roam-memory-read' and
;; `ai/roam-memory-list' fetch them back.  Writes are rights-gated
;; through `ai/roam-assert-full-rights' and fail closed.
;;
;; Layer 2 - structured Prolog facts.  `ai/roam-memory-assert-fact'
;; appends one `world_fact/9' line to the append-only fact file
;; `ai/roam-memory-kb-file' (by default "llm/memory/kb-facts.pl");
;; `ai/roam-memory-fact-count' counts them.  Writes are rights-gated
;; and fail closed; single quotes are escaped and newline-bearing
;; values are rejected.
;;
;; The gptel tools `remember_fact', `recall_memory', and
;; `assert_world_fact' expose both layers to the LLM.  They fail soft:
;; they return error strings (containing "rights" on gate failures)
;; instead of signaling.
;;
;; This module is Doom-free so it can be tested in batch Emacs.

;;; Code:

(require 'ai-roam)
(require 'ai-roam-links)
(require 'cl-lib)

(defgroup ai-roam-memory nil
  "Layered memory for the roam AI."
  :group 'ai-roam)

(defconst ai/roam-memory-gptel-tool-remember "remember_fact"
  "Name of the gptel tool that stores a memory note.")

(defconst ai/roam-memory-gptel-tool-recall "recall_memory"
  "Name of the gptel tool that fetches a memory note.")

(defconst ai/roam-memory-gptel-tool-assert-fact "assert_world_fact"
  "Name of the gptel tool that appends a world_fact line.")

(defcustom ai/roam-memory-dir "llm/memory/"
  "Memory note directory relative to `ai/roam-directory'.
Expanded at call time so rebinding the roam root rebinds memory."
  :type 'string
  :group 'ai-roam-memory)

(defcustom ai/roam-memory-kb-file "llm/memory/kb-facts.pl"
  "Append-only Prolog fact file relative to `ai/roam-directory'.
Expanded at call time so rebinding the roam root rebinds the KB."
  :type 'string
  :group 'ai-roam-memory)

(defun ai/roam-memory--dir ()
  "Return the expanded memory note directory."
  (file-name-as-directory
   (expand-file-name ai/roam-memory-dir (ai/roam--directory))))

(defun ai/roam-memory--kb-file ()
  "Return the expanded world-fact file path."
  (expand-file-name ai/roam-memory-kb-file (ai/roam--directory)))

(defun ai/roam-memory--slug (subject)
  "Return the file slug for SUBJECT.
Lowercased, whitespace runs collapsed to dashes, then stripped to
[a-z0-9-]."
  (let ((slug (downcase subject)))
    (setq slug (replace-regexp-in-string "[[:space:]]+" "-" slug))
    (replace-regexp-in-string "[^a-z0-9-]" "" slug)))

(defun ai/roam-memory-file (subject)
  "Return the deterministic memory note file path for SUBJECT."
  (expand-file-name
   (concat (ai/roam-memory--slug subject) ".org")
   (ai/roam-memory--dir)))

(defun ai/roam-memory-read (subject)
  "Return the full content of SUBJECT's memory note, or nil when absent."
  (let ((file (ai/roam-memory-file subject)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun ai/roam-memory-write (subject content &optional source)
  "Create or update SUBJECT's memory note with CONTENT.
SOURCE, when non-nil, is recorded under the note's Memory heading.
Requires full editor rights; fails closed with `user-error'
otherwise.  The note carries a stable org ID across rewrites.
Return the note's file path."
  (let ((file (ai/roam-memory-file subject)))
    (ai/roam-assert-full-rights file "memory write")
    (let ((id (or (ai/roam-links--top-drawer-id file)
                  (progn (require 'org-id) (org-id-new)))))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "#+TITLE: " subject "\n")
        (insert ":PROPERTIES:\n:ID: " id "\n:END:\n\n")
        (insert "* Memory\n")
        (insert (if (string-suffix-p "\n" content) content
                  (concat content "\n")))
        (when source
          (insert "- Source: " source "\n")))
      file)))

(defun ai/roam-memory-list ()
  "Return the sorted subjects that have a memory note."
  (let ((dir (ai/roam-memory--dir)))
    (when (file-directory-p dir)
      (sort (cl-loop for file in (directory-files dir nil "\\.org\\'")
                     collect (file-name-base file))
            #'string<))))

(defun ai/roam-memory--quote-prolog (value)
  "Return VALUE as a single-quoted Prolog atom with quotes doubled."
  (concat "'" (replace-regexp-in-string "'" "''" value) "'"))

(defun ai/roam-memory--reject-newlines (what value)
  "Signal unless VALUE for WHAT is newline-free."
  (when (string-match-p "[\n\r]" value)
    (user-error "Roam AI: %s must not contain newlines" what)))

(defun ai/roam-memory--uuid ()
  "Return a fresh identifier for a world fact."
  (require 'org-id nil t)
  (if (fboundp 'org-id-new)
      (org-id-new)
    (format "%s-%x"
            (format-time-string "%Y%m%dT%H%M%S")
            (random most-positive-fixnum))))

(defun ai/roam-memory-assert-fact (subject predicate object &optional source)
  "Append one world_fact line about SUBJECT to the fact file.
PREDICATE links SUBJECT to OBJECT; SOURCE, when non-nil, records
where the fact came from (nil stores the Prolog atom `none').
Requires full editor rights; fails closed with `user-error'
otherwise.  The file is append-only: values are single-quote
escaped and newline-bearing values are rejected.  Return the line
written."
  (let ((file (ai/roam-memory--kb-file)))
    (ai/roam-assert-full-rights file "world fact assertion")
    (dolist (value (list subject predicate object))
      (ai/roam-memory--reject-newlines "world fact values" value))
    (when source
      (ai/roam-memory--reject-newlines "world fact source" source))
    (let ((line (format "world_fact('%s', %s, %s, %s, %s, agent, 1, active, '%s').\n"
                        (ai/roam-memory--uuid)
                        (ai/roam-memory--quote-prolog subject)
                        (ai/roam-memory--quote-prolog predicate)
                        (ai/roam-memory--quote-prolog object)
                        (if source
                            (ai/roam-memory--quote-prolog source)
                          "none")
                        (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))
      (make-directory (file-name-directory file) t)
      (write-region line nil file 'append 'no-message)
      line)))

(defun ai/roam-memory-fact-count ()
  "Return how many world_fact lines the fact file holds."
  (let ((file (ai/roam-memory--kb-file)))
    (if (not (file-exists-p file))
        0
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (how-many "^world_fact("))))))

(defun ai/roam-memory--clear-gptel-tool (name)
  "Remove an existing gptel tool named NAME before re-registering it."
  (when (fboundp 'gptel-get-tool)
    (ignore-errors (setf (gptel-get-tool name) nil))))

(defun ai/roam-memory--tool-remember (subject content &optional source)
  "gptel tool body: remember CONTENT under SUBJECT.
Fail soft: return an error string containing \"rights\" when the
editor-rights gate refuses the write."
  (condition-case err
      (format "Remembered under %s."
              (ai/roam-memory-write subject content source))
    (user-error
     (format "ERROR: remember_fact: %s" (error-message-string err)))))

(defun ai/roam-memory--tool-recall (subject)
  "gptel tool body: return SUBJECT's memory note content.
Fail soft: an absent subject yields a plain \"No memory\" string."
  (or (ai/roam-memory-read subject)
      (format "No memory for %s" subject)))

(defun ai/roam-memory--tool-assert-fact (subject predicate object &optional source)
  "gptel tool body: append one world_fact line for the values.
Fail soft: return an error string containing \"rights\" when the
editor-rights gate refuses the write."
  (condition-case err
      (progn
        (ai/roam-memory-assert-fact subject predicate object source)
        (format "World fact recorded (%d facts)."
                (ai/roam-memory-fact-count)))
    (user-error
     (format "ERROR: assert_world_fact: %s" (error-message-string err)))))

(defun ai/roam-memory-register-gptel-tools ()
  "Register the memory note gptel tools with gptel.
Both tools are synchronous and fail soft.  Registration is
idempotent via clear-then-register."
  (unless (require 'gptel nil t)
    (user-error "Roam AI: gptel is required for roam memory tools"))
  (ai/roam-memory--clear-gptel-tool ai/roam-memory-gptel-tool-remember)
  (ai/roam-memory--clear-gptel-tool ai/roam-memory-gptel-tool-recall)
  (ai/roam-memory--clear-gptel-tool ai/roam-memory-gptel-tool-assert-fact)
  (gptel-make-tool
   :name ai/roam-memory-gptel-tool-remember
   :function #'ai/roam-memory--tool-remember
   :category "roam"
   :description "Store a durable memory note in the user's org-roam memory area. SUBJECT names the topic and CONTENT is the memory to keep; SOURCE optionally records where the memory came from. Refuses to write when full editor rights are missing."
   :args '((:name "subject"
            :type string
            :description "Topic the memory belongs to")
           (:name "content"
            :type string
            :description "The memory text to store")
           (:name "source"
            :type string
            :optional t
            :description "Where this memory came from"))
   :include t)
  (gptel-make-tool
   :name ai/roam-memory-gptel-tool-recall
   :function #'ai/roam-memory--tool-recall
   :category "roam"
   :description "Fetch the durable memory note stored for SUBJECT in the user's org-roam memory area. Returns the note content, or a plain notice when no memory exists. Read-only."
   :args '((:name "subject"
            :type string
            :description "Topic to recall the memory for"))
   :include t)
  (gptel-make-tool
   :name ai/roam-memory-gptel-tool-assert-fact
   :function #'ai/roam-memory--tool-assert-fact
   :category "roam"
   :description "Append one structured world fact (SUBJECT PREDICATE OBJECT) to the durable Prolog fact log in the user's org-roam memory area. SOURCE optionally records where the fact came from. Refuses to write when full editor rights are missing."
   :args '((:name "subject"
            :type string
            :description "Subject entity of the fact")
           (:name "predicate"
            :type string
            :description "Relation linking subject to object")
           (:name "object"
            :type string
            :description "Object entity or value of the fact")
           (:name "source"
            :type string
            :optional t
            :description "Where this fact came from"))
   :include t)
  (when (boundp 'ai/agent-tools)
    (cl-pushnew ai/roam-memory-gptel-tool-remember ai/agent-tools :test #'equal)
    (cl-pushnew ai/roam-memory-gptel-tool-recall ai/agent-tools :test #'equal)
    (cl-pushnew ai/roam-memory-gptel-tool-assert-fact ai/agent-tools :test #'equal))
  (list ai/roam-memory-gptel-tool-remember
        ai/roam-memory-gptel-tool-recall
        ai/roam-memory-gptel-tool-assert-fact))

;;;###autoload
(defun ai/roam-memory-recall (subject)
  "Display SUBJECT's memory note in a right side window."
  (interactive
   (list (completing-read "Roam AI: recall memory for subject: "
                          (ai/roam-memory-list))))
  (let* ((content (or (ai/roam-memory-read subject)
                      (user-error "Roam AI: no memory for %s" subject)))
         (buffer (get-buffer-create
                  (format "*Roam Memory: %s*"
                          (ai/roam-memory--slug subject)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert content)
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (display-buffer buffer
                    '(display-buffer-in-side-window (side . right)))
    (pop-to-buffer buffer)))

(provide 'ai-roam-memory)
;;; ai-roam-memory.el ends here
