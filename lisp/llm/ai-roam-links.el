;;; ai-roam-links.el --- roam:id link repair with editor-rights gate -*- lexical-binding: t; -*-

;;; Commentary:

;; roam:id link plumbing for the roam AI:
;;
;; - `ai/roam-links-scan-directory' finds org files that lack an `ID:'
;;   property.  The scan is plain text: neither org nor org-id needs to
;;   be loaded, so it stays cheap on large graphs and in batch Emacs.
;; - `ai/roam-links-repair-missing' is the interactive repair loop.
;; - `ai/roam-links--ensure-id' inserts a property drawer or extends an
;;   existing one without org-mode machinery.
;; - `ai/roam-links-register-gptel-tools' exposes the rights-gated
;;   `create_roam_id_link' gptel tool.  Link creation fails closed
;;   through `ai/roam-assert-full-rights' unless the LLM holds full
;;   editor rights for the target file.
;;
;; This module is Doom-free so it can be tested in batch Emacs.

;;; Code:

(require 'ai-roam)
(require 'cl-lib)
(require 'subr-x)

(declare-function org-id-new "org-id" ())

(defgroup ai-roam-links nil
  "roam:id link repair for the roam AI."
  :group 'ai-roam)

(defconst ai/roam-links-gptel-tool "create_roam_id_link"
  "Name of the gptel tool that creates roam:id link targets.")

(defconst ai/roam-links--drawer-scan-limit 40
  "Number of leading lines scanned for a file's top property drawer.")

(defun ai/roam-links--read-contents (file)
  "Return the raw contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ai/roam-links--write-contents (file contents)
  "Write CONTENTS to FILE."
  (with-temp-file file
    (insert contents)))

(defun ai/roam-links--top-drawer-id (file)
  "Return the org ID from FILE's top property drawer, or nil.
Plain-text scan of the leading lines; neither org nor org-id is
loaded for this check.  Tolerates CRLF line endings."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((remaining ai/roam-links--drawer-scan-limit)
            (state 'prologue)
            id done)
        (while (and (not done) (> remaining 0) (not (eobp)))
          (let ((line (string-trim-right
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
            (setq remaining (1- remaining))
            (cond
             ((eq state 'prologue)
              (cond
               ((string-match-p "\\`[ \t]*:PROPERTIES:[ \t]*\\'" line)
                (setq state 'drawer))
               ((string-match-p "\\`[ \t]*\\'" line))
               ((string-match-p "\\`[ \t]*#\\+" line))
               (t (setq done t))))
             (t
              (cond
               ((string-match "\\`[ \t]*:ID:[ \t]*\\(.*?\\)[ \t]*\\'" line)
                (setq id (match-string 1 line))
                (setq done t))
               ((string-match-p "\\`[ \t]*:END:[ \t]*\\'" line)
                (setq done t)))))
            (forward-line 1)))
        id))))

(defun ai/roam-links-scan-directory (&optional directory)
  "Return sorted absolute org files under DIRECTORY lacking an org ID.
DIRECTORY defaults to `ai/roam-directory'.  Backup files (trailing
\"~\") and lock files (leading \".#\") are skipped."
  (let ((dir (file-name-as-directory
              (expand-file-name (or directory ai/roam-directory)))))
    (when (file-directory-p dir)
      (sort
       (cl-loop
        for file in (directory-files-recursively dir "\\.org\\'")
        unless (or (string-suffix-p "~" file)
                   (string-prefix-p ".#" (file-name-nondirectory file)))
        unless (ai/roam-links--top-drawer-id file)
        collect file)
       #'string<))))

(defun ai/roam-links--insert-id-line (contents id)
  "Return CONTENTS with an :ID: line for ID added at the top drawer.
When CONTENTS opens with a property drawer the :ID: line joins it;
otherwise a new drawer is inserted after any leading blank and
#+KEYWORD lines."
  (let* ((lines (split-string contents "\n"))
         (n (length lines))
         (idx 0))
    ;; Leading blank lines.
    (while (and (< idx n)
                (string-match-p "\\`[ \t]*\\'" (nth idx lines)))
      (setq idx (1+ idx)))
    ;; Leading #+KEYWORD lines (e.g. #+TITLE:, #+CREATED:).
    (while (and (< idx n)
                (string-match-p "\\`[ \t]*#\\+" (nth idx lines)))
      (setq idx (1+ idx)))
    (let ((id-line (format ":ID: %s" id)))
      (if (and (< idx n)
               (string-match-p "\\`[ \t]*:PROPERTIES:[ \t]*\\'"
                               (nth idx lines)))
          ;; Existing top drawer: add the ID right after its opening line.
          (mapconcat #'identity
                     (append (cl-subseq lines 0 (1+ idx))
                             (list id-line)
                             (cl-subseq lines (1+ idx)))
                     "\n")
        ;; No drawer yet: insert one, then keep a blank line before the body.
        (let* ((head (cl-subseq lines 0 idx))
               (tail (cl-subseq lines idx))
               (tail (if (and (consp tail)
                              (string-match-p "\\`[ \t]*\\'" (car tail)))
                         tail
                       (cons "" tail))))
          (mapconcat #'identity
                     (append head
                             (list ":PROPERTIES:" id-line ":END:")
                             tail)
                     "\n"))))))

(defun ai/roam-links--ensure-id (file)
  "Ensure FILE carries an org ID in its top property drawer.
Return the ID, generating and writing one when FILE lacks it.
Pure file I/O: safe outside org-mode and in batch Emacs."
  (or (ai/roam-links--top-drawer-id file)
      (progn
        (unless (file-exists-p file)
          (user-error "Roam AI: %s does not exist" file))
        (require 'org-id)
        (let* ((id (org-id-new))
               (contents (ai/roam-links--read-contents file)))
          (ai/roam-links--write-contents
           file (ai/roam-links--insert-id-line contents id))
          id))))

(defun ai/roam-links-repair-missing (&optional directory)
  "Add org IDs to every ID-less org file under DIRECTORY.
DIRECTORY defaults to `ai/roam-directory'.  Return (REPAIRED SKIPPED)."
  (interactive
   (list (read-directory-name "Roam AI: repair missing IDs in: "
                              (ai/roam--directory))))
  (let* ((missing (ai/roam-links-scan-directory directory))
         (repaired 0))
    (dolist (file missing)
      (message "Roam AI: repairing %s" file)
      (condition-case err
          (progn
            (ai/roam-links--ensure-id file)
            (cl-incf repaired))
        (error
         (message "Roam AI: could not repair %s: %s"
                  file (error-message-string err)))))
    (let ((skipped (- (length missing) repaired)))
      (message "Roam AI: repaired %d, skipped %d" repaired skipped)
      (list repaired skipped))))

(defun ai/roam-links--resolve-target (target)
  "Return the absolute org file path for gptel TARGET.
Relative paths resolve against `ai/roam-directory'."
  (if (file-name-absolute-p target)
      (expand-file-name target)
    (expand-file-name target (ai/roam--directory))))

(defun ai/roam-links--tool-create-id-link (target title)
  "Create a roam:id link target for org file TARGET named TITLE.
Fails closed with an error string when full editor rights are
missing for the target file; the file is never edited in that case."
  (let ((file (ai/roam-links--resolve-target target)))
    (if (not (file-exists-p file))
        (format "ERROR: create_roam_id_link: no such org file: %s" target)
      (condition-case err
          (progn
            (ai/roam-assert-full-rights file "roam:id link creation")
            (let ((id (ai/roam-links--ensure-id file)))
              (format "roam:id link ready for %s.  Insert [[roam:%s][%s]] (bare form [[roam:%s]])"
                      file id title id)))
        (user-error
         (format "ERROR: %s" (error-message-string err)))))))

(defun ai/roam-links--clear-gptel-tool (name)
  "Remove an existing gptel tool named NAME before re-registering it."
  (when (fboundp 'gptel-get-tool)
    (ignore-errors (setf (gptel-get-tool name) nil))))

(defun ai/roam-links-register-gptel-tools ()
  "Register the rights-gated roam:id link creation tool with gptel."
  (unless (require 'gptel nil t)
    (user-error "Roam AI: gptel is required for roam link tools"))
  (ai/roam-links--clear-gptel-tool ai/roam-links-gptel-tool)
  (gptel-make-tool
   :name ai/roam-links-gptel-tool
   :function #'ai/roam-links--tool-create-id-link
   :category "roam"
   :description "Create a roam:id link target for one of the user's org-roam notes. TARGET is the path of an existing org file; TITLE is the link text. Ensures the target carries an org ID and returns the [[roam:ID][TITLE]] link to insert. Refuses to edit the note when full editor rights are missing."
   :args '((:name "target"
            :type string
            :description "Path of the org file to link to")
           (:name "title"
            :type string
            :description "Link title text"))
   :include t)
  (when (boundp 'ai/agent-tools)
    (cl-pushnew ai/roam-links-gptel-tool ai/agent-tools :test #'equal))
  ai/roam-links-gptel-tool)

(provide 'ai-roam-links)
;;; ai-roam-links.el ends here
