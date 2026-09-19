;;; ai-roam.el --- Org-roam LLM ontology, rights, and outline drafting -*- lexical-binding: t; -*-

;;; Commentary:

;; The Roam AI foundation: a fixed section ontology for the org-roam
;; graph, an LLM editor-rights resolver, and outline-only drafting.
;;
;; Rights resolve in order: the global `ai/full-editor-rights' toggle,
;; then the per-section `ai/roam-section-rights' alist, then
;; `ai/roam-default-rights'.  `full' lets the LLM edit roam nodes and
;; create roam:id links; `outline' restricts it to generating org
;; outlines the user fills in.
;;
;; This module is Doom-free so it can be tested in batch Emacs.

;;; Code:

(require 'cl-lib)

(defgroup ai-roam nil
  "Roam AI: ontology, rights, and LLM drafting for org-roam."
  :group 'org)

(defcustom ai/roam-directory "~/Documents/Notes/org/roam/"
  "Root directory of the org-roam graph managed by the roam AI."
  :type 'string
  :group 'ai-roam)

(defcustom ai/roam-sections
  '(starintel osint entities hacking scada ai llm programming elisp
              prolog nix android reading media daily writing meta)
  "Fixed top-level roam sections.
The section a note lives in drives its editor rights and, later,
its publish visibility."
  :type '(repeat symbol)
  :group 'ai-roam)

(defcustom ai/roam-section-rights
  '((hacking . full) (scada . full) (writing . full) (meta . full))
  "Per-section rights overriding `ai/roam-default-rights'.
Sections absent from this alist use the default."
  :type '(alist :key-type symbol :value-type (choice (const full) (const outline)))
  :group 'ai-roam)

(defcustom ai/roam-default-rights 'outline
  "Rights used when the global toggle is off and no section entry applies."
  :type '(choice (const full) (const outline))
  :group 'ai-roam)

(defcustom ai/full-editor-rights nil
  "Global LLM editor-rights toggle.
When non-nil the LLM may edit roam nodes and create roam:id links
everywhere.  When nil rights fall back to the section alist and
`ai/roam-default-rights'."
  :type 'boolean
  :group 'ai-roam)

(defcustom ai/roam-outline-directive
  "Headings only, no body content: I will fill in the content myself."
  "Instruction appended to every outline-drafting prompt."
  :type 'string
  :group 'ai-roam)

(defun ai/roam--directory ()
  "Return `ai/roam-directory' as an expanded directory path."
  (file-name-as-directory (expand-file-name ai/roam-directory)))

(defun ai/roam-section-for-file (&optional file)
  "Return the roam SECTION symbol containing FILE, or nil.
FILE in the roam root, outside the graph, or in a subdirectory that
is not a declared section yields nil."
  (when (stringp file)
    (let* ((root (ai/roam--directory))
           (path (expand-file-name file)))
      (when (string-prefix-p root path)
        (let* ((rel (substring path (length root)))
               (head (car (split-string rel "/+" t))))
          (when head
            (car (cl-member (intern head) ai/roam-sections))))))))

(defun ai/roam-rights (&optional file)
  "Return the rights symbol, `full' or `outline', for FILE.
Resolution order: the global `ai/full-editor-rights' toggle, then
the per-section rights alist, then `ai/roam-default-rights'."
  (cond
   (ai/full-editor-rights 'full)
   ((cdr (assq (ai/roam-section-for-file file) ai/roam-section-rights)))
   (t ai/roam-default-rights)))

(defun ai/roam-full-p (&optional file)
  "Return non-nil when the LLM holds full editor rights for FILE."
  (eq (ai/roam-rights file) 'full))

(defun ai/roam-outline-p (&optional file)
  "Return non-nil when FILE is restricted to outline-only drafting."
  (eq (ai/roam-rights file) 'outline))

(defun ai/roam-assert-full-rights (&optional file operation)
  "Signal unless the LLM holds full rights for FILE.
OPERATION names the action for the error message."
  (unless (ai/roam-full-p file)
    (user-error "Roam AI: %s requires full editor rights (%s)"
                (or operation "this operation")
                (if ai/full-editor-rights "granted" "outline-only here"))))

(defun ai/roam-section-target-path (file section)
  "Return the absolute path for FILE moved into the SECTION directory."
  (expand-file-name
   (file-name-nondirectory file)
   (file-name-as-directory
    (expand-file-name (symbol-name section) (ai/roam--directory)))))

(defun ai/roam-toplevel-dirs ()
  "Return one absolute directory path per declared section."
  (mapcar (lambda (section)
            (file-name-as-directory
             (expand-file-name (symbol-name section) (ai/roam--directory))))
          ai/roam-sections))

(defun ai/roam-outline-prompt (topic section)
  "Compose the outline-drafting prompt for TOPIC in SECTION."
  (format "Draft an org-mode outline about \"%s\" for the %s section of my org-roam notes. %s Begin headings at level one for the note title and use level two for the main outline entries."
          topic (symbol-name section) ai/roam-outline-directive))

(defun ai/roam-toggle-full-editor-rights ()
  "Flip the global `ai/full-editor-rights' toggle."
  (interactive)
  (setq ai/full-editor-rights (not ai/full-editor-rights))
  (message "Roam AI: full editor rights %s"
           (if ai/full-editor-rights "ENABLED (LLM may edit roam nodes)" "disabled (outline-only)")))

(defun ai/roam--current-section ()
  "Return the section of the current buffer's file, or nil."
  (ai/roam-section-for-file (buffer-file-name)))

(defun ai/roam-draft-outline (topic section)
  "Draft an org outline for TOPIC in SECTION via gptel.
Allowed everywhere: outline drafting is the fallback rights level."
  (interactive
   (let* ((default-section (ai/roam--current-section))
          (section (intern
                    (completing-read
                     "Section: "
                     (mapcar #'symbol-name ai/roam-sections)
                     nil nil nil nil
                     (when default-section (symbol-name default-section))))))
     (list (read-string (format "Outline topic for %s: " section)) section)))
  (unless (require 'gptel nil t)
    (user-error "Roam AI: gptel is required for outline drafting"))
  (let* ((prompt (ai/roam-outline-prompt topic section))
         (buffer (get-buffer-create "*Roam Outline*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "# Outline draft: %s (%s)\n\n" topic section))))
    (gptel-request prompt
      :system "You generate org-mode outlines. Return only org headings, no prose, no code fences."
      :callback
      (lambda (response _info)
        (when (stringp response)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert response "\n")))
          (pop-to-buffer buffer)
          (message "Roam AI: outline draft ready"))))))

(defun ai/roam-adopt-section (file section)
  "Move FILE into the SECTION directory of the roam graph.
Ensures the file carries an org ID when org-roam is available so
roam:id links survive the move."
  (interactive
   (progn
     (cl-assert (buffer-file-name) nil "Current buffer has no file")
     (list (buffer-file-name)
           (intern (completing-read
                    "Adopt into section: "
                    (mapcar #'symbol-name ai/roam-sections))))))
  (let* ((target (ai/roam-section-target-path file section))
         (target-dir (file-name-directory target)))
    (unless (file-exists-p file)
      (user-error "Roam AI: %s does not exist" file))
    (make-directory target-dir t)
    (when (and (fboundp 'org-id-get) (derived-mode-p 'org-mode))
      (with-current-buffer (find-file-noselect file)
        (org-id-get-create)
        (save-buffer)))
    (rename-file file target)
    (message "Roam AI: adopted %s into %s" (file-name-nondirectory file) section)
    target))

(provide 'ai-roam)
;;; ai-roam.el ends here
