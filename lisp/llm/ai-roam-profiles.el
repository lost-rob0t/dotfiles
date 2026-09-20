;;; ai-roam-profiles.el --- Named gptel profiles for the roam AI -*- lexical-binding: t; -*-

;;; Commentary:

;; Named gptel profiles ("presets") for the roam AI.  Typing @roam at
;; the start of a prompt line in any gptel chat applies the `roam'
;; profile: a notes-aware assistant wired with the roam system message
;; (rights awareness included) and the registered roam tools.  The
;; `roam-outline' profile drafts org outlines only.
;;
;; Profiles can also be applied interactively with
;; `ai/roam-profiles-apply' (Doom: SPC m y r).  Applying a profile
;; defines a gptel preset named roam:PROFILE once per session, sets
;; `gptel-preset' buffer-locally when that variable exists, and
;; unconditionally sets `gptel-system-message' and `gptel-tools'
;; buffer-locally so behavior is identical whether or not the running
;; gptel supports presets.  A :before advice on `gptel-send' consumes
;; a leading @mention on the current line before sending.
;;
;; This module is Doom-free and batch-safe: gptel is never hard
;; required, and every gptel touch is guarded.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-roam)
(require 'ai-roam-chat)

(declare-function gptel-make-preset "gptel" (name &rest plist))

(defgroup ai-roam-profiles nil
  "Named gptel profiles for the roam AI."
  :group 'ai-roam)

(defcustom ai/roam-profiles
  '((roam
     . ((description . "Answer questions about my notes and chat using my roam graph, tools, and memory")
        (system-message-fn . ai/roam-chat--system-message)
        (tools-fn . ai/roam-chat--tools)))
    (roam-outline
     . ((description . "Draft org outlines for my roam notes only")
        (system-message-fn . nil)
        (tools-fn . nil))))
  "Named roam AI gptel profiles.
Each entry is (NAME . SPEC) where NAME is a symbol and SPEC is an
alist with a `description' string, a `system-message-fn' function
called with the current buffer's file name (nil means use
`ai/roam-profiles-outline-message'), and a `tools-fn' function
returning the gptel tool list (nil means no tools)."
  :type '(alist :key-type symbol
                :value-type (alist :options ((description string)
                                             (system-message-fn function)
                                             (tools-fn function))))
  :group 'ai-roam-profiles)

(defcustom ai/roam-profiles-outline-message
  "You are the roam outline assistant for my org-roam notes.  OUTLINE ONLY: draft org-mode headings and structure for me to fill in.  You must not edit note content, must not create roam:id links, and must treat every section as outline-only regardless of resolved editor rights."
  "Fixed system message for profiles without a `system-message-fn'.
Emphasizes outline-only behavior."
  :type 'string
  :group 'ai-roam-profiles)

(defvar ai/roam-profiles--presets (make-hash-table :test #'eq)
  "Profiles whose gptel preset has already been defined this session.")

(defvar ai/roam-profiles--advice-added nil
  "Non-nil once the @mention advice sits on `gptel-send'.")

(defun ai/roam-profiles-names ()
  "Return the sorted list of profile name symbols."
  (sort (mapcar #'car ai/roam-profiles)
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun ai/roam-profiles--mention-regexp ()
  "Return the regexp matching a leading @profile mention.
Group 1 is the profile name; matching is case-insensitive under a
t binding of `case-fold-search'."
  (let ((names
         (sort (mapcar (lambda (name) (regexp-quote (symbol-name name)))
                       (ai/roam-profiles-names))
               (lambda (a b) (> (length a) (length b))))))
    (concat "\\`[ \t\n\r]*@[ \t\n\r]*\\("
            (string-join names "\\|") "\\)\\_>")))

(defun ai/roam-profiles--match (input)
  "Return (PROFILE-NAME END) for a leading mention in INPUT, or nil.
END is the string index just past the @token; whitespace before
the @ is skipped.  Return nil when INPUT is not a string or does
not begin with a profile mention."
  (when (stringp input)
    (let ((case-fold-search t))
      (when (string-match (ai/roam-profiles--mention-regexp) input)
        (list (intern (downcase (match-string 1 input)))
              (match-end 0))))))

(defun ai/roam-profiles--extract-mention (input)
  "Extract a leading @profile mention from INPUT.
Return (PROFILE-NAME . REMAINING-TEXT-TRIMMED), or nil when INPUT
does not begin with a profile mention."
  (let ((match (ai/roam-profiles--match input)))
    (when match
      (cons (nth 0 match)
            (string-trim (substring input (nth 1 match)))))))

(defun ai/roam-profiles--preset-name (profile-name)
  "Return the gptel preset symbol for PROFILE-NAME."
  (intern (format "roam:%s" profile-name)))

(defun ai/roam-profiles--resolve (profile-name file)
  "Return (SYSTEM-MESSAGE . TOOLS) for PROFILE-NAME at FILE."
  (let ((definition (assq profile-name ai/roam-profiles)))
    (unless definition
      (user-error "Roam AI: unknown profile %s" profile-name))
    (let ((system-fn (alist-get 'system-message-fn (cdr definition)))
          (tools-fn (alist-get 'tools-fn (cdr definition))))
      (cons (if (functionp system-fn)
                (funcall system-fn file)
              ai/roam-profiles-outline-message)
            (when (functionp tools-fn)
              (funcall tools-fn))))))

(defun ai/roam-profiles--define-preset (profile-name resolved)
  "Define the gptel preset for PROFILE-NAME from RESOLVED.
RESOLVED is the (SYSTEM-MESSAGE . TOOLS) pair from
`ai/roam-profiles--resolve'.  Safe to call repeatedly: redefinition
refreshes the preset.  Does nothing when gptel presets are
unavailable."
  (when (fboundp 'gptel-make-preset)
    (let ((plist (list :description
                       (format "Roam AI %s profile: %s"
                               profile-name
                               (alist-get
                                'description
                                (cdr (assq profile-name ai/roam-profiles))))
                       :system (car resolved))))
      (when (cdr resolved)
        (setq plist (plist-put plist :tools (cdr resolved))))
      (apply #'gptel-make-preset
             (ai/roam-profiles--preset-name profile-name)
             plist))))

;;;###autoload
(defun ai/roam-profiles-apply (profile-name)
  "Apply the roam AI PROFILE-NAME to the current buffer.
Define the roam:PROFILE gptel preset once, set `gptel-preset'
buffer-locally when available, and set `gptel-system-message' and
`gptel-tools' buffer-locally so behavior does not depend on preset
support."
  (interactive
   (list (intern (completing-read "Roam AI profile: "
                                  (mapcar #'symbol-name
                                          (ai/roam-profiles-names))
                                  nil t))))
  (unless (require 'gptel nil t)
    (user-error "Roam AI: gptel required for roam profiles"))
  (let ((resolved (ai/roam-profiles--resolve profile-name
                                             (buffer-file-name))))
    (unless (gethash profile-name ai/roam-profiles--presets)
      (ai/roam-profiles--define-preset profile-name resolved)
      (puthash profile-name t ai/roam-profiles--presets))
    (when (boundp 'gptel-preset)
      (setq-local gptel-preset (ai/roam-profiles--preset-name profile-name)))
    (setq-local gptel-system-message (car resolved))
    (setq-local gptel-tools (cdr resolved))
    (message "Roam AI: profile %s applied" profile-name)))

(defun ai/roam-profiles-send-advice (&rest _)
  "Consume a leading @profile mention before `gptel-send' runs.
Scan the current line from its start to point; on a mention, delete
the @token (leaving the remaining text) and apply the profile."
  (save-excursion
    (let* ((start (line-beginning-position))
           (text (buffer-substring-no-properties start (point)))
           (match (ai/roam-profiles--match text)))
      (when match
        (delete-region start (+ start (nth 1 match)))
        (ai/roam-profiles-apply (nth 0 match))))))

(defun ai/roam-profiles-setup ()
  "Set up the roam AI profiles globally.
When gptel is available: add the `gptel-send' advice exactly once
and define the per-profile gptel presets (once per session).
Without gptel this is a silent no-op so startup stays resilient."
  (when (require 'gptel nil t)
    (unless ai/roam-profiles--advice-added
      (advice-add 'gptel-send :before #'ai/roam-profiles-send-advice)
      (setq ai/roam-profiles--advice-added t))
    ;; Define each profile's preset once, in a neutral (no-file)
    ;; context.  The first definition wins the session hash, so the
    ;; buffer-local settings from `ai/roam-profiles-apply' remain the
    ;; authoritative per-buffer behavior.
    (dolist (name (ai/roam-profiles-names))
      (unless (gethash name ai/roam-profiles--presets)
        (ai/roam-profiles--define-preset name (ai/roam-profiles--resolve name nil))
        (puthash name t ai/roam-profiles--presets)))))

(provide 'ai-roam-profiles)
;;; ai-roam-profiles.el ends here
