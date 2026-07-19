;;; temple.el --- Oracle divination system -*- lexical-binding: t; -*-

;; Copyright (C) 2024 lost-rob0t

;; Author: lost-rob0t <lost-rob0t@users.noreply.github.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0.0") (org-ql "0.6"))
;; Keywords: tools, divination, org-mode
;; URL: https://github.com/lost-rob0t/.dotfiles

;;; Commentary:

;; Temple provides an oracle divination system integrated with org-roam.
;; It supports various divination methods, pattern analysis, and AI-assisted
;; interpretation when GPTel is available.

;; Key features:
;; - Multiple divination methods
;; - Org-roam integration for storing readings
;; - Pattern frequency analysis
;; - AI-assisted interpretation (optional)
;; - Daily affirmations and meanings

;;; Code:

(require 'org)
(require 'org-roam nil t)
(require 'org-ql nil t)

(defgroup temple nil
  "Oracle divination system."
  :group 'tools
  :prefix "temple-")

(defcustom temple-directory "~/Documents/Notes/org/Temple"
  "Directory containing temple knowledge base files."
  :type 'directory
  :group 'temple)

(defcustom temple-prolog-system 'swi
  "Prolog system to use for knowledge base queries."
  :type '(choice (const :tag "SWI-Prolog" swi)
                 (const :tag "GNU Prolog" gnu)
                 (const :tag "Other" other))
  :group 'temple)

(defvar temple-session-log nil
  "Log of divination sessions for the current day.")

(defvar temple-patterns-cache nil
  "Cache of pattern frequency data.")

;;; Core Divination Functions

(defun temple-generate-number ()
  "Generate a random number for divination (1-78 for tarot-like system)."
  (1+ (random 78)))

(defun temple-divine-basic ()
  "Perform basic number divination."
  (interactive)
  (let ((number (temple-generate-number)))
    (temple-record-divination number)
    (temple-display-meaning number)
    number))

(defun temple-divine-with-question ()
  "Perform divination with a specific question."
  (interactive)
  (let* ((question (read-string "Your question: "))
         (number (temple-generate-number)))
    (temple-record-divination number question)
    (temple-display-meaning number question)
    number))

(defun temple-divine-three-card ()
  "Perform three-card divination (past, present, future)."
  (interactive)
  (let* ((question (read-string "Your question: "))
         (past (temple-generate-number))
         (present (temple-generate-number))
         (future (temple-generate-number))
         (reading (list past present future)))
    (temple-record-three-card-reading reading question)
    (temple-display-three-card-meaning reading question)
    reading))

;;; Recording and Display Functions

(defun temple-record-divination (number &optional question)
  "Record a divination with NUMBER and optional QUESTION."
  (let ((timestamp (current-time))
        (entry (list :number number
                     :question question
                     :timestamp timestamp)))
    (push entry temple-session-log)
    (when (featurep 'org-roam)
      (temple-save-to-org-roam entry))))

(defun temple-record-three-card-reading (numbers question)
  "Record a three-card reading with NUMBERS and QUESTION."
  (let ((timestamp (current-time))
        (entry (list :type 'three-card
                     :numbers numbers
                     :question question
                     :timestamp timestamp)))
    (push entry temple-session-log)
    (when (featurep 'org-roam)
      (temple-save-to-org-roam entry))))

(defun temple-display-meaning (number &optional question)
  "Display meaning for NUMBER with optional QUESTION context."
  (let ((buffer (get-buffer-create "*Temple Oracle*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "🕉️  TEMPLE ORACLE 🕉️\n"))
      (insert (format "═══════════════════\n\n"))

      (when question
        (insert (format "Question: %s\n\n" question)))

      (insert (format "Number: %d\n" number))
      (insert (format "Drawn at: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))

      (insert "MEANING:\n")
      (insert "--------\n")
      (insert (temple-get-meaning number))

      (goto-char (point-min))
      (pop-to-buffer buffer))))

(defun temple-display-three-card-meaning (numbers question)
  "Display three-card reading for NUMBERS with QUESTION."
  (let ((buffer (get-buffer-create "*Temple Oracle*"))
        (labels '("Past" "Present" "Future")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "🕉️  THREE CARD READING 🕉️\n"))
      (insert (format "═══════════════════════\n\n"))

      (insert (format "Question: %s\n\n" question))

      (dotimes (i 3)
        (let ((number (nth i numbers))
              (label (nth i labels)))
          (insert (format "%s (%d):\n" label number))
          (insert (format "%s\n\n" (temple-get-meaning number)))))

      (insert "SYNTHESIS:\n")
      (insert "----------\n")
      (insert (temple-synthesize-reading numbers))

      (goto-char (point-min))
      (pop-to-buffer buffer))))

;;; Knowledge Base Functions

(defun temple-get-meaning (number)
  "Get meaning for NUMBER from knowledge base."
  (or (temple-get-prolog-meaning number)
      (temple-get-fallback-meaning number)))

(defun temple-get-prolog-meaning (number)
  "Get meaning from Prolog knowledge base if available."
  (when (executable-find "swipl")
    (let ((kb-file (expand-file-name "kb/meanings.pl" temple-directory)))
      (when (file-exists-p kb-file)
        (temple-query-prolog kb-file
                            (format "meaning(%d, Text)" number))))))

(defun temple-get-fallback-meaning (number)
  "Get fallback meaning when Prolog is not available."
  (let ((meanings '((1 . "New beginnings, fresh start, potential")
                    (2 . "Balance, cooperation, partnership")
                    (3 . "Creativity, growth, expansion")
                    (12 . "Sobriety, daily reset, spiritual discipline")
                    (21 . "Completion, fulfillment, achievement")
                    (22 . "Master builder, unlimited potential")
                    (42 . "The answer to everything, deep wisdom")
                    (78 . "Completion of the journey, cosmic understanding"))))
    (or (cdr (assoc number meanings))
        (format "Number %d carries the energy of %s"
                number
                (cond
                 ((<= number 10) "foundational principles")
                 ((<= number 22) "spiritual lessons")
                 ((<= number 40) "material manifestation")
                 ((<= number 60) "emotional wisdom")
                 (t "higher consciousness"))))))

(defun temple-synthesize-reading (numbers)
  "Synthesize meaning from multiple NUMBERS."
  (format "The progression from %d → %d → %d suggests a journey from %s through %s toward %s."
          (car numbers) (cadr numbers) (caddr numbers)
          (temple-get-short-meaning (car numbers))
          (temple-get-short-meaning (cadr numbers))
          (temple-get-short-meaning (caddr numbers))))

(defun temple-get-short-meaning (number)
  "Get abbreviated meaning for NUMBER."
  (cond
   ((<= number 10) "foundation")
   ((<= number 22) "spiritual growth")
   ((<= number 40) "material progress")
   ((<= number 60) "emotional development")
   (t "transcendence")))

;;; Prolog Integration

(defun temple-query-prolog (kb-file query)
  "Query Prolog KB-FILE with QUERY."
  (let ((temp-file (make-temp-file "temple-query" nil ".pl")))
    (with-temp-file temp-file
      (insert (format "
:- consult('%s').
:- %s, write(Text), nl, halt.
:- write('No meaning found'), nl, halt.
" kb-file query)))

    (let ((result (shell-command-to-string
                   (format "swipl -q -t halt -s %s" temp-file))))
      (delete-file temp-file)
      (if (string-match "No meaning found" result)
          nil
        (string-trim result)))))

;;; Pattern Analysis

(defun temple-analyze-patterns ()
  "Analyze patterns in recorded divinations."
  (interactive)
  (let ((numbers (temple-extract-numbers-from-log))
        (frequency-table (make-hash-table :test 'equal)))

    ;; Count frequencies
    (dolist (num numbers)
      (let ((count (gethash num frequency-table 0)))
        (puthash num (1+ count) frequency-table)))

    ;; Display results
    (let ((buffer (get-buffer-create "*Temple Patterns*")))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "PATTERN ANALYSIS\n")
        (insert "================\n\n")

        (insert "Number Frequencies:\n")
        (maphash (lambda (num count)
                   (insert (format "%2d: %s (%d times)\n"
                                   num
                                   (make-string count ?█)
                                   count)))
                 frequency-table)

        (goto-char (point-min))
        (pop-to-buffer buffer)))))

(defun temple-extract-numbers-from-log ()
  "Extract numbers from session log."
  (let (numbers)
    (dolist (entry temple-session-log)
      (cond
       ((plist-get entry :number)
        (push (plist-get entry :number) numbers))
       ((plist-get entry :numbers)
        (setq numbers (append (plist-get entry :numbers) numbers)))))
    numbers))

;;; Org-Roam Integration

(defun temple-save-to-org-roam (entry)
  "Save divination ENTRY to org-roam."
  (when (and (featurep 'org-roam)
             (boundp 'org-roam-directory)
             org-roam-directory)
    (let* ((timestamp (plist-get entry :timestamp))
           (date-str (format-time-string "%Y%m%d%H%M%S" timestamp))
           (title (format "Temple Reading %s"
                          (format-time-string "%Y-%m-%d %H:%M" timestamp)))
           (file-path (expand-file-name
                      (format "temple/temple-%s-reading.org" date-str)
                      org-roam-directory)))

      (make-directory (file-name-directory file-path) t)

      (with-temp-file file-path
        (insert (format "#+TITLE: %s\n" title))
        (insert (format "#+CREATED: %s\n"
                        (format-time-string "[%Y-%m-%d %a %H:%M]" timestamp)))
        (insert "#+FILETAGS: :temple:divination:\n\n")

        (when (plist-get entry :question)
          (insert (format "* Question\n%s\n\n" (plist-get entry :question))))

        (if (eq (plist-get entry :type) 'three-card)
            (temple-format-three-card-org entry)
          (temple-format-single-card-org entry))))))

(defun temple-format-single-card-org (entry)
  "Format single card ENTRY for org-roam."
  (let ((number (plist-get entry :number)))
    (insert (format "* Reading\nNumber: %d\n\n" number))
    (insert (format "** Meaning\n%s\n\n" (temple-get-meaning number)))
    (insert "** Reflection\n\n")))

(defun temple-format-three-card-org (entry)
  "Format three-card ENTRY for org-roam."
  (let ((numbers (plist-get entry :numbers))
        (labels '("Past" "Present" "Future")))
    (insert "* Reading\n")
    (dotimes (i 3)
      (let ((number (nth i numbers))
            (label (nth i labels)))
        (insert (format "** %s (%d)\n%s\n\n"
                        label number (temple-get-meaning number)))))
    (insert "** Synthesis\n")
    (insert (temple-synthesize-reading numbers))
    (insert "\n\n** Reflection\n\n")))

;;; AI Integration (when GPTel is available)

(defun temple-ai-interpret (numbers &optional question)
  "Get AI interpretation of NUMBERS with optional QUESTION."
  (when (featurep 'gptel)
    (let ((prompt (format "Interpret this oracle reading:\n\nNumbers: %s\n%s\n\nProvide spiritual insight and practical guidance."
                          (if (listp numbers)
                              (mapconcat #'number-to-string numbers ", ")
                            (number-to-string numbers))
                          (if question (format "Question: %s" question) ""))))
      (gptel-send prompt))))

;;; Interactive Commands

;;;###autoload
(defun temple ()
  "Open temple divination system."
  (interactive)
  (let ((choice (read-char-choice
                 "Temple Oracle: (1) Basic (2) Question (3) Three-card (4) Patterns (5) AI: "
                 '(?1 ?2 ?3 ?4 ?5))))
    (case choice
      (?1 (temple-divine-basic))
      (?2 (temple-divine-with-question))
      (?3 (temple-divine-three-card))
      (?4 (temple-analyze-patterns))
      (?5 (when (featurep 'gptel)
            (call-interactively #'temple-ai-interpret))))))

;;;###autoload
(defun temple-reset-session ()
  "Reset the current temple session."
  (interactive)
  (setq temple-session-log nil)
  (setq temple-patterns-cache nil)
  (message "Temple session reset"))

(provide 'temple)
;;; temple.el ends here