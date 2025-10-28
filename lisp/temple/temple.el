;; -*- lexical-binding: t; -*-
;;; nsaspy's schizo temple
(require 'gptel)
(require 'org-element)
(require 'org-roam)
(require 'subr-x)


(defun temple ()
  "Open the temple."
  (interactive)
  (find-file "~/Documents/Notes/org/Temple/temple.org"))



(defvar temple-prolog-file (expand-file-name "~/Documents/Notes/org/Temple/kb/facts.pl"))


(defvar temple-prolog-binary "swipl"
  "Path to SWI-Prolog executable.")

(defvar temple-base-pl "/home/unseen/Documents/Notes/org/Temple/kb/base.pl"
  "Path to the main Prolog file that loads meanings.pl.")


(defun temple-run-query (query)
  "Run a Prolog QUERY in temple-base-pl and return raw output as string."
  (let ((cmd (format "%s -q -s %s -g \"%s, writeln(Result), halt.\""
                     temple-prolog-binary
                     temple-base-pl
                     query)))
    (with-temp-buffer
      (shell-command cmd (current-buffer))
      (string-trim (buffer-string)))))

(defun temple-query-int-meaning-list (query)
  "Run QUERY returning a list of Number-Meaning pairs and parse it."
  (let ((output (temple-run-query query)))
    ;; parse [1-prompt,1-creative_block,...] into (1 prompt) style
    (if (string-match "\\[\\(.*\\)\\]" output)
        (mapcar (lambda (pair)
                  (let ((parts (split-string pair "-" t)))
                    (cons (string-to-number (car parts))
                          (string-trim (cadr parts)))))
                (split-string (match-string 1 output) "," t))
      ;; fallback: return empty
      nil)))

(defun temple-insert-daily-int-meanings (n)
  "Insert N random Number:Meaning pairs into today's Org-roam node."
  (interactive "nNumber of meanings: ")
  (org-roam-dailies-find-today)
  (goto-char (point-max))
  (insert (format "\n* Temple Oracle (%d meanings)\n" n))
  (let ((results (temple-query-int-meaning-list
                  (format "random_meanings(%d, Result)" n))))
    (dolist (pair results)
      (insert (format "- %d: %s\n" (car pair) (cdr pair))))))

;; optional keybinding
(global-set-key (kbd "C-c t m") #'temple-insert-daily-int-meanings)

;;; Affirmations
(defun temple-get-random-affirmation ()
  "Query Prolog for a random affirmation and return it as a string."
  (let ((cmd (format "%s -q -s %s -g \"random_affirmation(Text), write(Text), halt.\""
                     temple-prolog-binary
                     temple-base-pl)))
    (with-temp-buffer
      (shell-command cmd (current-buffer))
      (string-trim (buffer-string)))))

(defun temple-show-affirmation ()
  "Display a random affirmation in the minibuffer."
  (interactive)
  (let ((affirmation (temple-get-random-affirmation)))
    (message "🕉️  %s" affirmation)))

(defun temple-insert-affirmation ()
  "Insert a random affirmation at point."
  (interactive)
  (insert (temple-get-random-affirmation)))

;; For later: insert into org-roam daily
(defun temple-insert-daily-affirmation (&optional n)
  "Insert N random affirmations into today's Org-roam daily node.
If N is nil, insert 1 affirmation."
  (interactive "p")
  (unless n (setq n 1))
  (org-roam-dailies-find-today)
  (goto-char (point-max))
  (insert (format "\n* Temple Affirmations (%s)\n"
                  (format-time-string "%H:%M")))
  (dotimes (_ n)
    (insert (format "- %s\n" (temple-get-random-affirmation)))))

;; Keybindings
(global-set-key (kbd "C-c t a") #'temple-show-affirmation)
(global-set-key (kbd "C-c t A") #'temple-insert-daily-affirmation)

(defun temple-divine-with-question (question n-meanings n-affirmations)
  "Perform a full Temple divination with QUESTION, N-MEANINGS, and N-AFFIRMATIONS.
Creates an org entry with the question as header, tagged :temple:."
  (interactive
   (list (read-string "What do you seek guidance on? ")
         (read-number "Number of meanings: " 3)
         (read-number "Number of affirmations: " 1)))
  (org-roam-dailies-find-today)
  (goto-char (point-max))
  (insert (format "\n* %s :temple:\n" question))
  (insert (format "<%s>\n\n" (format-time-string "%Y-%m-%d %a %H:%M")))

  ;; Insert meanings - use the working function
  (insert "** Numeric Oracle\n")
  (let ((results (temple-query-int-meaning-list
                  (format "random_meanings(%d, Result)" n-meanings))))
    (dolist (pair results)
      (insert (format "- *%d*: %s\n" (car pair) (cdr pair)))))

  ;; Insert affirmations
  (insert "\n** Affirmations\n")
  (dotimes (_ n-affirmations)
    (insert (format "- %s\n" (temple-get-random-affirmation))))

  ;; Insert reflection space
  (insert "\n** Reflection\n")
  (insert "#+begin_quote\n\n#+end_quote\n")
  (forward-line -1))

(global-set-key (kbd "C-c t d") #'temple-divine-with-question)

(defun temple-review-past-divinations ()
  "Review all past Temple divinations using org-ql."
  (interactive)
  (require 'org-ql)
  (org-ql-search (org-roam-dailies--list-files)
    '(tags "temple")
    :sort 'date
    :title "Temple Divinations"))

(defun temple-search-divinations (query-text)
  "Search Temple divinations containing QUERY-TEXT."
  (interactive "sSearch divinations for: ")
  (require 'org-ql)
  (org-ql-search (org-roam-dailies--list-files)
    `(and (tags "temple")
          (regexp ,query-text))
    :sort 'date))

(global-set-key (kbd "C-c t r") #'temple-review-past-divinations)
(global-set-key (kbd "C-c t s") #'temple-search-divinations)

;;; Pattern Recording & Expansion System

(defun temple-add-meaning (number meaning)
  "Add a new NUMBER-MEANING pair to meanings.pl"
  (interactive
   (list (read-number "Number: ")
         (read-string "Meaning (use underscores): ")))
  (with-temp-buffer
    (insert-file-contents temple-prolog-file)
    (goto-char (point-max))
    (insert (format "\nnumber_meaning(%d, %s)." number meaning))
    (write-region (point-min) (point-max) temple-prolog-file)
    (message "Added: %d → %s" number meaning)))

(defun temple-record-pattern (numbers interpretation)
  "Record an observed pattern of NUMBERS with INTERPRETATION."
  (interactive
   (list (read-string "Numbers observed (space-separated): ")
         (read-string "Pattern interpretation: ")))
  (org-roam-dailies-find-today)
  (goto-char (point-max))
  (insert (format "\n* Pattern: [%s] :temple:pattern:\n" numbers))
  (insert (format "<%s>\n\n" (format-time-string "%Y-%m-%d %a %H:%M")))
  (insert (format "Numbers: %s\n" numbers))
  (insert (format "Interpretation: %s\n\n" interpretation))
  (insert "** Context\n")
  (insert "What was happening when you noticed this?\n\n")
  (insert "** Resonance\n")
  (insert "Why did this pattern stand out?\n\n"))

(defun temple-pattern-frequency ()
  "Analyze which numbers appear most frequently in divinations."
  (interactive)
  (require 'org-ql)
  (let ((frequencies (make-hash-table :test 'equal)))
    ;; Extract all numbers from temple-tagged entries
    (org-ql-select (org-roam-dailies--list-files)
      '(tags "temple")
      :action (lambda ()
                (save-excursion
                  (let* ((element (org-element-at-point))
                         (begin (org-element-property :begin element))
                         (end (org-element-property :end element))
                         (content (buffer-substring-no-properties begin end)))
                    (when content
                      ;; Find all "- NUMBER:" patterns
                      (with-temp-buffer
                        (insert content)
                        (goto-char (point-min))
                        (while (re-search-forward "^- \\*?\\([0-9]+\\)\\*?: " nil t)
                          (let ((num (match-string 1)))
                            (puthash num (1+ (gethash num frequencies 0)) frequencies)))))))))
    ;; Display results
    (let ((sorted (sort (hash-table-keys frequencies)
                        (lambda (a b)
                          (> (gethash a frequencies)
                             (gethash b frequencies))))))
      (with-current-buffer (get-buffer-create "*Temple Frequencies*")
        (erase-buffer)
        (insert "** Temple Number Frequencies\n\n")
        (insert "Numbers appearing most in your divinations:\n\n")
        (dolist (num sorted)
          (insert (format "- %s: %d times\n"
                          num (gethash num frequencies))))
        (org-mode)
        (pop-to-buffer (current-buffer))))))

(defun temple-synchronicity-log (number event)
  "Log a synchronicity - when NUMBER appears in the world."
  (interactive
   (list (read-number "Number you saw: ")
         (read-string "Where/how did you see it? ")))
  (org-roam-dailies-find-today)
  (goto-char (point-max))
  (insert (format "\n* Sync: %d :temple:sync:\n" number))
  (insert (format "<%s>\n" (format-time-string "%Y-%m-%d %a %H:%M")))
  (insert (format "%s\n\n" event))
  ;; Look up meanings cleanly
  (let* ((cmd (format "%s -q -s %s -g \"meanings_for_number(%d, Ms), write(Ms), halt.\""
                      temple-prolog-binary
                      temple-base-pl
                      number))
         (output (with-temp-buffer
                   (shell-command cmd (current-buffer))
                   (buffer-string))))
    (when (string-match "\\[\\([^]]+\\)\\]" output)
      (let ((meanings (replace-regexp-in-string "," ", " (match-string 1 output))))
        (insert (format "Known meanings: %s\n" meanings))))))

(global-set-key (kbd "C-c t n") #'temple-add-meaning)
(global-set-key (kbd "C-c t p") #'temple-record-pattern)
(global-set-key (kbd "C-c t f") #'temple-pattern-frequency)
(global-set-key (kbd "C-c t y") #'temple-synchronicity-log)
;;; AI-Enhanced Oracle Interpretation

(defvar temple-interpretation-system-prompt
  "You are an oracle interpreter for a personal divination system. The user receives:
- Numeric spreads with symbolic meanings
- Affirmations from a curated knowledge base
- A specific question they're seeking guidance on

Your role:
1. Synthesize the numeric meanings into coherent guidance
2. Connect the affirmations to the question's emotional/practical dimensions
3. Provide actionable, grounded interpretation
4. Identify patterns and connections the user might miss
5. Be direct, honest, and specific - avoid vague mysticism

The user is technically sophisticated (OSINT/pentesting background), values clarity and signal-to-noise ratio, and is working on sobriety (number 12 is their anchor). They use this system for cognitive navigation and decision-making.

Format your response as:
- **The Signal**: Core message in 1-2 sentences
- **Number Synthesis**: How the numbers interact/progress
- **Affirmation Integration**: How affirmations connect to the spread
- **Actionable Guidance**: Specific next steps
- **Pattern Notes**: Anything unusual or significant")

;;; AI-Enhanced Oracle Interpretation

(defvar temple-interpretation-system-prompt
  "You are an oracle interpreter for a personal divination system. The user receives:
- Numeric spreads with symbolic meanings
- Affirmations from a curated knowledge base
- A specific question they're seeking guidance on

Your role:
1. Synthesize the numeric meanings into coherent guidance
2. Connect the affirmations to the question's emotional/practical dimensions
3. Provide actionable, grounded interpretation
4. Identify patterns and connections the user might miss
5. Be direct, honest, and specific - avoid vague mysticism

The user is technically sophisticated (OSINT/pentesting background), values clarity and signal-to-noise ratio, and is working on sobriety (number 12 is their anchor). They use this system for cognitive navigation and decision-making.

Format your response as:
- **The Signal**: Core message in 1-2 sentences
- **Number Synthesis**: How the numbers interact/progress
- **Affirmation Integration**: How affirmations connect to the spread
- **Actionable Guidance**: Specific next steps
- **Pattern Notes**: Anything unusual or significant")

;;; AI-Enhanced Oracle Interpretation

(defvar temple-interpretation-system-prompt
  "You are an oracle interpreter for a personal divination system. The user receives:
- Numeric spreads with symbolic meanings
- Affirmations from a curated knowledge base
- A specific question they're seeking guidance on

Your role:
1. Synthesize the numeric meanings into coherent guidance
2. Connect the affirmations to the question's emotional/practical dimensions
3. Provide actionable, grounded interpretation
4. Identify patterns and connections the user might miss
5. Be direct, honest, and specific - avoid vague mysticism

The user is technically sophisticated (OSINT/pentesting background), values clarity and signal-to-noise ratio, and is working on sobriety (number 12 is their anchor). They use this system for cognitive navigation and decision-making.

Format your response as:
- **The Signal**: Core message in 1-2 sentences
- **Number Synthesis**: How the numbers interact/progress
- **Affirmation Integration**: How affirmations connect to the spread
- **Actionable Guidance**: Specific next steps
- **Pattern Notes**: Anything unusual or significant")

(defun temple-interpret-with-ai (question numbers-alist affirmations)
  "Send oracle results to AI for interpretation.
NUMBERS-ALIST is like ((12 . \"habit_formation\") (19 . \"mindfulness_prompt\"))
AFFIRMATIONS is a list of affirmation strings."
  (let* ((numbers-formatted
          (mapconcat (lambda (pair)
                       (format "- *%d*: %s" (car pair) (cdr pair)))
                     numbers-alist "\n"))
         (affirmations-formatted
          (mapconcat (lambda (aff)
                       (format "- %s" aff))
                     affirmations "\n"))
         (prompt (format "Question: %s\n\n** Numeric Oracle\n%s\n\n** Affirmations\n%s\n\nProvide interpretation:"
                         question
                         numbers-formatted
                         affirmations-formatted)))
    (gptel-request prompt
      :system temple-interpretation-system-prompt
      :callback (lambda (response info)
                  (if (not response)
                      (message "AI interpretation failed: %s" info)
                    (with-current-buffer (get-buffer-create "*Temple Interpretation*")
                      (erase-buffer)
                      (insert (format "* %s\n\n" question))
                      (insert response)
                      (org-mode)
                      (display-buffer (current-buffer))))))))

;;; Add this at the VERY TOP of temple.el if not already there
;; -*- lexical-binding: t; -*-

(defun temple-divine-with-ai (question n-meanings n-affirmations)
  "Perform divination with AI interpretation."
  (interactive
   (list (read-string "What do you seek guidance on? ")
         (read-number "Number of meanings: " 3)
         (read-number "Number of affirmations: " 2)))

  (org-roam-dailies-find-today)
  (goto-char (point-max))
  (insert (format "\n* %s :temple:ai:\n" question))
  (insert (format "<%s>\n\n" (format-time-string "%Y-%m-%d %a %H:%M")))

  ;; Get oracle results
  (let* ((meanings (temple-query-int-meaning-list
                    (format "random_meanings(%d, Result)" n-meanings)))
         (affs (cl-loop repeat n-affirmations
                        collect (temple-get-random-affirmation)))
         (numbers (mapcar #'car meanings)))

    ;; Insert oracle data
    (insert (format "** Raw Numbers: %s\n"
                    (mapconcat #'number-to-string numbers " → ")))
    (insert "\n** Numeric Oracle\n")
    (dolist (pair meanings)
      (insert (format "- *%d*: %s\n" (car pair) (cdr pair))))

    (insert "\n** Affirmations\n")
    (dolist (aff affs)
      (insert (format "- %s\n" aff)))

    ;; AI interpretation - use a NAMED buffer as communication channel
    (insert "\n** AI Interpretation\n")
    (insert "#+begin_quote\n")
    (let ((marker-pos (point)))
      (insert "_Consulting oracle..._")
      (insert "\n#+end_quote\n")

      ;; Store marker info in buffer-local variable
      (setq-local temple--ai-marker marker-pos)
      (setq-local temple--ai-buffer-name (buffer-name))

      ;; Build prompt
      (let ((prompt (format "Question: %s\n\n** RAW NUMBERS: %s\n\n** Numeric Oracle\n%s\n\n** Affirmations\n%s\n\nAnalyze this spread. Pay attention to numeric patterns."
                            question
                            (mapconcat #'number-to-string numbers " → ")
                            (mapconcat (lambda (p) (format "- *%d*: %s" (car p) (cdr p)))
                                       meanings "\n")
                            (mapconcat (lambda (a) (format "- %s" a))
                                       affs "\n"))))

        ;; Send request - callback uses buffer-local vars
        (gptel-request
            prompt
          :system temple-interpretation-system-prompt
          :callback #'temple--ai-callback)))

    ;; Reflection space
    (insert "\n** Your Reflection\n")
    (insert "#+begin_quote\n\n#+end_quote\n")
    (forward-line -1)))

(defun temple--ai-callback (response info)
  "Callback for AI interpretation. Uses buffer-local variables."
  (if (not response)
      (message "Temple AI failed: %s" info)
    ;; Get buffer by name (stored in buffer-local var)
    (let ((target-buf (get-buffer temple--ai-buffer-name)))
      (if (not target-buf)
          (message "Temple: target buffer was killed")
        (with-current-buffer target-buf
          (save-excursion
            ;; Go to stored position
            (goto-char temple--ai-marker)
            ;; Delete "Consulting oracle..." line
            (delete-region (line-beginning-position) (line-end-position))
            ;; Insert response
            (insert response)))))))

(global-set-key (kbd "C-c t i") #'temple-divine-with-ai)

(defun temple-recent-numbers (days)
  "Show numbers from last N DAYS to spot recurring themes"
  (org-ql-search
    (org-roam-dailies--list-files)
    `(and (tags "temple")
          (ts :from ,(- days) :to 0))))

;;; temple-ai.el - AI-powered pattern analysis
;;; All functions suffixed with * use AI interpretation


(defvar temple-ai-model "claude-sonnet-4-20250514"
  "Model for divination analysis.")

(defvar temple-ai-system-prompt
  "You are analyzing numeric divination patterns from a Prolog-based oracle system.

The user practices cyber-divination:
- Numbers 1-50 have personal meanings (see meanings.pl)
- Number 12 (sobriety/habit_formation/daily_reset) is currently most sacred
- Synchronicities are numbers appearing in external world
- User works in OSINT/pentesting, thinks symbolically
- User is in recovery, values clarity over escapism

Analyze patterns for:
- Recurring numbers and their meanings
- Clustering (what appears together)
- Synchronicity timing relative to divinations
- Anxiety/question patterns
- Actionable insights

Be direct, technical, and spiritually literate. No fluff.")

(defun temple-recent-numbers* (days)
  "AI analyzes numbers from last N DAYS for patterns."
  (interactive "nAnalyze last N days: ")
  (let ((entries nil))
    ;; Collect all temple entries from timeframe
    (org-ql-select (org-roam-dailies--list-files)
      '(tags "temple")
      :action (lambda ()
                (let* ((element (org-element-at-point))
                       (begin (org-element-property :begin element))
                       (end (org-element-property :end element))
                       (timestamp (org-entry-get nil "TIMESTAMP"))
                       (content (when (and begin end)
                                  (buffer-substring-no-properties begin end))))
                  (when content
                    (push (cons timestamp content) entries)))))

    ;; Build prompt with all data
    (let* ((prompt (format "Analyze these divination entries from the last %d days:\n\n%s\n\nWhat patterns emerge? What's the oracle saying?"
                           days
                           (mapconcat (lambda (e)
                                        (format "[%s]\n%s\n---"
                                                (or (car e) "no-timestamp")
                                                (cdr e)))
                                      (reverse entries) "\n")))
           (buf (get-buffer-create "*Temple Recent Analysis*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "** Analysis: Last %d Days\n\n" days))
        (insert "Processing...\n\n"))
      (gptel-request prompt
        :system temple-ai-system-prompt
        :buffer buf
        :callback (lambda (response info)
                    (when response
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (delete-region (point-min) (point-max))
                        (insert (format "** Analysis: Last %d Days\n\n" days))
                        (insert response)
                        (org-mode)
                        (display-buffer buf))))))))

(defun temple-pattern-frequency* ()
  "AI interprets which numbers appear most and why."
  (interactive)
  (require 'org-ql)
  (let ((frequencies (make-hash-table :test 'equal))
        (contexts (make-hash-table :test 'equal)))

    ;; Gather numbers + context
    (org-ql-select (org-roam-dailies--list-files)
      '(tags "temple")
      :action (lambda ()
                (save-excursion
                  (let* ((element (org-element-at-point))
                         (title (org-get-heading t t t t))
                         (begin (org-element-property :begin element))
                         (end (org-element-property :end element))
                         (content (when (and begin end)
                                    (buffer-substring-no-properties begin end))))
                    (when content
                      (with-temp-buffer
                        (insert content)
                        (goto-char (point-min))
                        (while (re-search-forward "^[-*] \\*?\\([0-9]+\\)\\*?: \\([^\n]+\\)" nil t)
                          (let ((num (match-string 1))
                                (meaning (match-string 2)))
                            (puthash num (1+ (gethash num frequencies 0)) frequencies)
                            (push (cons title meaning)
                                  (gethash num contexts nil))
                            (puthash num (gethash num contexts) contexts)))))))))

    ;; Build analysis prompt
    (let* ((sorted-nums (sort (hash-table-keys frequencies)
                              (lambda (a b)
                                (> (gethash a frequencies)
                                   (gethash b frequencies)))))
           (data (mapconcat
                  (lambda (num)
                    (format "Number %s: appeared %d times\nContexts:\n%s\n"
                            num
                            (gethash num frequencies)
                            (mapconcat (lambda (c)
                                         (format "  - %s: %s" (car c) (cdr c)))
                                       (gethash num contexts)
                                       "\n")))
                  sorted-nums
                  "\n"))
           (buf (get-buffer-create "*Temple Frequency Analysis*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "** Frequency Pattern Analysis\n\nProcessing...\n"))
      (gptel-request
          (format "Number frequency analysis:\n\n%s\n\nWhat themes are dominant? What is the user being shown repeatedly?" data)
        :system temple-ai-system-prompt
        :buffer buf
        :callback (lambda (response info)
                    (when response
                      (with-current-buffer buf
                        (erase-buffer)
                        (insert "** Frequency Pattern Analysis\n\n")
                        (insert response)
                        (org-mode)
                        (display-buffer buf))))))))

(defun temple-synchronicity-analysis* (days)
  "AI analyzes synchronicity patterns and timing."
  (interactive "nAnalyze syncs from last N days: ")
  (let ((syncs nil))
    (org-ql-select (org-roam-dailies--list-files)
      '(tags "sync")
      :action (lambda ()
                (let* ((element (org-element-at-point))
                       (begin (org-element-property :begin element))
                       (end (org-element-property :end element))
                       (content (when (and begin end)
                                  (buffer-substring-no-properties begin end))))
                  (when content
                    (push content syncs)))))

    (let* ((prompt (format "Synchronicity log analysis:\n\n%s\n\nWhen do numbers appear in the world vs in divinations? What's the pattern of manifestation?"
                           (mapconcat 'identity (reverse syncs) "\n---\n")))
           (buf (get-buffer-create "*Temple Sync Analysis*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "Processing...\n"))
      (gptel-request prompt
        :system temple-ai-system-prompt
        :buffer buf
        :callback (lambda (response info)
                    (when response
                      (with-current-buffer buf
                        (erase-buffer)
                        (insert response)
                        (org-mode)
                        (display-buffer buf))))))))

(defun temple-question-pattern* ()
  "AI analyzes what types of questions user asks the oracle."
  (interactive)
  (let ((questions nil))
    (org-ql-select (org-roam-dailies--list-files)
      '(tags "temple")
      :action (lambda ()
                (let ((heading (org-get-heading t t t t))
                      (timestamp (org-entry-get nil "TIMESTAMP")))
                  (when (string-match-p "\\?" heading)
                    (push (cons heading timestamp) questions)))))

    (let* ((prompt (format "User's oracle questions:\n\n%s\n\nWhat patterns in anxiety, seeking, relationships? What is really being asked?"
                           (mapconcat (lambda (q)
                                        (format "[%s] %s"
                                                (or (cdr q) "no-timestamp")
                                                (car q)))
                                      (reverse questions) "\n")))
           (buf (get-buffer-create "*Temple Question Analysis*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "Processing...\n"))
      (gptel-request prompt
        :system temple-ai-system-prompt
        :buffer buf
        :callback (lambda (response info)
                    (when response
                      (with-current-buffer buf
                        (erase-buffer)
                        (insert response)
                        (org-mode)
                        (display-buffer buf))))))))

(defun temple-read-all* ()
  "Meta-reading: AI analyzes the entire temple practice."
  (interactive)
  (let ((all-content ""))
    (dolist (file (org-roam-dailies--list-files))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (setq all-content
                (concat all-content
                        (buffer-substring-no-properties (point-min) (point-max))
                        "\n\n===FILE BREAK===\n\n")))))
    (let ((buf (get-buffer-create "*Temple Meta-Reading*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "Processing complete archive...\n"))
      (gptel-request
          (format "Complete temple archive:\n\n%s\n\nProvide a meta-reading: What is this practice revealing? What's the user's relationship with the oracle? Where is growth needed?"
                  all-content)
        :system temple-ai-system-prompt
        :buffer buf
        :callback (lambda (response info)
                    (when response
                      (with-current-buffer buf
                        (erase-buffer)
                        (insert "** Temple Meta-Analysis\n")
                        (insert (format "<%s>\n\n" (format-time-string "%Y-%m-%d %a %H:%M")))
                        (insert response)
                        (org-mode)
                        (display-buffer buf))))))))

(defun number-tarot (&optional question n affirmations)
  "Perform a numeric divination reading with past/present/future spread.
Queries the Prolog knowledge base for meanings and affirmations."
  (interactive (list (read-string "What do you seek guidance on? ")
                     (read-number "Number of meanings: " 3)
                     (read-number "Number of affirmations: " 2)))
  (let* ((meanings (temple-query-int-meaning-list (format "random_meanings(%d, Result)" n)))
         (past (cdr (first meanings)))
         (present (cdr (second meanings)))
         (future (cdr (third meanings)))
         (numbers (mapcar #'car meanings))
         (affirmation-texts (temple-query-affirmations* affirmations))
         (buf (get-buffer-create "*Temple Reading*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Temple Reading\n")
      (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert "* Reading\n\n")
      (when (and question (not (string-empty-p question)))
        (insert (format "** Question\n%s\n\n" question)))
      (insert (format "** Numbers Drawn\n%s\n\n"
                      (mapconcat #'number-to-string numbers " → ")))
      (insert (format "** Past [%d]\n%s\n\n" (first numbers) past))
      (insert (format "** Present [%d]\n%s\n\n" (second numbers) present))
      (insert (format "** Future [%d]\n%s\n\n" (third numbers) future))
      (when affirmation-texts
        (insert "\n** Affirmations*\n")
        (dolist (aff affirmation-texts)
          (insert (format "- %s\n" aff))))
      (insert "\n** Reflection\n\n")
      (goto-char (point-max)))
    (pop-to-buffer buf)
    (message "Reading complete. Numbers: %s" numbers)))

(defun temple-query-affirmations* (n)
  "Query N random affirmations from the Prolog KB."
  (let ((result '()))
    (dotimes (_ n)
      (let ((aff (temple-query-single "random_affirmation(Text)")))
        (when aff
          (push (cdr (assoc 'Text aff)) result))))
    (nreverse result)))




;; Keybindings for AI analysis
(global-set-key (kbd "C-c t * r") #'temple-recent-numbers*)
(global-set-key (kbd "C-c t * f") #'temple-pattern-frequency*)
(global-set-key (kbd "C-c t * s") #'temple-synchronicity-analysis*)
(global-set-key (kbd "C-c t * q") #'temple-question-pattern*)
(global-set-key (kbd "C-c t * m") #'temple-read-all*)

(provide 'temple)
