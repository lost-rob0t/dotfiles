;;; temple-planets.el --- Planetary divination system (extra confirmation bias)

(require 'json)
(require 'url)

;;; Configuration
(defvar temple-ipgeo-api-key #'(nsa/auth-source-get :host "ipgeolocation.io")
  "Free API key from ipgeolocation.io")

(defvar temple-prolog-available nil
  "Whether Prolog integration is available")

;;; Prolog Bridge (with fallback)

(defun temple--prolog-query (query)
  "Query Prolog knowledge base, return result or nil"
  (condition-case err
      (if (and (fboundp 'ediprolog-dwim)
               (file-exists-p (expand-file-name "~/Documents/Notes/org/Temple/kb/base.pl")))
          ;; Try ediprolog if available
          (with-temp-buffer
            (insert query)
            (ediprolog-dwim)
            (buffer-string))
        ;; Fallback: call swipl directly
        (let ((prolog-file (expand-file-name "~/Documents/Notes/org/Temple/kb/base.pl")))
          (when (file-exists-p prolog-file)
            (with-temp-buffer
              (call-process "swipl" nil t nil
                            "-q"
                            "-s" prolog-file
                            "-g" query
                            "-t" "halt")
              (buffer-substring-no-properties (point-min) (point-max))))))
    (error
     (message "Prolog query failed: %s" (error-message-string err))
     nil)))

(defun temple--get-affirmation ()
  "Get random affirmation from Prolog"
  (let ((result (temple--prolog-query
                 "random_affirmation(Text), write(Text)")))
    (when (and result (> (length (string-trim result)) 0))
      (string-trim result))))

(defun temple--get-meanings (number)
  "Get meanings for a number from Prolog"
  (let* ((query (format "meanings_for_number(%d, Ms), write(Ms)" number))
         (result (temple--prolog-query query)))
    (when (and result (> (length (string-trim result)) 0))
      (string-trim result))))

;;; API Functions (simplified)
(defun temple-get-astronomy-simple* ()
  "Get astronomy data via ipgeolocation.io"
  (condition-case err
      (let* ((url (format "https://api.ipgeolocation.io/astronomy?apiKey=%s" ;dont fool yourself Thinking that was enough bro.
                          temple-ipgeo-api-key))
             (url-request-method "GET")
             (buffer (url-retrieve-synchronously url t nil 5)))
        (when buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "^$")
            (let ((json-object-type 'plist))
              (json-read)))))
    (error
     (message "API call failed: %s" (error-message-string err))
     nil)))

;;; Simple moon phase (no API needed)

(defun temple-simple-moon-phase* ()
  "Calculate moon phase without API"
  (let* ((now (float-time))
         ;; Known new moon: 2000-01-06 18:14 UTC
         (new-moon-ref 947182440.0)
         ;; Synodic month in seconds (29.530588853 days)
         (lunar-month 2551442.8)
         (elapsed (- now new-moon-ref))
         (cycles (/ elapsed lunar-month))
         (phase (- cycles (floor cycles))))
    (list phase
          (cond
           ((< phase 0.033) "New Moon 🌑")
           ((< phase 0.216) "Waxing Crescent 🌒")
           ((< phase 0.283) "First Quarter 🌓")
           ((< phase 0.466) "Waxing Gibbous 🌔")
           ((< phase 0.533) "Full Moon 🌕")
           ((< phase 0.716) "Waning Gibbous 🌖")
           ((< phase 0.783) "Last Quarter 🌗")
           ((< phase 0.966) "Waning Crescent 🌘")
           (t "New Moon 🌑")))))

;;; Planetary hours (traditional)

(defun temple-planetary-hour* ()
  "Get current planetary hour ruler"
  (let* ((hour (string-to-number (format-time-string "%H")))
         (day (string-to-number (format-time-string "%u"))) ; 1=Mon
         (planets '("Moon" "Mars" "Mercury" "Jupiter" "Venus" "Saturn" "Sun"))
         ;; Day rulers: Mon=Moon, Tue=Mars, Wed=Mercury, Thu=Jupiter, Fri=Venus, Sat=Saturn, Sun=Sun
         (day-rulers '(0 1 2 3 4 5 6)) ; indices into planets
         (day-start (nth (1- day) day-rulers))
         (hour-ruler (mod (+ day-start hour) 7)))
    (nth hour-ruler planets)))

;;; Main divination reading

(defun temple-planetary-reading* ()
  "Generate divination reading based on current conditions"
  (interactive)
  (let* ((moon-data (temple-simple-moon-phase*))
         (moon-phase-num (car moon-data))
         (moon-phase-name (cadr moon-data))
         (planetary-hour (temple-planetary-hour*))
         (sacred-number 12) ; Your key sobriety number
         (affirmation (temple--get-affirmation))
         (number-meanings (temple--get-meanings sacred-number))
         (hour-num (string-to-number (format-time-string "%H")))
         (minute-num (string-to-number (format-time-string "%M")))
         ;; Derive reading number from time
         (reading-number (mod (+ hour-num minute-num) 50)))

    (with-current-buffer (get-buffer-create "*Temple Reading*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "╔═══════════════════════════════════════════════╗\n")
        (insert "║        🔮 TEMPLE DIVINATION READING 🔮        ║\n")
        (insert (format "║           %s           ║\n"
                        (format-time-string "%Y-%m-%d %H:%M")))
        (insert "╚═══════════════════════════════════════════════╝\n\n")

        (insert (format "* Sacred Number: %d\n" sacred-number))
        (when number-meanings
          (insert (format "  Meanings: %s\n" number-meanings)))
        (insert "\n")

        (when affirmation
          (insert "* Affirmation:\n")
          (insert (format "  「%s」\n\n" affirmation)))

        (insert (format "* Lunar Phase: %s\n" moon-phase-name))
        (insert (format "  Phase: %.1f%% illuminated\n\n" (* moon-phase-num 100)))

        (insert (format "* Planetary Hour: %s\n\n" planetary-hour))

        (insert (format "* Reading Number: %d\n" reading-number))
        (when-let ((reading-meanings (temple--get-meanings reading-number)))
          (insert (format "  Meanings: %s\n\n" reading-meanings)))

        (insert "───────────────────────────────────────────────\n")
        (insert "* Interpretation:\n\n")

        ;; Add contextual interpretation
        (insert (pcase moon-phase-name
                  ((pred (string-match "New"))
                   "  New beginnings. Time for intention-setting.\n")
                  ((pred (string-match "Full"))
                   "  Peak energy. Completion and release.\n")
                  ((pred (string-match "Waxing"))
                   "  Building phase. Take action, grow.\n")
                  ((pred (string-match "Waning"))
                   "  Releasing phase. Let go, reflect.\n")
                  (_ "  Transitional energy. Stay aware.\n")))

        (insert "\n")
        (insert "───────────────────────────────────────────────\n")
        (insert "\n")

        ;; Add AI query button
        (let ((button-start (point)))
          (insert-text-button "🤖 Query AI for More Interpretation"
                              'action 'temple--query-ai-interpretation
                              'follow-link t
                              'face 'button)
          (insert "\n\n"))

        (org-mode)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun temple--query-ai-interpretation (button)
  "Query AI for additional interpretation of the current reading"
  (interactive)
  (let* ((buffer-content (with-current-buffer "*Temple Reading*"
                           (buffer-substring-no-properties (point-min) (point-max))))
         (prompt (format "Please provide deeper spiritual and psychological interpretation of this divination reading:\n\n%s\n\nFocus on practical guidance and symbolic meanings." buffer-content)))
    (if (fboundp 'gptel-send)
        ;; Use gptel if available
        (progn
          (with-current-buffer (get-buffer-create "*Temple AI Interpretation*")
            (erase-buffer)
            (insert "🤖 AI Interpretation Request Sent...\n\n")
            (insert "Reading being analyzed:\n")
            (insert "──────────────────────\n")
            (insert buffer-content)
            (insert "\n──────────────────────\n\n")
            (goto-char (point-max))
            (gptel-send prompt))
          (display-buffer "*Temple AI Interpretation*"))
      ;; Fallback if gptel not available
      (message "AI interpretation requires gptel package. Install with: (package-install 'gptel)"))))

;;; Quick reading (pure offline)

(defun temple-quick-reading* ()
  "Quick reading without any external dependencies"
  (interactive)
  (let* ((moon (temple-simple-moon-phase*))
         (hour (temple-planetary-hour*))
         (num (mod (string-to-number (format-time-string "%M")) 50))
         (affirmation (temple--get-affirmation)))
    (message "🌙 %s | %s hour | #%d | %s"
             (cadr moon) hour num affirmation)))

;;; Simple display

(defun temple-show-moon ()
  "Show just moon phase (no API)"
  (interactive)
  (let* ((moon (temple-simple-moon-phase*))
         (phase-pct (* (car moon) 100))
         (phase-name (cadr moon)))
    (message "Moon: %s (%.1f%%)" phase-name phase-pct)))

;;; Keybindings
(global-set-key (kbd "C-c S r") 'temple-planetary-reading*)
(global-set-key (kbd "C-c S q") 'temple-quick-reading*)
(global-set-key (kbd "C-c S m") 'temple-show-moon)

(provide 'temple-planets)

;; Test it works:
;; (temple-planetary-reading*)
