;;; cash.el --- Fast cashier drop calculator -*- lexical-binding: t; -*-

;;; Commentary:
;; Quick cash counting and drop calculation for cashier work
;; Target drawer amount: $100, drop everything over

;;; Code:

(defvar cashier-drawer-target 100
  "Target amount to keep in drawer ($100).")

(defvar cashier-denominations
  '((20 . "20s")
    (10 . "10s")
    (5 . "5s")
    (1 . "1s")
    (0.25 . "quarters")
    (0.10 . "dimes")
    (0.05 . "nickels"))
  "Denominations and their labels (no pennies due to shortage).")

(defvar cashier-log nil
  "List of cash counting sessions for the day.")

(defun cashier-count-cash (counts)
  "Calculate total from COUNTS alist of (denomination . quantity)."
  (let ((total 0))
    (dolist (count counts)
      (setq total (+ total (* (car count) (cdr count)))))
    total))

(defun cashier-calculate-drop (current-total)
  "Calculate how much to drop given CURRENT-TOTAL in drawer."
  (max 0 (- current-total cashier-drawer-target)))

(defun cashier-parse-input (input)
  "Parse INPUT like '5 20s, 2 10s' into counts alist."
  (let ((counts '())
        (parts (split-string input "[,;]" t " ")))
    (dolist (part parts)
      (let ((tokens (split-string (string-trim part) " " t)))
        (when (>= (length tokens) 2)
          (let ((quantity (string-to-number (car tokens)))
                (denom-str (cadr tokens)))
            (let ((denom (cashier-denomination-value denom-str)))
              (when denom
                (push (cons denom quantity) counts)))))))
    counts))

(defun cashier-denomination-value (denom-str)
  "Get numeric value for DENOM-STR like '20s' or 'quarters'."
  (cond
   ((string-match-p "20s?" denom-str) 20)
   ((string-match-p "10s?" denom-str) 10)
   ((string-match-p "5s?" denom-str) 5)
   ((string-match-p "1s?" denom-str) 1)
   ((string-match-p "quarters?" denom-str) 0.25)
   ((string-match-p "dimes?" denom-str) 0.10)
   ((string-match-p "nickels?" denom-str) 0.05)
   (t nil)))

(defun cashier-format-money (amount)
  "Format AMOUNT as currency."
  (format "$%.2f" amount))

(defun cashier-quick-count ()
  "Quick interactive cash counter."
  (interactive)
  (let* ((input (read-string "Enter cash (e.g. '5 20s, 2 10s, 8 quarters'): "))
         (counts (cashier-parse-input input))
         (total (cashier-count-cash counts))
         (drop-amount (cashier-calculate-drop total))
         (remaining (- total drop-amount)))

    (with-current-buffer (get-buffer-create "*Cashier*")
      (erase-buffer)
      (insert (format "CASH COUNT\n"))
      (insert (format "==========\n\n"))

      (dolist (count counts)
        (let ((denom (car count))
              (qty (cdr count)))
          (insert (format "%d × %s = %s\n"
                         qty
                         (cashier-format-money denom)
                         (cashier-format-money (* denom qty))))))

      (insert (format "\nTOTAL IN DRAWER: %s\n" (cashier-format-money total)))
      (insert (format "TARGET: %s\n" (cashier-format-money cashier-drawer-target)))

      (if (> drop-amount 0)
          (progn
            (insert (format "\n*** DROP %s INTO SAFE ***\n" (cashier-format-money drop-amount)))
            (insert (format "KEEP IN DRAWER: %s\n" (cashier-format-money remaining))))
        (insert (format "\nNO DROP NEEDED\n")))

      (when (> drop-amount 0)
        (insert (format "\nDROP BREAKDOWN:\n"))
        (cashier-suggest-drop-breakdown drop-amount))

      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))

    (push (list (current-time) total drop-amount) cashier-log)

    (message "Total: %s | Drop: %s"
             (cashier-format-money total)
             (cashier-format-money drop-amount))))

(defun cashier-suggest-drop-breakdown (amount)
  "Suggest how to break down AMOUNT for safe drop (prioritizing large bills)."
  (let ((remaining amount)
        (breakdown '()))

    (dolist (denom '(20 10 5 1))
      (when (>= remaining denom)
        (let ((count (floor (/ remaining denom))))
          (when (> count 0)
            (push (cons denom count) breakdown)
            (setq remaining (- remaining (* denom count)))))))

    (dolist (item (reverse breakdown))
      (insert (format "Drop %d × $%d bills\n" (cdr item) (car item))))

    (when (> remaining 0)
      (insert (format "Keep change: %s\n" (cashier-format-money remaining))))))

(defun cashier-reset-log ()
  "Clear the daily cash log."
  (interactive)
  (setq cashier-log nil)
  (message "Cash log cleared"))

(defun cashier-show-log ()
  "Show today's cash counting log."
  (interactive)
  (with-current-buffer (get-buffer-create "*Cashier Log*")
    (erase-buffer)
    (insert "TODAY'S CASH LOG\n")
    (insert "===============\n\n")

    (if cashier-log
        (let ((total-drops 0))
          (dolist (entry (reverse cashier-log))
            (let ((time (format-time-string "%H:%M" (car entry)))
                  (drawer-total (cadr entry))
                  (drop-amount (caddr entry)))
              (insert (format "%s - Drawer: %s, Drop: %s\n"
                             time
                             (cashier-format-money drawer-total)
                             (cashier-format-money drop-amount)))
              (setq total-drops (+ total-drops drop-amount))))
          (insert (format "\nTOTAL DROPS TODAY: %s\n" (cashier-format-money total-drops))))
      (insert "No entries yet today.\n"))

    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun cashier-emergency-drop-check ()
  "Quick check for $50s and $100s that must be dropped immediately."
  (interactive)
  (let ((input (read-string "Any 50s or 100s? (e.g. '2 50s, 1 100'): ")))
    (when (not (string-empty-p input))
      (let* ((counts (cashier-parse-input input))
             (emergency-total 0))
        (dolist (count counts)
          (when (or (= (car count) 50) (= (car count) 100))
            (setq emergency-total (+ emergency-total (* (car count) (cdr count))))))
        (if (> emergency-total 0)
            (message "*** EMERGENCY DROP: %s ***" (cashier-format-money emergency-total))
          (message "No emergency drops needed"))))))

(defun cashier-super-quick ()
  "Super fast cash count with minimal keystrokes."
  (interactive)
  (let* ((twenties (read-number "20s: " 0))
         (tens (read-number "10s: " 0))
         (fives (read-number "5s: " 0))
         (ones (read-number "1s: " 0))
         (quarters (read-number "quarters: " 0))
         (total (+ (* twenties 20)
                   (* tens 10)
                   (* fives 5)
                   (* ones 1)
                   (* quarters 0.25)))
         (drop-amount (cashier-calculate-drop total)))

    (message "Total: %s | DROP: %s"
             (cashier-format-money total)
             (cashier-format-money drop-amount))

    (push (list (current-time) total drop-amount) cashier-log)

    (when (> drop-amount 0)
      (with-current-buffer (get-buffer-create "*Drop Alert*")
        (erase-buffer)
        (insert (format "*** DROP %s ***\n\n" (cashier-format-money drop-amount)))
        (cashier-suggest-drop-breakdown drop-amount)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(defun cashier-android-quick ()
  "Android-optimized single input version."
  (interactive)
  (let ((input (read-string "Cash (20s,10s,5s,1s,quarters): ")))
    (let* ((numbers (mapcar #'string-to-number (split-string input "[, ]" t)))
           (twenties (or (nth 0 numbers) 0))
           (tens (or (nth 1 numbers) 0))
           (fives (or (nth 2 numbers) 0))
           (ones (or (nth 3 numbers) 0))
           (quarters (or (nth 4 numbers) 0))
           (total (+ (* twenties 20)
                     (* tens 10)
                     (* fives 5)
                     (* ones 1)
                     (* quarters 0.25)))
           (drop-amount (cashier-calculate-drop total)))

      (message "DROP: %s (Total: %s)"
               (cashier-format-money drop-amount)
               (cashier-format-money total))

      (push (list (current-time) total drop-amount) cashier-log))))

(defun cashier-emergency-50-100 ()
  "Quick 50s/100s emergency drop check."
  (interactive)
  (let* ((fifties (read-number "50s: " 0))
         (hundreds (read-number "100s: " 0))
         (total (+ (* fifties 50) (* hundreds 100))))
    (if (> total 0)
        (message "*** DROP %s IMMEDIATELY ***" (cashier-format-money total))
      (message "No 50s/100s"))))

(defun cashier-drawer-status ()
  "Quick drawer status check."
  (interactive)
  (if cashier-log
      (let* ((last-entry (car cashier-log))
             (last-total (cadr last-entry))
             (last-drop (caddr last-entry))
             (estimated-current (- last-total last-drop)))
        (message "Est. drawer: %s (last count: %s)"
                 (cashier-format-money estimated-current)
                 (format-time-string "%H:%M" (car last-entry))))
    (message "No counts yet - drawer should be at %s"
             (cashier-format-money cashier-drawer-target))))

(provide 'cash)
;;; cash.el ends here