;;; org-agenda-ui-test.el --- Tests for agenda UI and backlog clearing -*- lexical-binding: t; -*-

;; Batch runner:
;;   emacs -Q --batch -L .doom.d/autoload \
;;     -l .doom.d/tests/org-agenda-ui-test.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'cl-lib)

(require 'org-agenda-ui)

(defun nsa/org-agenda-ui-test--stamp (days-offset &optional repeater)
  "Return an active Org timestamp DAYS-OFFSET days from today.
REPEATER is appended verbatim (e.g. \" +1w\")."
  (let ((time (time-add (current-time) (days-to-time days-offset))))
    (format "<%s%s>"
            (format-time-string "%Y-%m-%d %a" time)
            (or repeater ""))))

(defconst nsa/org-agenda-ui-test--entries
  '(("GetGo work" . (:todo "LOOP" :scheduled (-8 . " +1w")))
    ("Overdue thing" . (:todo "TODO" :scheduled -3))
    ("Today thing" . (:todo "TODO" :scheduled 0))
    ("Future thing" . (:todo "TODO" :scheduled 7))
    ("Old reminder" . (:todo "WAIT" :deadline -10))
    ("No date" . (:todo "TODO"))
    ("Already done" . (:todo "DONE" :scheduled -5)))
  "Heading title -> todo/scheduled/deadline fixture specification.")

(defmacro nsa/org-agenda-ui-test--with-agenda-file (&rest body)
  "Evaluate BODY with `org-agenda-files' bound to one fixture file."
  (declare (indent 0))
  `(let ((directory (make-temp-file "org-agenda-ui-test-" t))
         (org-log-done nil)
         (org-log-repeat nil)
         (org-todo-keywords '((sequence "TODO" "LOOP" "WAIT" "|" "DONE")))
         org-agenda-files
         file)
     (unwind-protect
         (progn
           (setq file (expand-file-name "agenda.org" directory))
           (with-temp-file file
             (insert "* Backlog fixture\n")
             (pcase-dolist (`(,title . ,spec) nsa/org-agenda-ui-test--entries)
               (insert (format "* %s %s\n" (plist-get spec :todo) title))
               (let ((sched (plist-get spec :scheduled)))
                 (when sched
                   (insert (format "SCHEDULED: %s\n"
                                   (if (consp sched)
                                       (nsa/org-agenda-ui-test--stamp
                                        (car sched) (cdr sched))
                                     (nsa/org-agenda-ui-test--stamp sched))))))
               (when (plist-get spec :deadline)
                 (insert (format "DEADLINE: %s\n"
                                 (nsa/org-agenda-ui-test--stamp
                                  (plist-get spec :deadline))))))
             (insert "* End marker\n"))
           (setq org-agenda-files (list file))
           ,@body)
       (delete-directory directory t))))

(defun nsa/org-agenda-ui-test--heading-states ()
  "Return an alist of (TITLE . TODO-STATE) for every fixture heading."
  (let (states)
    (org-map-entries
     (lambda ()
       (let ((title (nth 4 (org-heading-components))))
         (unless (member title '("Backlog fixture" "End marker"))
           (push (cons title (org-entry-get (point) "TODO")) states))))
     nil 'file)
    states))

(ert-deftest nsa/org-agenda-ui-backlog-predicate-classifies-entries ()
  (nsa/org-agenda-ui-test--with-agenda-file
    (let ((classified ()))
      (with-current-buffer (find-file-noselect file)
        (org-mode)
        (org-map-entries
         (lambda ()
           (let ((title (nth 4 (org-heading-components))))
             (unless (member title '("Backlog fixture" "End marker"))
               (push (cons title (and (nsa/org-entry-backlog-p) t)) classified))))
         nil 'file))
      (should (equal classified
                     '(("Already done" . nil)
                       ("No date" . nil)
                       ("Old reminder" . t)
                       ("Future thing" . nil)
                       ("Today thing" . nil)
                       ("Overdue thing" . t)
                       ("GetGo work" . t)))))))

(ert-deftest nsa/org-agenda-ui-clear-backlog-marks-overdue-done ()
  (nsa/org-agenda-ui-test--with-agenda-file
    (let ((count (nsa/org-clear-backlog)))
      (should (= count 3))
      (with-current-buffer (find-file-noselect file)
        (let ((states (nsa/org-agenda-ui-test--heading-states)))
          (should (equal (cdr (assoc "Overdue thing" states)) "DONE"))
          (should (equal (cdr (assoc "Old reminder" states)) "DONE"))
          ;; Non-backlog entries are untouched.
          (should (equal (cdr (assoc "Today thing" states)) "TODO"))
          (should (equal (cdr (assoc "Future thing" states)) "TODO"))
          (should (equal (cdr (assoc "No date" states)) "TODO"))
          (should (equal (cdr (assoc "Already done" states)) "DONE"))
          ;; Repeating entries stay active and advance past today.
          (should (equal (cdr (assoc "GetGo work" states)) "LOOP"))
          (goto-char (point-min))
          (re-search-forward "^\\* LOOP GetGo work")
          (let ((scheduled (org-entry-get (point) "SCHEDULED")))
            (should scheduled)
            (should (> (time-to-days (org-time-string-to-time scheduled))
                       (time-to-days (current-time))))))))))

(ert-deftest nsa/org-agenda-ui-clear-backlog-is-idempotent ()
  (nsa/org-agenda-ui-test--with-agenda-file
    (should (= (nsa/org-clear-backlog) 3))
    (should (= (nsa/org-clear-backlog) 0))))

(ert-deftest nsa/org-agenda-ui-done-keyword-follows-buffer-keywords ()
  (nsa/org-agenda-ui-test--with-agenda-file
    (with-current-buffer (find-file-noselect file)
      (org-mode)
      (should (equal (nsa/agenda-ui--done-keyword) "DONE")))))

(ert-deftest nsa/org-agenda-ui-state-spec-styles-active-keywords ()
  (let ((loop (nsa/agenda-ui--state-spec "LOOP"))
        (done (nsa/agenda-ui--state-spec "DONE")))
    (should (member 'font-lock-type-face (plist-get loop :inherit)))
    (should (plist-get loop :box))
    (should (plist-get done :strike-through))))

(ert-deftest nsa/org-agenda-ui-fontify-adds-keyword-and-header-overlays ()
  (with-temp-buffer
    (delay-mode-hooks (org-agenda-mode))
    ;; A super-agenda group header line.
    (insert "  ⚠ Overdue\n")
    (let ((header-start (point-min)))
      (put-text-property header-start (1+ header-start)
                         'face 'org-super-agenda-header-face))
    ;; An item line carrying the usual agenda text properties.
    (insert "  weekly:    7:30-16:40 Scheduled:  LOOP  GetGo work   :work:getgo:\n")
    (let ((marker (copy-marker (point) t)))
      (put-text-property (line-beginning-position 0) (point)
                         'org-hd-marker marker))
    (nsa/agenda-ui-fontify)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (= (length overlays) 2))
      (dolist (ov overlays)
        (should (overlay-get ov 'nsa-agenda-ui))))
    ;; The LOOP token is pill-boxed with its state face.
    (goto-char (point-min))
    (re-search-forward "LOOP")
    (let ((spec (overlay-get (car (overlays-at (match-beginning 0))) 'face)))
      (should (member 'font-lock-type-face (plist-get spec :inherit))))
    ;; Running again must not duplicate overlays.
    (nsa/agenda-ui-fontify)
    (should (= (length (overlays-in (point-min) (point-max))) 2))))

(ert-deftest nsa/org-agenda-ui-fontify-ignores-tag-tokens ()
  (with-temp-buffer
    (delay-mode-hooks (org-agenda-mode))
    (insert "  daily:    8:00  TODO  File taxes   :todo:no:\n")
    (let ((marker (copy-marker (point) t)))
      (put-text-property (line-beginning-position 0) (point)
                         'org-hd-marker marker))
    (nsa/agenda-ui-fontify)
    ;; Only the leading TODO keyword is overlaid, never the :todo:no: tag.
    (should (= (length (overlays-in (point-min) (point-max))) 1))))

;;; org-agenda-ui-test.el ends here
