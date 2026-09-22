;;; qtile-topics.el --- Qtile topics dashboard and picker -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'qtile-ui)
(require 'qtile-ui-org)

(defconst qtile-topics-cancel-label "[Cancel]"
  "Completion choice used to leave the topic picker without switching.")

(defun qtile-topics--get (key object)
  (or (alist-get key object nil nil #'equal)
      (alist-get (symbol-name key) object nil nil #'equal)
      (alist-get (intern (symbol-name key)) object nil nil #'eq)))

(defun qtile-topics--call (action topic)
  "Invoke the Qtile IPC ACTION for TOPIC via `qtile cmd-obj'."
  (start-process "qtile-topic-cmd" nil "qtile" "cmd-obj" "-o" "cmd"
                 "-f" action "-a" topic))

(defun qtile-topics--topic-name (topic)
  "Return the group name from a JSON TOPIC pair."
  (cond
   ((vectorp topic) (aref topic 0))
   ((consp topic) (car topic))
   (t topic)))

(defun qtile-topics--topic-label (topic)
  "Return the display label from a JSON TOPIC pair."
  (cond
   ((vectorp topic)
    (if (> (length topic) 1) (aref topic 1) (aref topic 0)))
   ((consp topic) (or (cdr-safe topic) (car topic)))
   (t topic)))

(defun qtile-topics-switch (topic)
  "Switch to the TOPIC group."
  (interactive)
  (qtile-topics--call "focus_topic" topic)
  (qtile-ui-close-current))

(defun qtile-topics-send-window (topic)
  "Send the focused Qtile window to the TOPIC group."
  (interactive)
  (qtile-topics--call "send_to_topic" topic)
  (qtile-ui-close-current))

(defun qtile-topics-close (topic)
  "Close the TOPIC group; its windows move to another group."
  (interactive)
  (qtile-topics--call "remove_topic" topic))

(defun qtile-topics-create ()
  "Prompt for a topic name and register it in Qtile at runtime."
  (interactive)
  (let ((name (string-trim (read-string "Topic name: "))))
    (unless (string-empty-p name)
      (qtile-topics--call "create_topic" name)
      (qtile-ui-close-current))))

(defun qtile-topics-render (params)
  "Render the topics dashboard from structured popup PARAMS."
  (switch-to-buffer (get-buffer-create "*Qtile Topics*"))
  (unless (derived-mode-p 'qtile-topics-mode)
    (qtile-topics-mode))
  (let ((topics (append (qtile-topics--get 'topics (qtile-ui-args params)) nil))
        (inhibit-read-only t))
    (erase-buffer)
    (qtile-ui-org-heading "TOPICS")
    (qtile-ui-org-muted
     "c create  q/Escape close.  Actions: switch, send window, close.\n\n")
    (if topics
        (dolist (topic topics)
          (let ((name (qtile-topics--topic-name topic))
                (label (qtile-topics--topic-label topic)))
            (insert (propertize (format "%-16s" label) 'face 'qtile-ui-org-value))
            (qtile-ui-org-button
             "[switch]"
             (lambda (_button) (qtile-topics-switch name)))
            (insert " ")
            (qtile-ui-org-button
             "[send]"
             (lambda (_button) (qtile-topics-send-window name)))
            (insert " ")
            (qtile-ui-org-button
             "[close]"
             (lambda (_button) (qtile-topics-close name)))
            (insert "\n")))
      (qtile-ui-org-muted "No topic groups exist yet.\n"))
    (qtile-ui-org-separator)
    (qtile-ui-org-button "[ create topic ]"
                         (lambda (_button) (qtile-topics-create)))
    (insert "\n")
    (goto-char (point-min))))

(define-derived-mode qtile-topics-mode special-mode "Qtile-Topics"
  "Qtile topics dashboard."
  (qtile-ui-prepare-buffer)
  (qtile-ui-bind-dismiss)
  (local-set-key (kbd "c") #'qtile-topics-create)
  (setq-local buffer-read-only t))

(defun qtile-topics-open (params)
  "Open or reuse the topics dashboard from a Qtile popup."
  (qtile-topics-render params)
  (current-buffer))

(defun qtile-topics-pick (params)
  "Return a topic chosen with `completing-read' from a Qtile popup.

Mirrors the workflow picker: the selected topic name is printed to
stdout for the Qtile worker thread; Cancel or quitting prints nothing."
  (let* ((args (qtile-ui-args params))
         (choices (append (qtile-topics--get 'choices args) nil))
         (prompt (or (qtile-topics--get 'prompt args) "Topic: "))
         (picker-choices (append choices (list qtile-topics-cancel-label))))
    (switch-to-buffer (get-buffer-create "*Qtile Topic Pick*"))
    (qtile-ui-prepare-buffer)
    (erase-buffer)
    (qtile-ui-org-heading "TOPICS")
    (qtile-ui-org-muted (format "%s  Escape/C-g cancels.\n\n" prompt))
    (condition-case nil
        (let ((selected
               (completing-read prompt picker-choices nil t
                                nil nil (car choices))))
          (if (equal selected qtile-topics-cancel-label)
              (progn
                (qtile-ui-close-current)
                nil)
            (prog1 selected
              (qtile-ui-close-current))))
      (quit
       (qtile-ui-close-current)
       nil))))

(provide 'qtile-topics)
;;; qtile-topics.el ends here
