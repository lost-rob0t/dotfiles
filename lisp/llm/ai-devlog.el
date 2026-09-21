;;; ai-devlog.el --- GitHub/Forgejo devlog capture for gptel -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-id)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'url-parse)

(defgroup ai/devlog nil
  "Org-roam development logging backed by GitHub, Forgejo, and Prolog."
  :group 'applications
  :prefix "ai/devlog-")

(defcustom ai/devlog-changelog-heading "changlog"
  "Top-level daily heading used for normalized change events."
  :type 'string
  :group 'ai/devlog)

(defcustom ai/devlog-devlog-heading "devlog"
  "Top-level daily heading used for detailed development-session logs."
  :type 'string
  :group 'ai/devlog)

(defcustom ai/devlog-default-since-hours 24
  "Default lookback used by `ai/devlog-sync'."
  :type 'integer
  :group 'ai/devlog)

(defcustom ai/devlog-max-items 40
  "Default maximum number of items collected from each remote provider."
  :type 'integer
  :group 'ai/devlog)

(defcustom ai/devlog-tea-login nil
  "Optional tea login name.  Nil lets tea use its configured default/login inference."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'ai/devlog)

(defcustom ai/devlog-tea-host-regexp
  "\\(?:forgejo\\|gitea\\|git\\.starintel\\.actor\\|starintel\\.actor\\)"
  "Regexp identifying remotes that should be queried through tea."
  :type 'regexp
  :group 'ai/devlog)

(defcustom ai/devlog-command-timeout 20
  "Maximum seconds allowed for one gh, tea, or git subprocess."
  :type 'integer
  :group 'ai/devlog)

(defcustom ai/devlog-kb-file
  (expand-file-name
   "ai-devlog/devlog-facts.pl"
   (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache/")))
  "Generated Prolog fact file derived from Org-roam dailies."
  :type 'file
  :group 'ai/devlog)

(defcustom ai/devlog-confirm-tool-call nil
  "When non-nil, gptel asks before the devlog tool mutates the daily note."
  :type 'boolean
  :group 'ai/devlog)

(defconst ai/devlog-event-schema "org-prolog-devlog/v1")
(defconst ai/devlog-kb-schema "org-prolog-devlog-kb/v1")

(defconst ai/devlog-agent-instructions
  "\n\nDevlog rules:\n- DevlogSync writes auditable GitHub/Forgejo/local Git activity into today's Org-roam daily under `changlog` and `devlog`, then rebuilds the Prolog KB.\n- Pass a concise factual `note` describing the development work; never invent provider state.\n- Use DevlogQuery to answer questions from the derived symbolic development history.\n")

(defun ai/devlog--result (&rest pairs)
  "Serialize alternating PAIRS as gptel-safe JSON text."
  (let (object)
    (while pairs
      (push (cons (pop pairs) (pop pairs)) object))
    (string-to-multibyte
     (decode-coding-string
      (json-serialize (nreverse object)
                      :null-object nil :false-object :json-false)
      'utf-8 t))))

(defun ai/devlog--run (program arguments directory &optional timeout)
  "Run PROGRAM with ARGUMENTS in DIRECTORY and return a plist result."
  (if-let ((executable (executable-find program)))
      (let* ((stdout (generate-new-buffer " *ai-devlog-stdout*"))
             (stderr (generate-new-buffer " *ai-devlog-stderr*"))
             (default-directory (file-name-as-directory (expand-file-name directory)))
             (process-environment (copy-sequence process-environment))
             (deadline (+ (float-time) (or timeout ai/devlog-command-timeout)))
             (process nil)
             timed-out)
        (setenv "GH_PROMPT_DISABLED" "1")
        (setenv "GIT_TERMINAL_PROMPT" "0")
        (unwind-protect
            (progn
              (setq process
                    (make-process
                     :name "ai-devlog"
                     :buffer stdout
                     :stderr stderr
                     :command (cons executable arguments)
                     :connection-type 'pipe
                     :noquery t))
              (while (and (process-live-p process) (< (float-time) deadline))
                (accept-process-output process 0.05))
              (when (process-live-p process)
                (setq timed-out t)
                (delete-process process))
              (while (accept-process-output process 0.02))
              (list :ok (and (not timed-out) (zerop (process-exit-status process)))
                    :exit-code (unless timed-out (process-exit-status process))
                    :timed-out timed-out
                    :stdout (with-current-buffer stdout
                              (buffer-substring-no-properties (point-min) (point-max)))
                    :stderr (with-current-buffer stderr
                              (buffer-substring-no-properties (point-min) (point-max)))
                    :command (cons program arguments)))
          (when (buffer-live-p stdout) (kill-buffer stdout))
          (when (buffer-live-p stderr) (kill-buffer stderr))))
    (list :ok nil :missing t :stdout "" :stderr (format "%s not found" program)
          :command (cons program arguments))))

(defun ai/devlog--project-root (&optional directory)
  "Resolve DIRECTORY to a Git/project root."
  (let* ((default-directory (file-name-as-directory
                             (expand-file-name (or directory default-directory))))
         (project (project-current nil default-directory)))
    (or (and project (expand-file-name (project-root project)))
        (locate-dominating-file default-directory ".git")
        (user-error "Not inside a project: %s" default-directory))))

(defun ai/devlog--roam-root ()
  "Return the active Org-roam root without requiring org-roam."
  (file-name-as-directory
   (expand-file-name
    (or (and (boundp 'org-roam-directory) org-roam-directory)
        (getenv "KB_ROAM_ROOT")
        (getenv "GPT_TODOS_NOTES_DIR")
        "~/Documents/Notes/org"))))

(defun ai/devlog--dailies-directory ()
  "Return the dailies directory relative to the roam root."
  (or (and (boundp 'org-roam-dailies-directory) org-roam-dailies-directory)
      "daily"))

(defun ai/devlog--daily-file (&optional time)
  "Return the Org-roam daily path for TIME."
  (expand-file-name
   (format "%s/%s.org"
           (directory-file-name (ai/devlog--dailies-directory))
           (format-time-string "%Y-%m-%d" (or time (current-time))))
   (ai/devlog--roam-root)))

(defun ai/devlog--ensure-daily-file (file)
  "Create FILE with private Org-roam metadata when it does not exist."
  (make-directory (file-name-directory file) t)
  (unless (file-exists-p file)
    (with-temp-file file
      (insert ":PROPERTIES:\n:ID: " (if (fboundp 'org-id-new)
                                      (org-id-new)
                                    (secure-hash 'sha1 (format "%s%s" file (float-time))))
              "\n:END:\n")
      (insert "#+TITLE: " (file-name-base file) "\n")
      (insert "#+ROAM_SCHEMA: org-roam-meta/v1\n")
      (insert "#+ROAM_KIND: daily\n#+ROAM_VISIBILITY: private\n")
      (insert "#+CENSOR_PROFILE: strict\n")
      (insert "#+FILETAGS: :development:devlog:git:\n\n")))
  file)

(defun ai/devlog--parse-remote-url (remote-name url)
  "Return normalized remote metadata for REMOTE-NAME and URL."
  (condition-case nil
      (let (host path)
        (cond
         ((string-match "\\`[^/@:]+@\\([^:]+\\):\\(.+\\)\\'" url)
          (setq host (match-string 1 url)
                path (match-string 2 url)))
         ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" url)
          (let ((parsed (url-generic-parse-url url)))
            (setq host (url-host parsed)
                  path (string-remove-prefix "/" (url-filename parsed)))))
         (t nil))
        (when (and host path)
          (setq path (replace-regexp-in-string "\\.git\\'" "" path))
          (setq path (replace-regexp-in-string "\\`/+\\|/+\\'" "" path))
          (when (string-match "\\`\\([^/]+/[^/]+\\)" path)
            `((remote . ,remote-name)
              (host . ,(downcase host))
              (repo . ,(match-string 1 path))
              (url . ,url)))))
    (error nil)))

(defun ai/devlog--git-remotes (root)
  "Return normalized fetch remotes for ROOT."
  (let ((result (ai/devlog--run "git" '("remote" "-v") root))
        remotes)
    (when (plist-get result :ok)
      (dolist (line (split-string (plist-get result :stdout) "\n" t))
        (when (string-match "\\`\\([^[:space:]]+\\)[[:space:]]+\\([^[:space:]]+\\)[[:space:]]+(fetch)\\'" line)
          (when-let ((parsed (ai/devlog--parse-remote-url
                              (match-string 1 line) (match-string 2 line))))
            (push parsed remotes)))))
    (nreverse (delete-dups remotes))))

(defun ai/devlog--provider-repositories (root)
  "Return GitHub and tea repository metadata discovered from ROOT remotes."
  (let ((remotes (ai/devlog--git-remotes root)) github tea)
    (dolist (remote remotes)
      (let ((host (alist-get 'host remote)))
        (cond
         ((and (not github) (string= host "github.com")) (setq github remote))
         ((and (not tea) (string-match-p ai/devlog-tea-host-regexp host))
          (setq tea remote)))))
    `((github . ,github) (tea . ,tea) (all . ,remotes))))

(defun ai/devlog--json (text)
  "Parse JSON TEXT into alists/lists, returning nil on invalid input."
  (condition-case nil
      (json-parse-string text :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil)
    (error nil)))

(defun ai/devlog--field (object &rest keys)
  "Return the first non-nil value from OBJECT matching KEYS."
  (cl-loop for key in keys
           for symbol = (if (symbolp key) key (intern key))
           for value = (or (alist-get symbol object)
                           (alist-get (symbol-name symbol) object nil nil #'equal))
           when value return value))

(defun ai/devlog--string (value)
  "Normalize VALUE to a single-line string."
  (let ((text (cond ((null value) "")
                    ((stringp value) value)
                    ((numberp value) (number-to-string value))
                    ((eq value t) "true")
                    (t (format "%s