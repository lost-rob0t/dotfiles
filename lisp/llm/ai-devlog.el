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

(defun ai/devlog--project-root (&optional direc