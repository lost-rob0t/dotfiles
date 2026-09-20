;;; crew.el --- Roles and ARADR crews over the OpenCode workspace -*- lexical-binding: t; -*-
;; Generated from crew.org. Edit the Org source, then tangle.
(require 'opencode-workspace)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup crew nil "Named, bounded agent crews." :group 'tools)
(defcustom crew-program "opencode-worker" "Canonical worker wrapper, not raw OpenCode." :type 'string)
(defcustom crew-policy-program "swipl" "SWI-Prolog used for closed policy operations." :type 'string)
(defcustom crew-source-root opencode-workspace--root "Git checkout containing crew contracts." :type 'directory)
(defcustom crew-global-parallel 16 "Process ceiling across all crews." :type 'integer)
(defconst crew-protocol "ZARA-CREW/1")
(defconst crew--phases '(research adversarial_review analysis design design_review promotion develop verify completed))
(defvar crew--roles (make-hash-table :test #'eq))
(defvar crew--teams (make-hash-table :test #'eq))
(defvar crew--members (make-hash-table :test #'equal))
(defvar crew--requests (make-hash-table :test #'equal))
(defvar crew--serial 0)
(defvar crew--drain-timer nil)
(defvar crew--drain-offset 0)
(defvar crew--launching nil)
(defvar crew--policy-processes 0)
(defvar crew-expert-check-function nil
  "Optional trusted function (EXPERT-IDS WORKER) returning a runtime report.
Report must contain runtime, revision and experts with observed loaded/active
states. Nil means unavailable. Never derive this report from model prose.")

(cl-defstruct crew--team name project model roles max-agents parallel depth children
  remaining run (epoch 0) (phase 'research) receipts pins stopped)
(cl-defstruct crew--member id team role parent depth remaining cancelled output reported)

(defun crew--id-p (value)
  (and (symbolp value) (opencode-workspace--name-p (symbol-name value))))
(defun crew--strings-p (values)
  (and (listp values) (<= (length values) 32)
       (cl-every (lambda (x) (and (stringp x) (<= (string-bytes x) 256))) values)))
(defun crew--positive (value upper)
  (and (integerp value) (> value 0) (<= value upper)))
(defun crew--team (name)
  (or (gethash name crew--teams) (user-error "Unknown crew: %s" name)))
(defun crew--role (team name)
  (or (cdr (assq name (crew--team-roles team))) (user-error "Undeclared crew role: %s" name)))

(defun crew-register-role (name &rest properties)
  "Register NAME with goal, skills, experts, capabilities and allowed delegates.
Definitions are trusted Elisp configuration, never model-produced executable code."
  (unless (and (crew--id-p name) (stringp (plist-get properties :goal))
               (<= (string-bytes (plist-get properties :goal)) 4096)
               (crew--strings-p (plist-get properties :skills))
               (crew--strings-p (plist-get properties :experts))
               (crew--strings-p (plist-get properties :capabilities))
               (cl-every #'crew--id-p (plist-get properties :delegates))
               (cl-every (lambda (phase) (memq phase crew--phases)) (plist-get properties :phases)))
    (user-error "Invalid role definition"))
  (puthash name (copy-tree properties) crew--roles)
  name)

(defmacro defcrew-role (name &rest properties)
  "Define role NAME. Property values are ordinary trusted Elisp expressions."
  `(crew-register-role ',name ,@properties))

(defun crew--pin (relative)
  (let ((path (expand-file-name relative crew-source-root)))
    (unless (and (file-regular-p path) (not (file-symlink-p path))
                 (file-in-directory-p (file-truename path) (file-truename crew-source-root))
                 (<= (file-attribute-size (file-attributes path)) 32768))
      (user-error "Missing, unsafe or oversized crew source: %s" relative))
    (with-temp-buffer
      (insert-file-contents path)
      (list relative (secure-hash 'sha256 (current-buffer)) (buffer-string)))))

(cl-defun crew-register (name &key project model roles (max-agents 128)
                             (parallel 8) (max-depth 4) (max-children 8) (turn-budget 512))
  "Define an ARADR crew without launching workers or invoking providers."
  (unless (and (crew--id-p name) (not (gethash name crew--teams))
               (stringp project) (not (file-remote-p project)) (file-directory-p project)
               (stringp model) (not (string-empty-p model))
               (listp roles) roles (<= (length roles) 32)
               (crew--positive max-agents 512) (crew--positive parallel max-agents)
               (crew--positive max-depth 16) (crew--positive max-children 128)
               (crew--positive turn-budget 100000))
    (user-error "Invalid or already defined crew"))
  (let* ((snapshots (mapcar (lambda (role)
                            (cons role (copy-tree (or (gethash role crew--roles)
                                                    (user-error "Unknown role: %s" role))))) roles))
         (skills (delete-dups (cons "crew-expert"
                                   (cl-mapcan (lambda (r) (copy-sequence (plist-get (cdr r) :skills))) snapshots))))
         (pins (list (crew--pin "contracts/zara-crew-v1/contract.md")
                     (crew--pin ".prolog/kb/crew_protocol_v1.pl"))))
    (dolist (role snapshots)
      (dolist (delegate (plist-get (cdr role) :delegates))
        (unless (assq delegate snapshots) (user-error "Delegate role is outside crew: %s" delegate))))
    (dolist (skill skills)
      (unless (and (stringp skill) (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" skill))
        (user-error "Invalid skill name"))
      (push (crew--pin (format ".opencode/skills/%s/SKILL.md" skill)) pins))
    (puthash name (make-crew--team :name name :project (file-truename project) :model model
                   :roles snapshots :max-agents max-agents :parallel parallel :depth max-depth
                   :children max-children :remaining turn-budget :pins pins
                   :run (secure-hash 'sha256 (format "%s:%s:%s" name (float-time) (random)))) crew--teams))
  name)

(defmacro defcrew (name &rest properties)
  "Define crew NAME. No worker starts until `crew-spawn' or `crew-kickoff'."
  `(crew-register ',name ,@properties))

(defun crew--members (team)
  (cl-remove-if-not (lambda (m) (eq team (crew--member-team m))) (hash-table-values crew--members)))
(defun crew--running (&optional team)
  (cl-count-if (lambda (m)
                (and (or (null team) (eq team (crew--member-team m)))
                     (eq 'running (opencode-workspace-worker-state
                                   (opencode-workspace--worker (crew--member-id m))))))
              (hash-table-values crew--members)))

(defun crew--policy (request callback)
  "Run a bounded, fixed Prolog policy program asynchronously. No payload eval."
  (when (>= crew--policy-processes 32) (user-error "Policy queue is full"))
  (let ((input "") (finished nil) timer process)
    (cl-labels ((finish (result)
                  (unless finished
                    (setq finished t)
                    (cl-decf crew--policy-processes)
                    (when (timerp timer) (cancel-timer timer))
                    (when (process-live-p process) (delete-process process))
                    (funcall callback result))))
      (cl-incf crew--policy-processes)
      (condition-case nil
          (progn
            (setq process
                  (make-process
                   :name "crew-policy" :command (list crew-policy-program "-q" "-s"
                                (expand-file-name "scripts/crew-policy.pl" crew-source-root))
                   :connection-type 'pipe :coding 'utf-8-unix :noquery t
                   :filter (lambda (_ chunk)
                             (setq input (concat input chunk))
                             (when (> (string-bytes input) 65536) (finish nil)))
                   :sentinel (lambda (p _)
                               (when (memq (process-status p) '(exit signal))
                                 (finish
                                  (and (eq 'exit (process-status p)) (= 0 (process-exit-status p))
                                       (condition-case nil
                                           (json-parse-string input :object-type 'alist :array-type 'list
                                                              :null-object nil :false-object nil)
                                         (error nil))))))))
            (setq timer (run-at-time 5 nil (lambda () (finish nil))))
            (process-send-string process (concat (json-encode request) "\n"))
            (process-send-eof process))
        (error (finish nil))))))

(defun crew--spawn-context (team role parent turns)
  (when (or (crew--team-stopped team) (eq 'completed (crew--team-phase team)))
    (user-error "Crew stopped or completed"))
  (let* ((spec (crew--role team role))
         (member (and parent (or (gethash parent crew--members) (user-error "Unknown parent"))))
         (members (crew--members team))
         (parent-spec (and member (crew--role team (crew--member-role member)))))
    (when (and (plist-get spec :phases)
               (not (memq (crew--team-phase team) (plist-get spec :phases))))
      (user-error "Role is not admitted in the current ARADR phase"))
    (unless (and (crew--positive turns 100000)
                 (< (length members) (crew--team-max-agents team))) (user-error "Agent or turn limit"))
    (when member
      (unless (and (eq team (crew--member-team member)) (not (crew--member-cancelled member))
                   (member "crew.spawn" (plist-get parent-spec :capabilities))
                   (memq (opencode-workspace-worker-state (opencode-workspace--worker parent)) '(idle running)))
        (user-error "Parent is stopped, cross-crew or cannot delegate")))
    `((op . "spawn")
      (parent . ((delegates . ,(vconcat (mapcar #'symbol-name
                                      (if member (plist-get parent-spec :delegates)
                                        (mapcar #'car (crew--team-roles team))))))
                 (capabilities . ,(vconcat (if member (plist-get parent-spec :capabilities)
                                            (plist-get spec :capabilities))))
                 (remaining . ,(if member (crew--member-remaining member) (crew--team-remaining team)))
                 (depth . ,(if member (crew--member-depth member) -1))
                 (children . ,(if member (cl-count parent members :key #'crew--member-parent :test #'equal) 0))))
      (child . ((role . ,(symbol-name role)) (capabilities . ,(vconcat (plist-get spec :capabilities))) (turns . ,turns)))
      (limits . ((max_depth . ,(crew--team-depth team)) (max_children . ,(crew--team-children team))
                 (max_agents . ,(crew--team-max-agents team)) (members . ,(length members)))))))

(cl-defun crew-spawn (name role task &key parent (turns 8) callback)
  "Ask Prolog to admit a new ROLE instance. CALLBACK receives ID or nil.
The proposal is asynchronous; model prose cannot bypass admission."
  (unless (and (stringp task) (> (string-bytes task) 0) (<= (string-bytes task) 8192))
    (user-error "Task must be 1-8192 bytes"))
  (let* ((team (crew--team name)) (epoch (crew--team-epoch team))
         (request (crew--spawn-context team role parent turns))
         (parent-generation (and parent (opencode-workspace-worker-generation
                                         (opencode-workspace--worker parent)))))
    (crew--policy
     request
     (lambda (answer)
       (let (id)
         (condition-case err
             (progn
               (unless (and (eq t (alist-get 'ok answer)) (= epoch (crew--team-epoch team))
                            (or (null parent) (= parent-generation (opencode-workspace-worker-generation
                                                                  (opencode-workspace--worker parent)))))
                 (user-error "Policy denied/unavailable or stale parent"))
               ;; Recompute after async admission: concurrent proposals cannot overspend.
               (unless (equal request (crew--spawn-context team role parent turns))
                 (user-error "Admission snapshot changed; submit a fresh proposal"))
               (setq id (format "crew-%s-%d" name (cl-incf crew--serial)))
               (let* ((member (and parent (gethash parent crew--members)))
                      (worker (opencode-workspace-add-agent id (if parent (opencode-workspace-worker-directory (opencode-workspace--worker parent)) (crew--team-project team))
                                  (or (plist-get (crew--role team role) :model) (crew--team-model team)) "crew-worker")))
                 (if member (cl-decf (crew--member-remaining member) turns)
                   (cl-decf (crew--team-remaining team) turns))
                 (puthash id (make-crew--member :id id :team team :role role :parent parent
                              :depth (if member (1+ (crew--member-depth member)) 0) :remaining turns) crew--members)
                 (crew-send id task)
                 (ignore worker)))
           (error (when (and id (gethash id crew--members)) (crew-cancel id))
                  (setq id nil) (message "Crew admission: %s" (error-message-string err))))
         (when (and parent (gethash parent crew--members)
                    (not (crew--member-cancelled (gethash parent crew--members))))
           (condition-case nil
               (crew-send parent (format "Spawn receipt: child=%s; role=%s; status=%s"
                                         (or id "none") role (if id "admitted" "denied")))
             (error nil)))
         (when callback (funcall callback id)))))))

(defun crew-kickoff (name role tasks &optional turns)
  "Spawn one independent ROLE instance for each task, with bounded admission."
  (unless (and (listp tasks) (<= (length tasks) 512)) (user-error "At most 512 tasks"))
  ;; Serial admission, parallel execution: avoid racing a shared budget snapshot.
  (when tasks
    (crew-spawn name role (car tasks) :turns (or turns 8)
                :callback (lambda (id)
                            (when id (crew-kickoff name role (cdr tasks) turns))))))

(defun crew--packet (member task)
  (let* ((team (crew--member-team member)) (spec (crew--role team (crew--member-role member))))
    `((protocol . ,crew-protocol) (crew . ,(symbol-name (crew--team-name team)))
      (agent . ,(crew--member-id member)) (parent . ,(crew--member-parent member))
      (run . ,(crew--team-run team)) (epoch . ,(crew--team-epoch team))
      (files . ,(vconcat (opencode-workspace--files
                          (opencode-workspace--worker (crew--member-id member)))))
      (role . ,(symbol-name (crew--member-role member))) (goal . ,(plist-get spec :goal))
      (phase . ,(symbol-name (crew--team-phase team)))
      (requested_experts . ,(vconcat (plist-get spec :experts)))
      (contracts . ,(vconcat (mapcar (lambda (pin) `((path . ,(car pin)) (sha256 . ,(cadr pin))))
                                     (crew--team-pins team))))
      (task . ,task))))

(defun crew-send (id task)
  "Send a task with pinned role, skill and protocol context."
  (let* ((member (or (gethash id crew--members) (user-error "Unknown crew member")))
         (team (crew--member-team member)))
    (when (or (crew--team-stopped team) (crew--member-cancelled member)) (user-error "Crew/member stopped"))
    (opencode-workspace-send id (json-encode (crew--packet member task)))))

(defun crew--schedule ()
  (unless (timerp crew--drain-timer)
    (setq crew--drain-timer
          (run-at-time 0 nil
            (lambda ()
              (setq crew--drain-timer nil)
              (let* ((members (hash-table-values crew--members)) (count (length members)))
                (when (> count 0)
                  (setq crew--drain-offset (mod (1+ crew--drain-offset) count))
                  (dolist (member (append (nthcdr crew--drain-offset members)
                                         (cl-subseq members 0 crew--drain-offset)))
                    (opencode-workspace--pump (opencode-workspace--worker (crew--member-id member)))))))))))

(defun crew--pump (original worker)
  (let ((member (gethash (opencode-workspace-worker-id worker) crew--members)))
    (if (null member) (funcall original worker)
      (let* ((team (crew--member-team member)) (spec (crew--role team (crew--member-role member))))
        (when (and (eq 'idle (opencode-workspace-worker-state worker))
                   (opencode-workspace-worker-inbox worker)
                   (not (crew--team-stopped team)) (not (crew--member-cancelled member))
                   (< (crew--running) crew-global-parallel) (< (crew--running team) (crew--team-parallel team)))
          (condition-case err
              (progn
                (when (<= (crew--member-remaining member) 0) (user-error "Member turn allocation exhausted"))
                (dolist (pin (crew--team-pins team))
                  (unless (equal (cadr pin) (cadr (crew--pin (car pin))))
                    (user-error "Pinned contract/KB/skill changed; define a fresh crew")))
                (when (plist-get spec :experts)
                  (let ((report (and crew-expert-check-function
                                     (funcall crew-expert-check-function (plist-get spec :experts) worker))))
                    (unless report (user-error "Required expert runtime unavailable"))
                    (opencode-workspace--accept-experts worker report)
                    (dolist (id (plist-get spec :experts))
                      (unless (cl-find-if (lambda (e) (and (equal id (alist-get 'id e))
                                                           (member (alist-get 'state e) '("loaded" "active"))))
                                         (alist-get 'experts report))
                        (user-error "Required expert not loaded: %s" id)))))
                (let* ((process-environment (copy-sequence process-environment))
                       (opencode-workspace-program crew-program)
                       (opencode-workspace-program-prefix
                        (list "--format" "json" "--max-retries" "0" "--mode" "readonly"
                              "--role" (symbol-name (crew--member-role member))
                              "--dir" (opencode-workspace-worker-directory worker)))
                       (opencode-workspace-peer-program (expand-file-name "scripts/crew-client.py" crew-source-root))
                       (crew--launching t)
                       (context (mapconcat #'caddr
                                  (cl-remove-if-not
                                   (lambda (pin)
                                     (or (not (string-prefix-p ".opencode/skills/" (car pin)))
                                         (cl-some (lambda (skill)
                                                    (equal (car pin) (format ".opencode/skills/%s/SKILL.md" skill)))
                                                  (cons "crew-expert" (plist-get spec :skills)))))
                                   (crew--team-pins team)) "\n\n"))
                       (permission (copy-tree '(("*" . "deny") ("read" . "deny") ("glob" . "deny")
                                     ("grep" . "deny") ("list" . "deny") ("crew" . "deny")
                                     ("external_directory" . "deny") ("task" . "deny")
                                     ("bash" . "deny") ("edit" . "deny"))))
                       (configuration `((share . "disabled")
                         (agent . ((crew-worker . ((mode . "primary")
                           (description . "Pinned ARADR crew member") (prompt . ,context)
                           (permission . ,permission))))))))
                  (when (member "workspace.read" (plist-get spec :capabilities))
                    (dolist (key '("read" "glob" "grep" "list"))
                      (setcdr (assoc key permission) "allow")))
                  (when (or (member "crew.message" (plist-get spec :capabilities))
                            (member "crew.spawn" (plist-get spec :capabilities)))
                    (setcdr (assoc "crew" permission) "allow"))
                  (when (> (string-bytes context) 65536) (user-error "Pinned context too large"))
                  (setenv "OPENCODE_CONFIG_DIR" (expand-file-name ".opencode" crew-source-root))
                  (setenv "OPENCODE_CONFIG_CONTENT" (json-encode configuration))
                  (setenv "CREW_CLIENT_SCRIPT" opencode-workspace-peer-program)
                  (setenv "OPENCODE_CREW_NAME" (symbol-name (crew--team-name team)))
                  (setenv "OPENCODE_CREW_RUN" (crew--team-run team))
                  (setenv "OPENCODE_CREW_EPOCH" (number-to-string (crew--team-epoch team)))
                  (setf (crew--member-output member) "" (crew--member-reported member) nil)
                  (cl-decf (crew--member-remaining member))
                  (funcall original worker)))
            (error (opencode-workspace--stop worker 'blocked (error-message-string err)))))))))

(defun crew--files (original worker)
  ;; The existing wrapper has no --file flag. Attach references, not hidden bytes.
  (if crew--launching nil (funcall original worker)))
(defun crew--capture (worker event)
  (when-let ((member (gethash (opencode-workspace-worker-id worker) crew--members)))
    (when (equal "text" (alist-get 'type event))
      (let ((text (concat (or (crew--member-output member) "")
                          (alist-get 'text (alist-get 'part event)))))
        (setf (crew--member-output member) (substring text 0 (min (length text) 2048)))))))

(defun crew--completion (original worker process generation)
  (let* ((member (gethash (opencode-workspace-worker-id worker) crew--members))
         (valid (and member (opencode-workspace--current-p worker process generation)
                     (memq (process-status process) '(exit signal))))
         (text (and member (crew--member-output member)))
         (success (and valid (= 0 (process-exit-status process))
                       (opencode-workspace-worker-finished worker)
                       (string-empty-p (string-trim (opencode-workspace-worker-input worker))))))
    (prog1 (funcall original worker process generation)
      (when (and valid (crew--member-parent member)
                 (not (equal generation (crew--member-reported member))))
        (setf (crew--member-reported member) generation)
        (let* ((parent (crew--member-parent member)) (owner (gethash parent crew--members)))
          (when (and owner (not (crew--member-cancelled owner)))
            (condition-case nil
                (crew-send parent (format "Child result (unverified context): %s; state=%s; %s"
                                          (crew--member-id member) (if success "completed" "failed")
                                          (or text "no output")))
              (error nil)))))
      (crew--schedule))))
(defun crew--wake-after (&rest _) (crew--schedule))

(defun crew-cancel (id)
  "Cancel ID and its descendants; retain lineage and knowledge history."
  (interactive (list (completing-read "Crew agent: " (hash-table-keys crew--members) nil t)))
  (let ((member (or (gethash id crew--members) (user-error "Unknown member"))))
    (setf (crew--member-cancelled member) t)
    (maphash (lambda (child candidate)
               (when (equal id (crew--member-parent candidate)) (crew-cancel child))) crew--members)
    (opencode-workspace-cancel id)))

(defun crew-stop (name)
  "Stop a crew and fence pending admissions. Do not delete its history."
  (interactive (list (intern (completing-read "Crew: " (hash-table-keys crew--teams) nil t))))
  (let ((team (crew--team name)))
    (setf (crew--team-stopped team) t)
    (cl-incf (crew--team-epoch team))
    (dolist (member (crew--members team))
      (unless (crew--member-parent member) (crew-cancel (crew--member-id member))))))

(defun crew-advance (name receipt callback)
  "Advance one ARADR phase using a trusted exact-artifact RECEIPT.
Not exported to the peer protocol. Approval/review is never inferred from text."
  (let* ((team (crew--team name)) (phase (crew--team-phase team))
         (epoch (crew--team-epoch team)) (next (cadr (memq phase crew--phases))))
    (unless next (user-error "Crew already completed"))
    (when (or (> (crew--running team) 0)
              (cl-some (lambda (m) (opencode-workspace-worker-inbox
                                    (opencode-workspace--worker (crew--member-id m)))) (crew--members team)))
      (user-error "Finish or cancel all phase work before advancing"))
    (crew--policy `((op . "transition") (from . ,(symbol-name phase)) (to . ,(symbol-name next)) (receipt . ,receipt))
      (lambda (answer)
        (let ((accepted (and (eq t (alist-get 'ok answer)) (= epoch (crew--team-epoch team))
                             (= 0 (crew--running team))
                             (not (cl-some (lambda (m) (opencode-workspace-worker-inbox (opencode-workspace--worker (crew--member-id m)))) (crew--members team)))
                             (eq phase (crew--team-phase team)) (not (crew--team-stopped team)))))
          (when accepted
            (push (copy-tree receipt) (crew--team-receipts team))
            (setf (crew--team-phase team) next)
            (cl-incf (crew--team-epoch team))
            ;; Phase contexts stay fresh, even when a worker identity is reused.
            (dolist (member (crew--members team))
              (setf (opencode-workspace-worker-session (opencode-workspace--worker (crew--member-id member))) nil)))
          (funcall callback accepted))))))

(defun crew--receive (original packet)
  (if (not (equal crew-protocol (alist-get 'protocol packet)))
      (if (or (alist-get 'protocol packet)
              (gethash (alist-get 'from packet) crew--members)
              (gethash (alist-get 'to packet) crew--members))
          (error "Unknown/legacy protocol for managed crew") (funcall original packet))
    (let* ((id (alist-get 'from packet)) (member (gethash id crew--members))
           (worker (and member (opencode-workspace--worker id)))
           (key (alist-get 'id packet)) (fingerprint (secure-hash 'sha256 (json-encode packet))))
      (unless (and worker (equal 1 (alist-get 'version packet))
                   (equal opencode-workspace--identity (alist-get 'workspace packet))
                   (equal (symbol-name (crew--team-name (crew--member-team member))) (alist-get 'crew packet))
                   (equal (crew--team-run (crew--member-team member)) (alist-get 'run packet))
                   (equal (crew--team-epoch (crew--member-team member)) (alist-get 'epoch packet))
                   (integerp (alist-get 'generation packet))
                   (= (opencode-workspace-worker-generation worker) (alist-get 'generation packet))
                   (eq 'running (opencode-workspace-worker-state worker))
                   (process-live-p (opencode-workspace-worker-process worker))
                   (not (crew--member-cancelled member)) (opencode-workspace--name-p key))
        (error "Invalid, stale or cross-crew envelope"))
      (if-let ((prior (gethash key crew--requests)))
          (unless (equal prior fingerprint) (error "Conflicting request ID"))
        (when (>= (hash-table-count crew--requests) 4096) (error "Receipt budget exhausted"))
        (pcase (alist-get 'method packet)
          ("message"
           (let ((target (gethash (alist-get 'to packet) crew--members)))
             (unless (and target (not (equal id (crew--member-id target))) (eq (crew--member-team member) (crew--member-team target))
                          (member "crew.message" (plist-get (crew--role (crew--member-team member)
                                                                (crew--member-role member)) :capabilities)))
               (error "Peer target/capability denied"))
             (crew-send (crew--member-id target) (alist-get 'text packet))))
          ("spawn"
           (crew-spawn (crew--team-name (crew--member-team member))
                       (intern-soft (alist-get 'role packet)) (alist-get 'text packet)
                       :parent id :turns (alist-get 'turns packet)))
          (_ (error "Unsupported crew operation")))
        (puthash key fingerprint crew--requests))
      t)))

;;;###autoload
(defun crew-open ()
  "Open namespaced crew buffers plus the role/parent/budget roster."
  (interactive)
  (setq opencode-workspace-buffer-prefix "crew:")
  (opencode-workspace)
  (with-current-buffer (get-buffer-create "*crew:roles*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (maphash (lambda (id member)
                 (insert (format "%s | crew=%s | role=%s | parent=%s | depth=%d | turns=%d\n"
                          id (crew--team-name (crew--member-team member)) (crew--member-role member)
                          (or (crew--member-parent member) "operator") (crew--member-depth member)
                          (crew--member-remaining member)))) crew--members))
    (display-buffer (current-buffer))))

;; Install once, preserving the existing workspace implementation and callbacks.
(setq opencode-workspace-buffer-prefix "crew:")
(setq opencode-workspace-max-agents 512 opencode-workspace-max-turns 100000)
(advice-add 'opencode-workspace--pump :around #'crew--pump)
(advice-add 'opencode-workspace--files :around #'crew--files)
(advice-add 'opencode-workspace--sentinel :around #'crew--completion)
(advice-add 'opencode-workspace--event :after #'crew--capture)
(advice-add 'opencode-workspace--stop :after #'crew--wake-after)
(advice-add 'opencode-workspace--receive :around #'crew--receive)
(provide 'crew)
;;; crew.el ends here
