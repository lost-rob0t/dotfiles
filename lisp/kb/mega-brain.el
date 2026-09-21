;;; mega-brain.el --- Persistent Org/Prolog Mega Brain bridge -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup star/mega-brain nil
  "Persistent typed bridge from Emacs to the Org-derived Prolog KB."
  :group 'applications
  :prefix "star/mega-brain-")

(defcustom star/mega-brain-swipl-program "swipl"
  "SWI-Prolog executable used by the persistent bridge."
  :type 'string
  :group 'star/mega-brain)

(defcustom star/mega-brain-dotfiles-root
  (file-name-as-directory
   (expand-file-name
    (or (getenv "STAR_DOTFILES_ROOT") "~/.dotfiles")))
  "Dotfiles checkout containing expert sources and bridge code."
  :type 'directory
  :group 'star/mega-brain)

(defcustom star/mega-brain-kb-file
  (expand-file-name
   "kb-roam/roam-mega-kb.pl"
   (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Generated Org-Roam Mega Brain fact file."
  :type 'file
  :group 'star/mega-brain)

(defcustom star/mega-brain-extra-sources nil
  "Additional reviewed Prolog sources loaded when the bridge starts."
  :type '(repeat file)
  :group 'star/mega-brain)

(defvar star/mega-brain--process nil)
(defvar star/mega-brain--buffer nil)
(defvar star/mega-brain--stderr-buffer nil)
(defvar star/mega-brain--pending "")
(defvar star/mega-brain--responses (make-hash-table :test #'equal))
(defvar star/mega-brain--request-counter 0)

(defun star/mega-brain--bridge-script ()
  (expand-file-name "lisp/kb/mega-brain-bridge.pl"
                    star/mega-brain-dotfiles-root))

(defun star/mega-brain--inventory-expert ()
  (expand-file-name ".zara/experts/inventory/kb/expert.pl"
                    star/mega-brain-dotfiles-root))

(defun star/mega-brain--sources ()
  (delete-dups
   (seq-filter
    #'file-readable-p
    (append (list star/mega-brain-kb-file
                  (star/mega-brain--inventory-expert))
            star/mega-brain-extra-sources))))

(defun star/mega-brain-live-p ()
  "Return non-nil when the persistent SWI-Prolog bridge is live."
  (and (processp star/mega-brain--process)
       (process-live-p star/mega-brain--process)))

(defun star/mega-brain--filter (_process chunk)
  (setq star/mega-brain--pending
        (concat star/mega-brain--pending chunk))
  (let ((start 0))
    (while (string-match "\n" star/mega-brain--pending start)
      (let* ((end (match-beginning 0))
             (line (substring star/mega-brain--pending 0 end)))
        (setq star/mega-brain--pending
              (substring star/mega-brain--pending (match-end 0)))
        (setq start 0)
        (unless (string-empty-p (string-trim line))
          (condition-case nil
              (let* ((object (json-parse-string
                              line
                              :object-type 'hash-table
                              :array-type 'list
                              :null-object nil
                              :false-object :false))
                     (id (gethash "id" object)))
                (when id
                  (puthash id object star/mega-brain--responses)))
            (json-parse-error
             (message "Mega Brain ignored non-JSON Prolog output: %s" line))))))))

(defun star/mega-brain-start ()
  "Start or return the persistent Mega Brain Prolog process."
  (interactive)
  (unless (star/mega-brain-live-p)
    (let ((script (star/mega-brain--bridge-script)))
      (unless (file-readable-p script)
        (user-error "Mega Brain bridge script is missing: %s" script))
      (unless (executable-find star/mega-brain-swipl-program)
        (user-error "SWI-Prolog is unavailable: %s" star/mega-brain-swipl-program))
      (setq star/mega-brain--pending "")
      (clrhash star/mega-brain--responses)
      (setq star/mega-brain--buffer (get-buffer-create " *mega-brain*")
            star/mega-brain--stderr-buffer
            (get-buffer-create "*Mega Brain Errors*"))
      (setq star/mega-brain--process
            (make-process
             :name "mega-brain"
             :buffer star/mega-brain--buffer
             :stderr star/mega-brain--stderr-buffer
             :command
             (append
              (list star/mega-brain-swipl-program
                    "-q" "-f" "none" "-s" script "--")
              (star/mega-brain--sources))
             :connection-type 'pipe
             :coding 'utf-8-unix
             :filter #'star/mega-brain--filter
             :noquery t))))
  star/mega-brain--process)

(defun star/mega-brain-stop ()
  "Stop the persistent bridge and clear volatile request state."
  (interactive)
  (when (processp star/mega-brain--process)
    (delete-process star/mega-brain--process))
  (setq star/mega-brain--process nil
        star/mega-brain--pending "")
  (clrhash star/mega-brain--responses))

(defun star/mega-brain-unwind-all ()
  "Hard-unwind the Prolog session by replacing the process."
  (interactive)
  (star/mega-brain-stop)
  (star/mega-brain-start)
  (message "Mega Brain Prolog session unwound"))

(defun star/mega-brain--request (operation &optional fields timeout)
  (star/mega-brain-start)
  (let* ((id (format "emacs-%d" (cl-incf star/mega-brain--request-counter)))
         (payload (append
                   (list (cons "id" id)
                         (cons "operation" operation))
                   fields))
         (deadline (+ (float-time) (or timeout 5.0))))
    (process-send-string
     star/mega-brain--process
     (concat (json-serialize payload) "\n"))
    (while (and (not (gethash id star/mega-brain--responses))
                (< (float-time) deadline)
                (star/mega-brain-live-p))
      (accept-process-output star/mega-brain--process 0.05))
    (let ((response (gethash id star/mega-brain--responses)))
      (remhash id star/mega-brain--responses)
      (unless response
        (error "Mega Brain request timed out"))
      (unless (eq (gethash "ok" response) t)
        (let ((failure (gethash "error" response)))
          (error "%s"
                 (if (hash-table-p failure)
                     (or (gethash "message" failure)
                         "Mega Brain request failed")
                   "Mega Brain request failed"))))
      (gethash "result" response))))

(defun star/mega-brain-status ()
  "Return bridge status, loaded sources, and the closed predicate API."
  (interactive)
  (let ((result (star/mega-brain--request "status")))
    (when (called-interactively-p 'interactive)
      (message "Mega Brain ready: %s"
               (length (gethash "predicates" result))))
    result))

(defun star/mega-brain-reset ()
  "Reload the configured Org-derived KB and expert sources in place."
  (interactive)
  (prog1 (star/mega-brain--request "reset")
    (message "Mega Brain reloaded")))

(defun star/mega-brain-query (module predicate args &optional limit)
  "Query a closed MODULE/PREDICATE with JSON-safe ARGS.

A variable argument is represented as ((var . NAME)). Compound input is
represented as ((functor . NAME) (args . VALUES)). No Prolog source text is
accepted by this API."
  (star/mega-brain--request
   "query"
   (list (cons "module" module)
         (cons "predicate" predicate)
         (cons "args" args)
         (cons "limit" (or limit 50)))))

(defun star/mega-brain--bridge-args-list (args)
  (let ((value (gethash "args" args)))
    (if (listp value) value nil)))

(defun star/mega-brain--zara-status (_args)
  (star/mega-brain-status))

(defun star/mega-brain--zara-query (args)
  (let ((module (gethash "module" args))
        (predicate (gethash "predicate" args))
        (values (star/mega-brain--bridge-args-list args))
        (limit (or (gethash "limit" args) 50)))
    (unless (and (stringp module) (stringp predicate))
      (error "module and predicate are required"))
    (star/mega-brain-query module predicate values limit)))

(defun star/mega-brain--zara-reset (_args)
  (star/mega-brain-reset))

(defun star/mega-brain--zara-unwind (_args)
  (star/mega-brain-unwind-all)
  '((status . "restarted")))

(defun star/mega-brain-register-zara-bridge ()
  "Expose the Mega Brain through Zara's trusted semantic bridge."
  (when (require 'zara nil t)
    (dolist (entry
             '(("mega_brain.status" . star/mega-brain--zara-status)
               ("mega_brain.query" . star/mega-brain--zara-query)
               ("mega_brain.reset" . star/mega-brain--zara-reset)
               ("mega_brain.unwind" . star/mega-brain--zara-unwind)))
      (zara-bridge-register-handler (car entry) (cdr entry)))))

(with-eval-after-load 'zara
  (star/mega-brain-register-zara-bridge))

(provide 'mega-brain)
;;; mega-brain.el ends here
