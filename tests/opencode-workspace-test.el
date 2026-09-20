;;; opencode-workspace-test.el --- Workspace regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'opencode-workspace)

(defmacro opencode-workspace-test-with-state (&rest body)
  `(let ((opencode-workspace--agents (make-hash-table :test #'equal))
         (opencode-workspace--seen (make-hash-table :test #'equal))
         (opencode-workspace--epoch 0)
         (opencode-workspace--turns 0)
         (opencode-workspace--identity "test-workspace")
         (opencode-workspace-buffer-prefix "test:")
         (opencode-workspace-experts-function nil))
     (unwind-protect (progn ,@body)
       (maphash (lambda (_ worker) (opencode-workspace--stop worker 'cancelled "test cleanup"))
                opencode-workspace--agents)
       (dolist (name '("chat" "files" "kb" "agents-list"))
         (when-let ((buffer (get-buffer (format "*test:%s*" name))))
           (kill-buffer buffer))))))

(ert-deftest opencode-workspace-identities-are-explicit ()
  (opencode-workspace-test-with-state
   (should-error (opencode-workspace-add-agent "../bad" default-directory "test/model"))
   (opencode-workspace-add-agent "alice" default-directory "test/model")
   (should-error (opencode-workspace-add-agent "alice" default-directory "test/model"))
   (should-error (opencode-workspace-send "absent" "hello"))
   (should (= 1 (hash-table-count opencode-workspace--agents)))))

(ert-deftest opencode-workspace-mailbox-is-bounded-fifo ()
  (opencode-workspace-test-with-state
   (let ((worker (opencode-workspace-add-agent "alice" default-directory "test/model"))
         (opencode-workspace-max-inbox 2))
     (setf (opencode-workspace-worker-state worker) 'running)
     (opencode-workspace-send "alice" "one")
     (opencode-workspace-send "alice" "two")
     (should-error (opencode-workspace-send "alice" "three"))
     (should (equal '("one" "two")
                    (mapcar (lambda (packet) (alist-get 'text packet))
                            (opencode-workspace-worker-inbox worker)))))))

(ert-deftest opencode-workspace-json-is-data-and-session-bound ()
  (opencode-workspace-test-with-state
   (opencode-workspace-add-agent "alice" default-directory "test/model")
   (let ((worker (opencode-workspace--worker "alice")))
     (opencode-workspace--event worker
       '((type . "text") (sessionID . "ses_test")
         (part . ((text . "\" ) (error \"not executed\")\nhello")))))
     (should (equal "ses_test" (opencode-workspace-worker-session worker)))
     (should-error (opencode-workspace--event worker
                    '((type . "text") (sessionID . "ses_other") (part . ((text . "no"))))))
     (with-current-buffer "*test:chat*"
       (should (string-match-p "not executed" (buffer-string)))))))

(ert-deftest opencode-workspace-cancel-fences-generation-and-clears-mailbox ()
  (opencode-workspace-test-with-state
   (let* ((worker (opencode-workspace-add-agent "alice" default-directory "test/model"))
          (before (opencode-workspace-worker-generation worker)))
     (setf (opencode-workspace-worker-state worker) 'running)
     (opencode-workspace-send "alice" "queued")
     (opencode-workspace-cancel "alice")
     (should (> (opencode-workspace-worker-generation worker) before))
     (should-not (opencode-workspace-worker-inbox worker))
     (should-error (opencode-workspace-send "alice" "after cancel")))))

(ert-deftest opencode-workspace-never-claims-unconfigured-experts-loaded ()
  (opencode-workspace-test-with-state
   (opencode-workspace-add-agent "alice" default-directory "test/model")
   (opencode-workspace-refresh)
   (with-current-buffer "*test:kb*"
     (should (string-match-p "unknown" (buffer-string))))))

(ert-deftest opencode-workspace-expert-reports-require-provenance ()
  (opencode-workspace-test-with-state
   (let ((worker (opencode-workspace-add-agent "alice" default-directory "test/model")))
     (should-error (opencode-workspace--accept-experts worker '((experts . ("made-up")))))
     (opencode-workspace--accept-experts worker
       '((runtime . "test-runtime") (revision . "r1")
         (experts . (((id . "rules") (version . "1") (state . "loaded"))))))
     (should (equal "r1" (alist-get 'revision (opencode-workspace-worker-experts worker)))))))

(ert-deftest opencode-workspace-file-selection-rejects-outside-and-unsaved ()
  (opencode-workspace-test-with-state
   (let* ((root (make-temp-file "oc-root" t))
          (outside (make-temp-file "oc-outside"))
          (inside (expand-file-name "input.txt" root)))
     (unwind-protect
         (progn
           (with-temp-file inside (insert "fixture"))
           (opencode-workspace-add-agent "alice" root "test/model")
           (should-error (opencode-workspace-add-file "alice" outside))
           (opencode-workspace-add-file "alice" inside)
           (with-current-buffer (find-file-noselect inside)
             (insert "unsaved")
             (should-error (opencode-workspace--files (opencode-workspace--worker "alice")))
             (set-buffer-modified-p nil)
             (kill-buffer)))
       (delete-directory root t)
       (delete-file outside)))))

(ert-deftest opencode-workspace-wont-overwrite-unowned-buffers ()
  (opencode-workspace-test-with-state
   (get-buffer-create "*test:chat*")
   (should-error (opencode-workspace--buffer "chat"))))

(ert-deftest opencode-workspace-receive-rejects-untrusted-file-modes ()
  (opencode-workspace-test-with-state
   (let ((path (make-temp-file "oc-message")))
     (unwind-protect
         (progn (set-file-modes path #o644)
                (should-error (opencode-workspace-receive-file path)))
       (delete-file path)))))

(ert-deftest opencode-workspace-peer-rejects-wrong-identity-and-stale-sender ()
  (opencode-workspace-test-with-state
   (opencode-workspace-add-agent "alice" default-directory "test/model")
   (opencode-workspace-add-agent "bob" default-directory "test/model")
   (should-error (opencode-workspace--receive
                  '((version . 1) (workspace . "other") (id . "msg1")
                    (from . "alice") (to . "bob") (generation . 0) (text . "hello"))))
   (should-error (opencode-workspace--receive
                  '((version . 1) (workspace . "test-workspace") (id . "msg1")
                    (from . "alice") (to . "bob") (generation . -1) (text . "hello"))))))

(ert-deftest opencode-workspace-real-process-json-fragments ()
  (opencode-workspace-test-with-state
   (let* ((script (make-temp-file "oc-fixture" nil ".py"))
          (opencode-workspace-program (or (executable-find "python3") (ert-fail "python3 required")))
          (opencode-workspace-program-prefix nil)
          (worker (opencode-workspace-add-agent "alice" default-directory "fixture/model")))
     (unwind-protect
         (progn
           (with-temp-file script
             (insert "import json,sys,time\n"
                     "sys.stdin.read()\n"
                     "s=json.dumps({'type':'text','sessionID':'ses_fixture','part':{'text':'fixture reply'}})+'\\n'\n"
                     "sys.stdout.write(s[:12]);sys.stdout.flush();time.sleep(.02)\n"
                     "sys.stdout.write(s[12:]);sys.stdout.flush()\n"
                     "print(json.dumps({'type':'step_finish','sessionID':'ses_fixture','part':{'reason':'stop'}}))\n"))
           (setq opencode-workspace-program-prefix (list script))
           (opencode-workspace-send "alice" "hello")
           (let ((deadline (+ (float-time) 5)))
             (while (and (eq 'running (opencode-workspace-worker-state worker))
                         (< (float-time) deadline))
               (accept-process-output nil .05)))
           (should (eq 'idle (opencode-workspace-worker-state worker)))
           (should (equal "ses_fixture" (opencode-workspace-worker-session worker)))
           (with-current-buffer "*test:chat*"
             (should (string-match-p "fixture reply" (buffer-string)))))
       (delete-file script)))))

(provide 'opencode-workspace-test)
