(in-package :lish-user)

(defparameter *star-agentic-enabled*
  (not (member (string-downcase (or (uiop:getenv "STAR_LISH_AGENTIC") "1"))
               '("0" "false" "off" "no")
               :test #'string=)))

(defparameter *star-rlm-mode*
  (string-downcase (or (uiop:getenv "STAR_LISH_RLM_MODE") "auto")))

(defvar *star-original-shell-eval-command* nil)

(defun star--join-words (words)
  (format nil "~{~a~^ ~}" words))

(defun star--run (program &rest arguments)
  (uiop:run-program (cons program arguments)
                    :input :interactive
                    :output *standard-output*
                    :error-output *error-output*
                    :ignore-error-status t))

(defun star--rlm (kind text)
  (let ((mode-env (format nil "STAR_LISH_RLM_MODE=~a" *star-rlm-mode*)))
    (uiop:run-program (list "env" mode-env "lish-rlm" kind text)
                      :input :interactive
                      :output *standard-output*
                      :error-output *error-output*
                      :ignore-error-status t)))

(defun star--known-command-p (shell expr)
  (let* ((words (lish::shell-expr-words expr))
         (raw (and words (lish::word-word (first words))))
         (command (typecase raw
                    (string raw)
                    (symbol (string-downcase (symbol-name raw)))
                    (t nil)))
         (line (or (ignore-errors (lish::shell-expr-line expr)) "")))
    (or (null command)
        (ignore-errors (lish::command-type shell command))
        (and (> (length line) 0)
             (member (char line 0) '(#\( #\' #\`) :test #'char=))
        (position #\= command))))

(defun star--agentic-shell-eval-command (shell expr context &key no-alias)
  (if (and *star-agentic-enabled*
           (not (star--known-command-p shell expr)))
      (progn
        (star--rlm "suggest" (lish::shell-expr-line expr))
        (values '(0) nil nil))
      (funcall *star-original-shell-eval-command*
               shell expr context :no-alias no-alias)))

(defun star-enable-agentic-evaluator ()
  (unless *star-original-shell-eval-command*
    (setf *star-original-shell-eval-command*
          (symbol-function 'lish::shell-eval-command))
    (setf (symbol-function 'lish::shell-eval-command)
          #'star--agentic-shell-eval-command))
  t)

(lish:defcommand ai ((words string :rest t :help "Free-form question or task."))
  "Route free-form text through Prolog-RLM without executing generated text."
  (star--rlm "suggest" (star--join-words words)))

(lish:defcommand ask ((words string :rest t :help "Question."))
  "Ask Prolog-RLM a question."
  (star--rlm "ask" (star--join-words words)))

(lish:defcommand agent ((words string :rest t :help "Agentic task."))
  "Run a task through Prolog-RLM's capability/budget/effect boundaries."
  (star--rlm "agent" (star--join-words words)))

(lish:defcommand shell ((words string :rest t :help "Desired shell operation."))
  "Generate a shell command or sequence."
  (star--rlm "shell" (star--join-words words)))

(lish:defcommand lisp ((words string :rest t :help "Desired Common Lisp operation."))
  "Generate idiomatic Common Lisp for the current Lish image."
  (star--rlm "lisp" (star--join-words words)))

(lish:defcommand oneliner ((words string :rest t :help "Desired one-liner."))
  "Generate a compact command-line one-liner."
  (star--rlm "oneliner" (star--join-words words)))

(lish:defcommand agentic ((state string :optional t :help "on, off, toggle, or status."))
  "Control automatic free-form routing."
  (let ((value (string-downcase (or state "status"))))
    (cond
      ((string= value "on") (setf *star-agentic-enabled* t))
      ((string= value "off") (setf *star-agentic-enabled* nil))
      ((string= value "toggle") (setf *star-agentic-enabled* (not *star-agentic-enabled*)))
      ((string= value "status"))
      (t (error "agentic expects on, off, toggle, or status")))
    (format t "agentic=~:[off~;on~] mode=~a~%" *star-agentic-enabled* *star-rlm-mode*)))

(lish:defcommand rlm-mode ((mode string :optional t :help "direct, symbolic, symbolic-recursive, or auto."))
  "Set the requested Prolog-RLM reasoning strategy for shell requests."
  (when mode
    (let ((value (string-downcase mode)))
      (unless (member value '("direct" "symbolic" "symbolic-recursive" "auto") :test #'string=)
        (error "Unknown RLM mode: ~a" value))
      (setf *star-rlm-mode* value)))
  (format t "rlm-mode=~a~%" *star-rlm-mode*))

(lish:defcommand experts ()
  "List repo-local Zara/Prolog experts."
  (star--run "lish-expert" "list"))

(lish:defcommand expert ((args string :rest t :help "lish-expert arguments."))
  "Invoke the repo-local expert CLI."
  (apply #'star--run "lish-expert" args))

(lish:defcommand expert-status ()
  "Show expert runtime status."
  (star--run "lish-expert" "status"))

(lish:defcommand lisa-status ()
  "Show whether the LISA Rete expert system is loaded in this Lisp image."
  (format t "lisa=~:[missing~;loaded~]~%"
          (not (null (find-package :lisa)))))

(star-enable-agentic-evaluator)
