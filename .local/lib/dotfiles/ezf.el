;;; ezf.el --- Private-file completion bridge -*- lexical-binding: t; -*-
;; Generated from scripts/opencode-tmux.org; no Doom dependency.
(require 'cl-lib)
(require 'json)
(define-error 'dotfiles-ezf-unavailable "No visible Emacs frame")
(define-error 'dotfiles-ezf-timeout "EZF completion timed out")

(defun dotfiles-ezf--read (candidates single)
  "Read exact CANDIDATES.  SINGLE returns after one choice; empty RET finishes."
  (let ((remaining (delete-dups (copy-sequence candidates)))
        selected done)
    (while (and remaining (not done))
      (let ((choice (completing-read
                     (if single "Pick: " "Pick (empty RET finishes): ")
                     remaining nil t)))
        (if (equal choice "")
            (setq done t)
          (unless (member choice remaining)
            (error "Completion returned an unknown candidate"))
          (push choice selected)
          (setq remaining (delete choice remaining)
                done single))))
    (vconcat (nreverse selected))))

(defun dotfiles-ezf--complete (request)
  "Complete REQUEST in an existing visible frame without changing its lifetime."
  (let ((frame (cl-find-if (lambda (item) (eq (frame-visible-p item) t))
                           (frame-list)))
        (function (alist-get 'function request)))
    (unless frame (signal 'dotfiles-ezf-unavailable nil))
    (with-selected-frame frame
      (select-frame-set-input-focus frame)
      (if (equal function "ezf-default")
          (dotfiles-ezf--read (alist-get 'candidates request) (alist-get 'single request))
        (unless (and (stringp function)
                     (string-match-p "\\`[A-Za-z_][A-Za-z0-9_+*/<>=!?$%-]\\{0,127\\}\\'" function))
          (error "Invalid completion function name"))
        (let ((symbol (intern-soft function)))
          (unless (and symbol (fboundp symbol))
            (error "Completion function is not defined: %s" function))
          (let ((result (funcall symbol (alist-get 'source request))))
            (cond ((stringp result) (vector result))
                  ((listp result) (vconcat result))
                  (t (error "Completion function must return strings")))))))))

(defun dotfiles-ezf-main (request-path)
  "Read private JSON REQUEST-PATH and write a private JSON response."
  (let* ((request (with-temp-buffer
                    (insert-file-contents request-path)
                    (json-parse-buffer :object-type 'alist :array-type 'list
                                       :null-object nil :false-object nil)))
         (output (alist-get 'output request))
         (timeout (alist-get 'timeout request))
         (response
          (condition-case failure
              (let ((values
                     (with-timeout (timeout (signal 'dotfiles-ezf-timeout nil))
                       (dotfiles-ezf--complete request))))
                (if (zerop (length values))
                    '(:status "cancel")
                  (list :status "ok" :values values)))
            (quit '(:status "cancel"))
            (dotfiles-ezf-unavailable
             '(:status "unavailable" :message "Open an Emacs frame (emacsclient -c), or use fzf"))
            (dotfiles-ezf-timeout '(:status "timeout" :message "EZF completion timed out"))
            (error (list :status "error" :message (error-message-string failure))))))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (json-serialize response) nil output nil 'silent))
    (set-file-modes output #o600)
    nil))

(provide 'dotfiles-ezf)
;;; ezf.el ends here
