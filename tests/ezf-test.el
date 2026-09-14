;;; ezf-test.el --- EZF regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../.local/lib/dotfiles/ezf.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(ert-deftest ezf-single-keeps-commas-quotes-and-nil ()
  (dolist (value '("nil" "a,b" "quote\"'" "slash\\x" "λ" "line\nbreak" "$(touch nope)"))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) value)))
      (should (equal (dotfiles-ezf--read (list value) t) (vector value))))))

(ert-deftest ezf-multi-uses-whole-candidates-not-csv ()
  (let ((answers '("a,b" "nil" "")))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop answers))))
      (should (equal (dotfiles-ezf--read '("a,b" "nil" "unused") nil) ["a,b" "nil"])))))

(ert-deftest ezf-rejects-unknown-completion ()
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "forged")))
    (should-error (dotfiles-ezf--read '("original") t))))

(ert-deftest ezf-empty-and-quit-are-not-literal-nil ()
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
    (should (equal (dotfiles-ezf--read '("nil") t) [])))
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (signal 'quit nil))))
    (should (eq (condition-case nil (dotfiles-ezf--read '("nil") t) (quit 'cancelled)) 'cancelled))))

(ert-deftest ezf-no-visible-frame-is-unavailable ()
  (cl-letf (((symbol-function 'frame-list) (lambda () nil)))
    (should-error (dotfiles-ezf--complete '((function . "ezf-default")))
                  :type 'dotfiles-ezf-unavailable)))

(ert-deftest ezf-json-response-and-cancellation ()
  (let* ((directory (make-temp-file "ezf-ert-" t))
         (request (expand-file-name "request.json" directory))
         (output (expand-file-name "result.json" directory)))
    (unwind-protect
        (progn
          (with-temp-file request
            (insert (json-serialize (list :timeout 5 :output output))))
          (cl-letf (((symbol-function 'dotfiles-ezf--complete) (lambda (_) ["nil" "a,b"])))
            (dotfiles-ezf-main request))
          (should (= (file-modes output) #o600))
          (should (equal (with-temp-buffer
                           (insert-file-contents output)
                           (json-parse-buffer :object-type 'alist :array-type 'list))
                         '((status . "ok") (values "nil" "a,b"))))
          (cl-letf (((symbol-function 'dotfiles-ezf--complete) (lambda (_) (signal 'quit nil))))
            (dotfiles-ezf-main request))
          (should (equal (with-temp-buffer
                           (insert-file-contents output)
                           (json-parse-buffer :object-type 'alist))
                         '((status . "cancel")))))
      (delete-directory directory t))))
