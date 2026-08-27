;;; research-dashboard-evil-test.el --- Evil bindings for research dashboard -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'research-dashboard)

(when (locate-library "research-dashboard-evil")
  (require 'research-dashboard-evil))

(defconst nsa/research-dashboard-evil-test--expected-bindings
  '(("g" . nsa/research-dashboard-refresh)
    ("RET" . nsa/research-dashboard-view)
    ("a" . nsa/research-dashboard-approve)
    ("r" . nsa/research-dashboard-reject)
    ("e" . nsa/research-dashboard-errors)
    ("s" . nsa/research-dashboard-search)
    ("/" . nsa/research-dashboard-search)
    ("f" . nsa/research-dashboard-filter)
    ("c" . nsa/research-dashboard-clear-filters)
    ("L" . nsa/research-dashboard-toggle-legacy)
    ("?" . nsa/research-dashboard-help))
  "Dashboard bindings that must work in Evil command states.")

(ert-deftest nsa/research-dashboard-evil-integration-is-installed ()
  (should (fboundp 'nsa/research-dashboard-evil-setup))
  (should (memq #'nsa/research-dashboard-evil-setup
                nsa/research-dashboard-mode-hook)))

(ert-deftest nsa/research-dashboard-evil-command-states-get-dashboard-keys ()
  (should (fboundp 'nsa/research-dashboard-evil-setup))
  (let (calls)
    (cl-letf (((symbol-function 'evil-local-set-key)
               (lambda (state key command)
                 (push (list state (key-description key) command) calls))))
      (nsa/research-dashboard-evil-setup))
    (dolist (state '(normal motion))
      (dolist (binding nsa/research-dashboard-evil-test--expected-bindings)
        (should (member (list state (car binding) (cdr binding)) calls))))))

(provide 'research-dashboard-evil-test)
;;; research-dashboard-evil-test.el ends here
