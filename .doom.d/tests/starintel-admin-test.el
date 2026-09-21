;;; starintel-admin-test.el --- tests for StarIntel admin UI -*- lexical-binding: t; -*-

(require 'ert)
(load-file
 (expand-file-name "../autoload/starintel-admin.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest starintel-admin-main-mode-is-org-native ()
  (with-temp-buffer
    (starintel-admin-mode)
    (should (derived-mode-p 'org-mode))
    (should buffer-read-only)))

(ert-deftest starintel-admin-user-table-is-dedicated-tabulated-mode ()
  (with-temp-buffer
    (starintel-admin-users-mode)
    (should (derived-mode-p 'tabulated-list-mode))
    (should (= 4 (length tabulated-list-format)))))

(ert-deftest starintel-admin-plan-is-derived-from-tenant-scopes ()
  (let ((user '(("username" . "alice")
                ("scopes" . ("documents:read" "tenant:pro" "tenant:research" "dataset:*")))))
    (should (equal "pro,research" (starintel-admin--user-plan user)))))

(ert-deftest starintel-admin-cells-cannot-break-org-tables ()
  (should (equal "hello¦world x"
                 (starintel-admin--cell "hello|world\nx"))))

(ert-deftest starintel-admin-does-not-own-startup-dashboard ()
  (should-not (bound-and-true-p initial-buffer-choice)))

(provide 'starintel-admin-test)
