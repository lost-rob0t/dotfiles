;;; star-android-modern-test.el --- Tests for native Android modern layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'star-android-modern)

(ert-deftest star/android-modern-declares-vim-and-org-ui-packages ()
  (dolist (package '(evil evil-collection org-modern))
    (should (memq package star/android-modern-packages))))

(ert-deftest star/android-modern-toolbar-is-org-first ()
  (should (equal (mapcar #'car star/android-modern-toolbar-spec)
                 '(dashboard previous-heading next-heading cycle open todo sync back))))

(ert-deftest star/android-modern-next-heading-uses-org-navigation ()
  (with-temp-buffer
    (org-mode)
    (insert "* One\nbody\n* Two\n")
    (goto-char (point-min))
    (star/android-modern-next-heading)
    (should (looking-at-p "\\* Two"))))

(provide 'star-android-modern-test)
;;; star-android-modern-test.el ends here
