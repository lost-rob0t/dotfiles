;;; org-food-test.el --- Tests for Org food helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-food)

(ert-deftest org-food-topical-tags-are-specific ()
  (should (equal (org-food--topical-tags "Italian")
                 '("recipe" "italian"))))

(ert-deftest org-food-topical-tags-have-food-fallback ()
  (should (equal (org-food--topical-tags "")
                 '("recipe" "food"))))

(ert-deftest org-food-formats-whole-quantity-cleanly ()
  (should (equal (org-food--format-number 3.0) "3")))

(ert-deftest org-food-event-table-has-consumption-delta ()
  (should (= (cdr (assoc "consume" org-food-event-deltas)) -1)))

(provide 'org-food-test)
;;; org-food-test.el ends here
