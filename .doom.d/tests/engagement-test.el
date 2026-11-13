;;; engagement-test.el --- Tests for engagement document generator -*- lexical-binding: t; -*-

(require 'ert)
(require 'engagement)

(defconst nsa/engagement-test--fields
  (list :kind "PENTEST ENGAGEMENT"
        :title "Example - All Assets"
        :target "example.test"
        :aliases "NONE"
        :auth-ref "AUTH-2026-001"
        :signatory "A. Signer"
        :role "Owner, Example Org"
        :auth-date "2026-09-09"
        :date "2026-09-09"
        :start "2026-09-09"
        :end "OPEN"
        :dataset "example-ds"
        :notes "NONE"))

(ert-deftest nsa/engagement-slug-normalizes ()
  (should (equal (nsa/engagement--slug "Example Target!.com") "example-target-com"))
  (should (equal (nsa/engagement--slug "--example--") "example")))

(ert-deftest nsa/engagement-render-fills-all-fields ()
  (let ((document (nsa/engagement--render nsa/engagement-test--fields)))
    (should (string-match-p "TITLE        : Example - All Assets" document))
    (should (string-match-p "TYPE         : PENTEST ENGAGEMENT" document))
    (should (string-match-p "Primary target: example.test" document))
    (should (string-match-p "(ref: AUTH-2026-001)" document))
    (should (string-match-p "Signatory: A. Signer (Owner, Example Org)" document))
    (should (string-match-p "Store under dataset: example-ds" document))
    (should-not (string-match-p "%[a-zA-Z]" document))))

(ert-deftest nsa/engagement-render-rejects-missing-fields ()
  (should-error (nsa/engagement--render (list :kind "OSINT TARGET SCOPING"))))

(ert-deftest nsa/engagement-template-covers-pentest-rules ()
  (should (string-match-p "ENGAGEMENT WINDOW" nsa/engagement--template))
  (should (string-match-p "RULES OF ENGAGEMENT" nsa/engagement--template))
  (should (string-match-p "Written authorization: ON FILE" nsa/engagement--template)))

(provide 'engagement-test)
;;; engagement-test.el ends here
