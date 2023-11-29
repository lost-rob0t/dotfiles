(in-package :nyxt-user)
;; Our input is *target*
(defun make-crunch-base-url (company-name)
  (format nil "https://www.crunchbase.com/organization/~a/company_financials" company-name))

(defun make-crunch-base-org-url (company-name)
  (format nil "https://www.crunchbase.com/organization/~a" company-name))
;; So to get the "correct" section you need to get the span by id, then get its parent then select with the
;; div.identifier-label selector to get the listing of aqutions

(defun get-section (section-name)
  (let* ((span (clss:select section-name (document-model (current-buffer))))
         (parents (nyxt/dom:parents (elt span 0))))
    (elt parents 0)))

(defun parse-ownership ()
  (nyxt:update-document-model)

  (let ((doms (clss:select "div.identifier-label" (get-section "#acquisitions"))))
    (loop for i from 0 to (- (length doms) 1)
          if (evenp i)
            collect (nyxt/dom:body (elt doms i)))))
(defun generate-org-csv (org-titles)
  "Generate CSV string with org-title, org-url pairs."
  (let ((csv-string "org-title,org-url"))
    (dolist (title org-titles)
      (let ((url (make-crunch-base-org-url title)))
        (setq csv-string (format nil "~a~%~a,~a" csv-string title url))))
    csv-string))


(define-command-global crunchbase-scrape-org (&optional (buffer (current-buffer)))
  "Scrape the aquisitions made by a company into a csv into clipboard."
  (nyxt:copy-to-clipboard (generate-org-csv (parse-ownership))))
