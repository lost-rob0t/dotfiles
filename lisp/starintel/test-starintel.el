;;; test-starintel.el --- Tests for starintel.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'starintel)

(ert-deftest starintel-base-url-test ()
  (let ((starintel-scheme "https")
        (starintel-host "star.example")
        (starintel-port 8443))
    (should (equal "https://star.example:8443" (starintel-base-url)))
    (should (equal "https://star.example:8443/health"
                   (starintel--request-url "/health" nil)))))

(ert-deftest starintel-query-string-test ()
  (should
   (equal "?q=star%20intel&limit=25"
          (starintel--query-string '((q . "star intel") (limit . 25))))))

(ert-deftest starintel-auth-header-test ()
  (let ((starintel-auth-token-function (lambda () "secret-token")))
    (should
     (equal "Bearer secret-token"
            (cdr (assoc "Authorization" (starintel--request-headers))))))
  (let ((starintel-auth-token-function nil))
    (should-not (assoc "Authorization" (starintel--request-headers)))))

(ert-deftest starintel-request-is-asynchronous-and-encodes-json-test ()
  (let (seen-url seen-method seen-headers seen-data)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 (declare (ignore callback cbargs silent inhibit-cookies))
                 (setq seen-url url
                       seen-method url-request-method
                       seen-headers url-request-extra-headers
                       seen-data url-request-data)
                 'fake-request-buffer)))
      (should
       (eq 'fake-request-buffer
           (starintel-request :post "/new/document/domain"
                              :data '((dtype . "domain")
                                      (record . "example.com")))))
      (should (equal "http://127.0.0.1:5000/new/document/domain" seen-url))
      (should (equal "POST" seen-method))
      (should (assoc "Content-Type" seen-headers))
      (should (string-match-p "example.com" seen-data)))))

(defun starintel-test--response-buffer (status body)
  (let ((buffer (generate-new-buffer " *starintel-response-test*")))
    (with-current-buffer buffer
      (setq-local url-http-response-status status)
      (insert (format "HTTP/1.1 %d Test\r\nContent-Type: application/json\r\n\r\n%s"
                      status body)))
    buffer))

(ert-deftest starintel-success-response-test ()
  (let ((buffer (starintel-test--response-buffer 200 "{\"msg\":\"OK\"}"))
        result failure)
    (with-current-buffer buffer
      (starintel--finish-request nil
                                 (lambda (data) (setq result data))
                                 (lambda (error) (setq failure error))))
    (should-not failure)
    (should (equal "OK" (alist-get 'msg result)))
    (should-not (buffer-live-p buffer))))

(ert-deftest starintel-http-error-test ()
  (let ((buffer (starintel-test--response-buffer 503 "{\"msg\":\"offline\"}"))
        result failure)
    (with-current-buffer buffer
      (starintel--finish-request nil
                                 (lambda (data) (setq result data))
                                 (lambda (error) (setq failure error))))
    (should-not result)
    (should (eq :http (plist-get failure :type)))
    (should (= 503 (plist-get failure :status)))
    (should (equal "offline"
                   (alist-get 'msg (plist-get failure :response))))))

(ert-deftest starintel-transport-error-does-not-include-token-test ()
  (let ((buffer (generate-new-buffer " *starintel-transport-test*"))
        (starintel-auth-token-function (lambda () "should-never-appear"))
        failure)
    (with-current-buffer buffer
      (starintel--finish-request '(:error (error . "connection failed"))
                                 #'ignore
                                 (lambda (error) (setq failure error))))
    (should (eq :transport (plist-get failure :type)))
    (should-not (string-match-p "should-never-appear" (format "%S" failure)))))

;;; test-starintel.el ends here
