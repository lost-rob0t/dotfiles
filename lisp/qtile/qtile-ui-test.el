;;; qtile-ui-test.el --- ERT coverage for shared Qtile popup UI -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'qtile-ui)
(require 'qtile-notifications)
(require 'qtile-services)
(load-file
 (expand-file-name "../../.config/qtile/qtile-desktop.el"
                   (file-name-directory load-file-name)))
(load-file
 (expand-file-name "../../.config/qtile/qtile-workflow.el"
                   (file-name-directory load-file-name)))

(ert-deftest qtile-ui-frame-parameters-are-popup-safe ()
  (let ((parameters
         (qtile-ui--frame-parameters
          "notifications"
          '((left . 120) (top . 46) (width . 640) (height . 620))
          nil)))
    (should (= (alist-get 'left parameters) 120))
    (should (= (alist-get 'top parameters) 46))
    (should (equal (alist-get 'minibuffer parameters) nil))
    (should (equal (alist-get 'mode-line-format parameters) nil))
    (should (eq (alist-get 'user-position parameters) t))
    (should (= (alist-get 'menu-bar-lines parameters) 0))
    (should (= (alist-get 'tool-bar-lines parameters) 0))
    (should (equal (alist-get 'vertical-scroll-bars parameters) nil))
     (should (equal (alist-get 'horizontal-scroll-bars parameters) nil))))

(ert-deftest qtile-ui-makes-popup-on-qtile-display ()
  (let (received)
    (cl-letf (((symbol-function 'make-frame-on-display)
               (lambda (display parameters)
                 (setq received (list display parameters))
                 'frame)))
      (should (eq (qtile-ui--make-frame
                   "test"
                   '((left . 10) (top . 20) (width . 400) (height . 300))
                   nil
                   '((display . ":0")))
                  'frame))
      (should (equal (car received) ":0")))))

(ert-deftest qtile-ui-prepare-buffer-removes-modeline-and-header ()
  ;; mode-line-format is buffer-local: setting it as a frame parameter was a
  ;; no-op, which is why Doom's modeline kept rendering inside popups.
  (with-temp-buffer
    (let ((doom-hide-mode-line (fboundp 'hide-mode-line-mode)))
      (qtile-ui-prepare-buffer)
      (should (null mode-line-format))
      (should (null header-line-format))
      (when doom-hide-mode-line
        (should (bound-and-true-p hide-mode-line-mode))))))

(ert-deftest qtile-ui-dismissal-keys-are-installed ()
  (with-temp-buffer
    (text-mode)
    (qtile-ui-bind-dismiss)
    (should (eq (key-binding (kbd "q")) #'qtile-ui-close-current))
    (should (eq (key-binding (kbd "ESC")) #'qtile-ui-close-current))
    (should (eq (key-binding (kbd "<escape>")) #'qtile-ui-close-current))))

(ert-deftest qtile-notifications-preserves-all-normalized-records ()
  (let ((payload (json-read-from-string
                  "{\"entries\":[{\"id\":1},{\"id\":2}],\"paused\":true}")))
    (should (= (length (qtile-notifications--get 'entries payload)) 2))
    (should (eq (qtile-notifications--get 'paused payload) t))))

(ert-deftest qtile-notifications-has-shared-dismissal-and-async-refresh ()
  (with-temp-buffer
    (qtile-notifications-mode)
    (should (eq (key-binding (kbd "q")) #'qtile-ui-close-current))
    (should (fboundp 'qtile-notifications-refresh))
    (should (fboundp 'make-process))))

(ert-deftest qtile-services-has-shared-popup-actions ()
  (with-temp-buffer
    (qtile-services-mode)
    (should (eq (key-binding (kbd "q")) #'qtile-ui-close-current))
    (should (equal (qtile-services--unit "qtile.service loaded active running Dashboard")
                   "qtile.service"))))

(ert-deftest qtile-legacy-renderers-accept-shared-parameters ()
  (should (equal (help-function-arglist #'qtile-org-todos-open)
                 '(&optional _params)))
  (should (equal (help-function-arglist #'qtile-agent-zero-open)
                 '(&optional _params))))

(ert-deftest qtile-services-restart-requires-two-activations ()
  (with-temp-buffer
    (qtile-services-mode)
    (setq qtile-services-selected "example.service")
    (setq qtile-services--confirm-restart nil)
    (let (calls)
      (cl-letf (((symbol-function 'start-process)
                 (lambda (&rest arguments) (push arguments calls))))
        (qtile-services-restart-selected)
        (should-not calls)
        (should qtile-services--confirm-restart)
        (qtile-services-restart-selected)
        (should (= (length calls) 1))
        (should (equal (car calls)
                       '("qtile-service-restart" nil "systemctl" "--user"
                         "restart" "example.service")))))))

(ert-deftest qtile-workflow-converts-json-vector-to-completion-list ()
  (with-temp-buffer
    (let (received)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _args)
                   (setq received choices)
                   "coding"))
                ((symbol-function 'qtile-ui-close-current) (lambda () nil)))
        (qtile-workflow-open
         '((args . ((choices . ["coding" "desktop" "focus"]))))))
      (should (equal received '("coding" "desktop" "focus"))))))

;;; qtile-ui-test.el ends here
