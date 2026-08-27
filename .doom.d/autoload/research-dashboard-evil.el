;;; research-dashboard-evil.el --- Evil integration for research dashboard -*- lexical-binding: t; -*-

(require 'subr-x)

(defconst nsa/research-dashboard-evil--command-states '(normal motion)
  "Evil states where research dashboard commands must remain directly usable.")

(defconst nsa/research-dashboard-evil--bindings
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
  "Research dashboard commands available from Evil command states.")

(defun nsa/research-dashboard-evil--apply-bindings ()
  "Install dashboard-local Evil command-state bindings in the current buffer."
  (dolist (state nsa/research-dashboard-evil--command-states)
    (dolist (binding nsa/research-dashboard-evil--bindings)
      (evil-local-set-key state (kbd (car binding)) (cdr binding)))))

;;;###autoload
(defun nsa/research-dashboard-evil-setup ()
  "Make research dashboard controls work directly in Evil command states."
  (if (fboundp 'evil-local-set-key)
      (nsa/research-dashboard-evil--apply-bindings)
    (let ((dashboard (current-buffer)))
      (with-eval-after-load 'evil
        (when (buffer-live-p dashboard)
          (with-current-buffer dashboard
            (nsa/research-dashboard-evil--apply-bindings)))))))

;;;###autoload
(add-hook 'nsa/research-dashboard-mode-hook #'nsa/research-dashboard-evil-setup)

(provide 'research-dashboard-evil)
;;; research-dashboard-evil.el ends here
