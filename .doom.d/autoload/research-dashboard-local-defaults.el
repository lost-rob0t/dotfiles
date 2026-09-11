;;; research-dashboard-local-defaults.el --- Local research roots -*- lexical-binding: t; -*-

;;; Commentary:
;; Machine-local research roots tracked by the research dashboard.  These
;; forms are emitted into Doom's generated autoloads so the roots and local
;; DWIM adapter are available whenever the dashboard is opened.

;;; Code:

;;;###autoload
(setq nsa/research-dashboard-local-checkouts
      (list (expand-file-name "~/starintel")
            (expand-file-name "~/Documents/Projects/prolog-rlm")))

;;;###autoload
(with-eval-after-load 'research-dashboard
  (require 'research-dashboard-local)
  (require 'research-dashboard-local-dwim))

(provide 'research-dashboard-local-defaults)
;;; research-dashboard-local-defaults.el ends here
