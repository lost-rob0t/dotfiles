;;; research-dashboard-local-defaults.el --- Local research roots -*- lexical-binding: t; -*-

;;; Commentary:
;; Machine-local research roots tracked by the research dashboard.  This form
;; is emitted into Doom's generated autoloads so the values are bound before
;; `research-dashboard-local' is loaded; its `defcustom' then preserves them.

;;; Code:

;;;###autoload
(setq nsa/research-dashboard-local-checkouts
      (list (expand-file-name "~/starintel")
            (expand-file-name "~/Documents/Projects/prolog-rlm")))

(provide 'research-dashboard-local-defaults)
;;; research-dashboard-local-defaults.el ends here
