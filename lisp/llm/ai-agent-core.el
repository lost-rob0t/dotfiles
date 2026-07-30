;;; ai-agent-core.el --- Canonical loader for the project-agent core -*- lexical-binding: t; -*-

(unless (featurep 'ai-agent-core)
  (load (expand-file-name
         "agent-core.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil
        'nomessage))

(provide 'ai-agent-core)
;;; ai-agent-core.el ends here
