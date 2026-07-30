;;; ai-agent.el --- Canonical loader for the gptel project agent -*- lexical-binding: t; -*-

(unless (featurep 'ai-agent)
  (require 'ai-agent-core)
  (load (expand-file-name
         "agent.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil
        'nomessage))

(provide 'ai-agent)
;;; ai-agent.el ends here
