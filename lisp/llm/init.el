;;; init.el --- Load the Emacs LLM stack -*- lexical-binding: t; -*-

(require 'ai)
(require 'ai-agent)
(require 'ai-mcp)
(require 'chat)

(with-eval-after-load 'org-ql
  (require 'todo nil t))

(provide 'ai-init)
;;; init.el ends here
