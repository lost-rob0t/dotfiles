;;; starintel-ide-init.el --- starintel-ide in live Doom -*- lexical-binding: t; -*-

;; Human-first StarIntel IDE. All AI flows through Consortium END TO END
;; (ui -> session -> process -> daemon -> Prolog-RLM -> provider); the
;; z.ai GLM Coding Plan is the provider. Fail-open: if anything below is
;; missing, editing still works (I4).

;;; Code:

;; Load the IDE from the ai-surfaces worktree until the PR stack lands.
;; Switch this path to the primary checkout after merging #1-#4.
(defvar starintel-ide-source-dir
  (expand-file-name "~/git/worktrees/starintel-ide-ai-surfaces"))

(when (file-exists-p (expand-file-name "starintel-ide.el" starintel-ide-source-dir))
  (add-to-list 'load-path starintel-ide-source-dir)
  ;; The Consortium emacs stack (client/process/session/config/ui).
  ;; Points at the fix-daemon-stderr worktree (consortium PR #8) until
  ;; it merges; then revert to /home/unseen/starintel/consortium/emacs.
  (add-to-list 'load-path
               "/home/unseen/git/worktrees/consortium-fix-daemon-stderr/emacs")
  (require 'starintel-ide)

  ;; Provider: z.ai GLM Coding Plan (plan quota only). Secret comes from
  ;; auth-source (machine api.z.ai) or the ZAI_API_KEY env var -- never
  ;; stored here.
  (setq consortium-rlm-config-provider 'zai
        consortium-rlm-config-models "glm-5.3"   ; your coding-plan model
        ;; Daemon: ~/.local/bin/consortium wraps the nix-built flake app
        ;; (nix build consortium#consortium --out-link
        ;;  ~/.local/state/consortium-daemon). Defaults are correct:
        ;; program "consortium", args ("daemon").
        )

  (starintel-ide-global-mode 1))

;; Quick test surface (also fine from M-x):
;;   C-c a c  chat sidebar (consortium e2e)
;;   C-c a q  ask about the active region
;;   C-c a d  documentation draft for the region
;;   C-c r t  run-log capture   C-c r l  run-log buffer
;;   C-c i s  symbol lookup     M-x starintel-ide-status

(provide 'starintel-ide-init)
;;; starintel-ide-init.el ends here
