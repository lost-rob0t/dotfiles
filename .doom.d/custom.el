(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval let*
      ((root (locate-dominating-file default-directory "AGENTS.md"))
       (roam (and root (expand-file-name "roam" root)))
       (cache (and root (expand-file-name ".cache" root))))
      (when root
       (setq-local org-directory roam org-roam-directory (file-truename roam)
                   org-roam-db-location (expand-file-name "org-roam.db" cache)
                   org-id-locations-file
                   (expand-file-name "org-id-locations" cache))))
     (eval progn (setq-local sly-contribs '(sly-fancy))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
