;; [[file:packages.org::+begin_src emacs-lisp][No heading:1]]
;; -*- lexical-binding: t -*-
;; No heading:1 ends here

;; [[file:packages.org::*org-ql][org-ql:1]]
(package! org-ql :recipe (:type git :host github :repo "alphapapa/org-ql"))
;; org-ql:1 ends here

;; [[file:packages.org::*org-timed-alerts][org-timed-alerts:1]]
(package! org-timed-alerts :recipe (:type git :host github :repo "legalnonsense/org-timed-alerts"))
;; org-timed-alerts:1 ends here

;; [[file:packages.org::*org-download][org-download:1]]
(package! org-download :recipe (:type git :host github :repo "abo-abo/org-download"))
;; org-download:1 ends here

;; [[file:packages.org::*org-wiki][org-wiki:1]]
(package! org-wiki :recipe (:type git :host github :repo "caiorss/org-wiki"))
;; org-wiki:1 ends here

;; [[file:packages.org::*ox-hugo][ox-hugo:1]]
(package! ox-hugo)
;; ox-hugo:1 ends here

;; [[file:packages.org::*envrc][envrc:1]]
(package! envrc :recipe (:type git :host github :repo "purcell/envrc"))
;; envrc:1 ends here

;; [[file:packages.org::*nim-mode][nim-mode:1]]
(use-package nim-mode
  :ensure t
  :hook
  (nim-mode . lsp))
;; nim-mode:1 ends here

;; [[file:packages.org::*flycheck-nim][flycheck-nim:1]]
(package! flycheck-nim :recipe (:type git :host github :repo "ALSchwalm/flycheck-nim"))
;; flycheck-nim:1 ends here

;; [[file:packages.org::*gForth][gForth:1]]
(package! gforth.el :recipe (:type git :host github :repo "smtlaissezfaire/gforth.el"))
;; gForth:1 ends here

;; [[file:packages.org::*webpaste.el][webpaste.el:1]]
(package! webpaste :recipe (:type git :host github :repo "etu/webpaste.el"))
;; webpaste.el:1 ends here

;; [[file:packages.org::*Burly][Burly:1]]
(package! burly :recipe (:type git :host github :repo "alphapapa/burly.el"))
;; Burly:1 ends here

;; [[file:packages.org::*podman.el][podman.el:1]]
(package! podman.el :recipe (:type git :host github :repo "akirak/podman.el"))
;; podman.el:1 ends here

;; [[file:packages.org::*pcap-mode.el][pcap-mode.el:1]]
(package! pcap-mode.el :recipe (:type git :host github :repo "orgcandman/pcap-mode"))
;; pcap-mode.el:1 ends here

;; [[file:packages.org::*exec-path-from-shell][exec-path-from-shell:1]]
(package! exec-path-from-shell  :recipe (:type git :host github :repo "purcell/exec-path-from-shell"))
;; exec-path-from-shell:1 ends here

;; [[file:packages.org::*cheat-sh][cheat-sh:1]]
(package! cheat-sh :recipe (:type git :host github :repo "davep/cheat-sh.el"))
;; cheat-sh:1 ends here

;; [[file:packages.org::*activity watch][activity watch:1]]
(package! activity-watch-mode :recipe (:type git :host github :repo "pauldub/activity-watch-mode"))
;; activity watch:1 ends here

;; [[file:packages.org::*plz][plz:1]]
(package! plz :recipe (:type git :host github :repo "alphapapa/plz.el"))
;; plz:1 ends here

;; [[file:packages.org::*ts][ts:1]]
(package! ts :recipe (:type git :host github :repo "alphapapa/ts.el"))
;; ts:1 ends here

;; [[file:packages.org::*dash][dash:1]]
(package! dash :recipe (:type git :host github :repo "magnars/dash.el"))
;; dash:1 ends here

;; [[file:packages.org::*ement][ement:1]]
(package! ement :recipe (:type git :host github :repo "alphapapa/ement.el"))
;; ement:1 ends here

;; [[file:packages.org::*inherit-org][inherit-org:1]]
(package! inherit-org :recipe (:host github :repo "chenyanming/inherit-org"))
;; inherit-org:1 ends here

;; [[file:packages.org::*alert][alert:1]]
(use-package alert
  :config (setq alert-default-style
            (if ns/enable-windows-p
              'toaster
              'libnotify)))
;; alert:1 ends here

;; [[file:packages.org::*misc][misc:1]]
(use-package shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(use-package nov
  :defer t
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))

(use-package anki
  :defer t
  :load-path "~/.emacs.d/lisp/anki/"
  :init
  (add-hook 'anki-mode-hook #'shrface-mode)
  (autoload 'anki "anki")
  (autoload 'anki-browser "anki")
  (autoload 'anki-list-decks "anki")
  :config
  (require 'shrface)
  (setq anki-shr-rendering-functions (append anki-shr-rendering-functions shr-external-rendering-functions))
  (setq sql-sqlite-program "/usr/bin/sqlite3")
  (setq anki-collection-dir "/Users/chandamon/Library/Application Support/Anki2/User 1"))
;; misc:1 ends here
