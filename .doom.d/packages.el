;; -*- lexical-binding: t -*-

;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives
      '(("GNU ELPA" . "http://elpa.gnu.org/packages/") ("MELPA Stable" . "https://stable.melpa.org/packages/") ("MELPA" . "https://melpa.org/packages/"))
      ;; Prefer MELPA Stable over GNU over MELPA. IOW prefer MELPA's stable
      ;; packages over everything and only fall back to GNU or MELPA if ;; necessary.
      package-archive-priorities '(("MELPA Stable" . 10) ("GNU ELPA" . 5) ("MELPA" . 0))) (package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) (package-refresh-contents) (package-install 'use-package))

(eval-when-compile (require 'use-package))

(unpin! org-roam)
(package! org-roam-ui)

(package! org-ql :recipe (:type git :host github :repo "alphapapa/org-ql"))

(package! org-timed-alerts :recipe (:type git :host github :repo "legalnonsense/org-timed-alerts"))

(package! org-alert)

(package! org-download :recipe (:type git :host github :repo "abo-abo/org-download"))

;; (package! ox-hugo)

(package! org-present :recipe (:type git :host github :repo "rlister/org-present"))

;; (package! org-pomodoro)

(package! org-modern)

(package! writeroom-mode)

(package! ob-ffuf :recipe (:type git :host github :repo "daniel-ts/ob-ffuf"))

(package! ob-http)

(package! ob-graphql)

(package! org-recoll :recipe (:type git :host github :repo "alraban/org-recoll"))

(package! project-tasks :recipe (:type git :host github :repo "TxGVNN/project-tasks"))

;(package! codeium :recipe (:type git :host github :repo "Exafunction/codeium.el"))

(package! envrc :recipe (:type git :host github :repo "purcell/envrc"))

(package! protobuf-mode)

(unpin! nim-mode)
(unpin! nim-mode :pin "1338e5b0d5e111ad932efb77d3cad680cc3b86c9")

;;(package! flycheck-nim :recipe (:type git :host github :repo "ALSchwalm/flycheck-nim"))

;(package! gforth.el :recipe (:type git :host github :repo "smtlaissezfaire/gforth.el"))

(package! lsp-pyright :recipe (:type git :host github :repo "emacs-lsp/lsp-pyright"))

(package! evil-smartparens)

(package! ob-prolog)

(package! gnuplot)

(package! hackmode :recipe (:type git :host github :repo "lost-rob0t/emacs-hackmode"))

(package! webpaste :recipe (:type git :host github :repo "etu/webpaste.el"))

(package! burly :recipe (:type git :host github :repo "alphapapa/burly.el"))

(package! podman.el :recipe (:type git :host github :repo "akirak/podman.el"))

(package! pcap-mode.el :recipe (:type git :host github :repo "orgcandman/pcap-mode"))

(package! exec-path-from-shell  :recipe (:type git :host github :repo "purcell/exec-path-from-shell"))

(package! cheat-sh :recipe (:type git :host github :repo "davep/cheat-sh.el"))

(package! activity-watch-mode :recipe (:type git :host github :repo "pauldub/activity-watch-mode"))

(package! discover :recipe (:type git :host github :repo "mickeynp/discover.el"))

(package! atomic-chrome)

;;(package! noaa.el :recipe (:type git :host github :repo "thomp/noaa"))

(package! app-launcher :recipe (:type git :host github :repo "SebastienWae/app-launcher"))

(package! yasnippet-snippets)

(package! plz :recipe (:type git :host github :repo "alphapapa/plz.el"))

(package! ts :recipe (:type git :host github :repo "alphapapa/ts.el"))

(package! dash :recipe (:type git :host github :repo "magnars/dash.el"))

(package! s :recipe (:type git :host github :repo "magnars/s.el"))

(package! alert :recipe (:type git :host github :repo "jwiegley/alert"))

(package! f)

(package! emacs-async :recipe (:type git :host github :repo "jwiegley/emacs-async"))

(package! triples :recipe (:type git :host github :repo "ahyatt/triples"))

(package! emacsql :recipe (:type git :host github :repo "magit/emacsql"))

(package! msgpack.el :recipe (:type git :host github :repo "xuchunyang/msgpack.el"))

(package! emacs-kv :recipe (:type git :host github :repo "nicferrier/emacs-kv"))

(package! jeison)

(package! org-contrib)

(package! ement :recipe (:type git :host github :repo "alphapapa/ement.el"))

(package! mastodon)

(package! elcord :recipe (:type git :host github :repo "Mstrodl/elcord"))

(package! elfeed-tube)

;(package! inherit-org :recipe (:host github :repo "chenyanming/inherit-org"))

(package! golden-ratio.el :recipe (:host github :repo "roman/golden-ratio.el"))

(package! dirvish :recipe (:host github :repo "alexluigit/dirvish"))

(package! ks-mode :recipe (:type git :host github :repo "jarpy/ks-mode"))

;(package! flyspell-lazy :disable t)

(package! libvirt :recipe (:type git :host github :repo "lost-rob0t/libvirt-el"))

;(package! exwm)

;(package! exwm-firefox-evil)

;(package! perspective )
;(package! perspective-exwm)

;(package! )
