;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin!)
(package! ox-hugo)
;; Install `plz' HTTP library (not on MELPA yet).
;(use-package plz
;  :straight (plz :fetcher github :repo "alphapapa/plz.el"))

;; Install and load `quelpa-use-package'.
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;; Install `plz' HTTP library (not on MELPA yet).
;;(use-package plz
;;  :quelpa (plz :fetcher github :repo "alphapapa/plz.el"))

;; Install Ement.
;;(use-package ement
;;  :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

;;(use-package burly
;;  :quelpa (burly :fetcher github :repo "alphapapa/burly.el"))

(package! plz :recipe (:type git :host github :repo "alphapapa/plz.el"))
(package! ement :recipe (:type git :host github :repo "alphapapa/ement.el"))
(package! burly :recipe (:type git :host github :repo "alphapapa/burly.el"))
(package! org-ql :recipe (:type git :host github :repo "alphapapa/org-ql"))
(package! ts :recipe (:type git :host github :repo "alphapapa/ts.el"))
(package! dash :recipe (:type git :host github :repo "magnars/dash.el"))
(package! org-timed-alerts :recipe (:type git :host github :repo "legalnonsense/org-timed-alerts"))
(package! webpaste :recipe (:type git :host github :repo "etu/webpaste.el"))
(package! podman.el :recipe (:type git :host github :repo "akirak/podman.el"))
(package! org-wiki.el :recipe (:type git :host github :repo "caiorss/org-wiki"))
(package! pcap-mode.el :recipe (:type git :host github :repo "orgcandman/pcap-mode"))

;; I have issues with nim path becuase i use choosenim
(package! exec-path-from-shell  :recipe (:type git :host github :repo "purcell/exec-path-from-shell"))

(use-package alert
  :config (setq alert-default-style
            (if ns/enable-windows-p
              'toaster
              'libnotify
              )))

(use-package nim-mode
  :ensure t
  :hook
  (nim-mode . lsp))
