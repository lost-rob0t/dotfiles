;; Nyxt configuration.
;; The visual layer is in theme.lisp; capture/reporting commands are in
;; reporting.lisp.

(in-package :nyxt)

(when (asdf:load-system :slynk)
  (define-command start-slynk (&optional (slynk-port 4006))
    "Start a Slynk server for SLY.

This exposes arbitrary code execution with the privileges of the Nyxt process.
Only run it on a trusted machine/session."
    (slynk:create-server :port slynk-port :dont-close t)
    (echo "Slynk server started at port ~a" slynk-port)))

(in-package #:nyxt-user)

;; Vi where navigation is useful, insert mode in the command/prompt UI.
(define-configuration (web-buffer panel-buffer nyxt/mode/editor:editor-buffer)
  ((default-modes
    (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

(define-configuration prompt-buffer
  ((default-modes
    (pushnew 'nyxt/mode/vi:vi-insert-mode %slot-value%))))

;; Privacy defaults.
(define-configuration web-buffer
  ((default-modes
    (remove-duplicates
     (append
      '(nyxt/mode/blocker:blocker-mode
        nyxt/mode/reduce-tracking:reduce-tracking-mode)
      %slot-value%)
     :test #'eq))))

(defvar *my-search-engines*
  (list
   '("google" "https://google.com/search?q=~a" "https://google.com")
   '("brave" "https://search.brave.com/search?q=~a" "https://search.brave.com")
   '("sp" "https://www.startpage.com/do/search?query=~a" "https://www.startpage.com")
   '("gh" "https://github.com/search?q=~a" "https://github.com")
   '("py" "https://docs.python.org/3/search.html?q=~a" "https://docs.python.org/3")
   '("cve" "https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=~a")
   '("fec"
     "https://www.fec.gov/data/receipts/individual-contributions/?contributor_name=~a"
     "https://www.fec.gov/data/receipts/individual-contributions/")
   '("nixpkgs"
     "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=~a"
     "https://search.nixos.org/packages"))
  "Search engines available from the prompt.")

(define-configuration context-buffer
  ((search-engines
    (append
     (mapcar (lambda (engine)
               (apply #'make-search-engine engine))
             *my-search-engines*)
     %slot-default%))))

(define-configuration browser
  ((external-editor-program
    (list "emacsclient" "-c" "-F" "'(name . \"floating\")"))))

;; Retain the compositor workaround that fixed blank WebKit rendering on this
;; setup.
(setf (uiop:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1")

;; Load visual configuration before status configuration so the status buffer
;; picks up the synthwave theme.
(nyxt::load-lisp "~/.config/nyxt/theme.lisp")
(nyxt::load-lisp "~/.config/nyxt/status.lisp")
(nyxt::load-lisp "~/.config/nyxt/reporting.lisp")
(nyxt::load-lisp "~/.config/nyxt/starintel.lisp")
(nyxt::load-lisp "~/.config/nyxt/quasar.lisp")
(nyxt::load-lisp "~/.config/nyxt/proxy.lisp")
(nyxt::load-lisp "~/.config/nyxt/crunchbase.lisp")
