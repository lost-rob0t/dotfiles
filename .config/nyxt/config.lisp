;; Load slynk

(in-package :nyxt)

(when (asdf:load-system :slynk)
  (define-command start-slynk (&optional (slynk-port 4006))
    "Start a Slynk server that can be connected to, for instance, in
Emacs via SLY.

Warning: This allows Nyxt to be controlled remotely, that is, to
execute arbitrary code with the privileges of the user running Nyxt.
Make sure you understand the security risks associated with this
before running this command."
    (slynk:create-server :port slynk-port :dont-close t)
    (echo "Slynk server started at port ~a" slynk-port)))


(in-package #:nyxt-user)

;; Auto config stuff
(define-configuration (web-buffer prompt-buffer panel-buffer
                                  nyxt/mode/editor:editor-buffer)
  ((default-modes (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))



(define-configuration (web-buffer)
  ((default-modes (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))

(define-configuration (web-buffer)
  ((default-modes
    (pushnew 'nyxt/mode/reduce-tracking:reduce-tracking-mode %slot-value%))))

(define-configuration (web-buffer)
  ((default-modes (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))



;; Make the Web buffers vi mode.
(define-configuration prompt-buffer
  ((default-modes (append '(vi-insert-mode) %slot-default%))))


(defvar *my-search-engines*
  (list
   '("google" "https://google.com/search?q=~a" "https://google.com")
   '("sp" "https://www.startpage.com/do/search?query=~a" "https://www.startpage.com")
   '("py" "https://docs.python.org/3/search.html?q=~a"
     "https://docs.python.org/3")
   '("fec" "https://www.fec.gov/data/receipts/individual-contributions/?contributor_name=~a" "https://www.fec.gov/data/receipts/individual-contributions/")
   '("nixpkgs" "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=~a" "https://search.nixos.org/packages")
   '("brave" "https://search.brave.com/search?q=~a" "https://search.brave.com/search"))

  "List of search engines.")

(define-configuration context-buffer
  "Go through the search engines above and make-search-engine out of them."
  ((search-engines
    (append
     (mapcar (lambda (engine) (apply 'make-search-engine engine))
             *my-search-engines*)
     %slot-default%))))




(nyxt::load-lisp "~/.config/nyxt/proxy.lisp")
(nyxt::load-lisp "~/.config/nyxt/status.lisp")
(nyxt::load-lisp "~/.config/nyxt/crunchbase.lisp")
