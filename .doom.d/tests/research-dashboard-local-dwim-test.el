;;; research-dashboard-local-dwim-test.el --- DWIM forge tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'research-dashboard)
(require 'research-dashboard-local)
(require 'research-dashboard-local-dwim)

(ert-deftest nsa/research-dashboard-local-dwim-parses-github-scp-ssh ()
  (let ((parsed
         (nsa/research-dashboard-local-dwim--parse-remote
          "git@github.com:lost-rob0t/prolog-rlm.git")))
    (should (equal (plist-get parsed :ssh-user) "git"))
    (should (equal (plist-get parsed :host) "github.com"))
    (should (equal (plist-get parsed :slug) "lost-rob0t/prolog-rlm"))))

(ert-deftest nsa/research-dashboard-local-dwim-parses-forgejo-scp-ssh ()
  (let ((parsed
         (nsa/research-dashboard-local-dwim--parse-remote
          "git@git.starintel.actor:nsaspy/dotfiles.git")))
    (should (equal (plist-get parsed :ssh-user) "git"))
    (should (equal (plist-get parsed :host) "git.starintel.actor"))
    (should (equal (plist-get parsed :slug) "nsaspy/dotfiles"))))

(ert-deftest nsa/research-dashboard-local-dwim-parses-ssh-url-with-port ()
  (let ((parsed
         (nsa/research-dashboard-local-dwim--parse-remote
          "ssh://git@git.starintel.actor:2222/nsaspy/star-kb.git")))
    (should (equal (plist-get parsed :ssh-user) "git"))
    (should (equal (plist-get parsed :host) "git.starintel.actor"))
    (should (equal (plist-get parsed :slug) "nsaspy/star-kb"))))

(ert-deftest nsa/research-dashboard-local-dwim-github-uses-gh-not-ssh-user ()
  (let (calls)
    (cl-letf (((symbol-function 'nsa/research-dashboard-local-dwim--command)
               (lambda (_directory program &rest args)
                 (push (cons program args) calls)
                 (cond
                  ((equal args '("repo" "view" "--json" "nameWithOwner"
                                 "--jq" ".nameWithOwner"))
                   "lost-rob0t/prolog-rlm")
                  ((equal args '("api" "user" "--jq" ".login"))
                   "lost-rob0t")))))
      (let ((info
             (nsa/research-dashboard-local-dwim--forge-info
              "/tmp/repo" "git@github.com:lost-rob0t/prolog-rlm.git")))
        (should (eq (plist-get info :provider) 'github))
        (should (equal (plist-get info :repo) "lost-rob0t/prolog-rlm"))
        (should (equal (plist-get info :actor) "lost-rob0t"))
        (should (seq-some (lambda (call) (equal (car call) "gh")) calls))
        (should-not (equal (plist-get info :actor) "git"))))))

(ert-deftest nsa/research-dashboard-local-dwim-forgejo-uses-tea-context ()
  (let (calls)
    (cl-letf (((symbol-function 'nsa/research-dashboard-local-dwim--command)
               (lambda (_directory program &rest args)
                 (push (cons program args) calls)
                 (when (and (equal program "tea")
                            (equal args '("api" "/user")))
                   "{\"login\":\"nsaspy\"}"))))
      (let ((info
             (nsa/research-dashboard-local-dwim--forge-info
              "/tmp/repo"
              "git@git.starintel.actor:nsaspy/starintel-auto-research.git")))
        (should (eq (plist-get info :provider) 'forgejo))
        (should (equal (plist-get info :host) "git.starintel.actor"))
        (should
         (equal (plist-get info :repo)
                "git.starintel.actor/nsaspy/starintel-auto-research"))
        (should (equal (plist-get info :actor) "nsaspy"))
        (should (seq-some (lambda (call) (equal (car call) "tea")) calls))))))

(ert-deftest nsa/research-dashboard-local-dwim-plain-git-falls-back-cleanly ()
  (cl-letf (((symbol-function 'nsa/research-dashboard-local-dwim--command)
             (lambda (&rest _args) nil)))
    (let ((info
           (nsa/research-dashboard-local-dwim--forge-info
            "/tmp/repo" "git@git.example.net:team/research.git")))
      (should (eq (plist-get info :provider) 'git))
      (should (equal (plist-get info :repo)
                     "git.example.net/team/research"))
      (should-not (plist-get info :actor)))))

(ert-deftest nsa/research-dashboard-local-dwim-decision-defaults-to-forge-user ()
  (with-temp-buffer
    (nsa/research-dashboard-mode)
    (setq nsa/research-dashboard-local-dwim--identities
          (make-hash-table :test #'equal))
    (let ((item
           (nsa/research-item-create
            :repo "git.starintel.actor/nsaspy/research"
            :branch "main"
            :path "research/example.org"
            :blob "blob"
            :title "Example"
            :lifecycle "RESEARCHED"
            :approval "PENDING"
            :content "body"))
          seen-login)
      (puthash
       "git.starintel.actor/nsaspy/research:research/example.org"
       '(:provider forgejo :actor "nsaspy")
       nsa/research-dashboard-local-dwim--identities)
      (let ((nsa/research-dashboard--login "lost-rob0t"))
        (nsa/research-dashboard-local-dwim--decide-advice
         (lambda (_state _item _file)
           (setq seen-login nsa/research-dashboard--login))
         "APPROVED" item "/tmp/example.org"))
      (should (equal seen-login "nsaspy")))))

(provide 'research-dashboard-local-dwim-test)
;;; research-dashboard-local-dwim-test.el ends here
