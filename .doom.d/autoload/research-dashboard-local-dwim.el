;;; research-dashboard-local-dwim.el --- DWIM forge identity for research dashboard -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)
(require 'research-dashboard-local)

(defvar-local nsa/research-dashboard-local-dwim--identities nil
  "Map local research item ids to forge/provider identity metadata.")

(defun nsa/research-dashboard-local-dwim--command (directory program &rest args)
  "Run PROGRAM ARGS in DIRECTORY and return trimmed stdout on success."
  (when (executable-find program)
    (let ((default-directory (file-name-as-directory directory)))
      (with-temp-buffer
        (when (zerop (apply #'process-file program nil t nil args))
          (string-trim (buffer-string)))))))

(defun nsa/research-dashboard-local-dwim--parse-remote (remote)
  "Parse REMOTE into a plist with :host, :slug and :ssh-user when available."
  (cond
   ((and remote
         (string-match
          "\\`\\([^/@:]+\\)@\\([^/:]+\\):\\(.+\\)\\'" remote))
    (list :ssh-user (match-string 1 remote)
          :host (downcase (match-string 2 remote))
          :slug (string-remove-suffix ".git" (match-string 3 remote))))
   ((and remote
         (string-match
          "\\`ssh://\\(?:\\([^/@]+\\)@\\)?\\([^/:]+\\)\\(?::[0-9]+\\)?/\\(.+\\)\\'"
          remote))
    (list :ssh-user (match-string 1 remote)
          :host (downcase (match-string 2 remote))
          :slug (string-remove-suffix ".git" (match-string 3 remote))))
   ((and remote
         (string-match
          "\\`https?://\\([^/]+\\)/\\(.+\\)\\'" remote))
    (list :host (downcase (match-string 1 remote))
          :slug (string-remove-suffix ".git" (match-string 2 remote))))
   (t nil)))

(defun nsa/research-dashboard-local-dwim--json-login (text)
  "Return a forge login from JSON TEXT, tolerating login/username/user keys."
  (when (and text (not (string-empty-p text)))
    (condition-case nil
        (let ((object (json-parse-string text :object-type 'alist)))
          (or (alist-get "login" object nil nil #'string=)
              (alist-get "username" object nil nil #'string=)
              (alist-get "user" object nil nil #'string=)))
      (error nil))))

(defun nsa/research-dashboard-local-dwim--github-info (repo-root parsed)
  "Resolve GitHub repository and actor metadata for REPO-ROOT and PARSED remote."
  (let* ((slug (or (nsa/research-dashboard-local-dwim--command
                    repo-root "gh" "repo" "view" "--json" "nameWithOwner"
                    "--jq" ".nameWithOwner")
                   (plist-get parsed :slug)))
         (actor (nsa/research-dashboard-local-dwim--command
                 repo-root "gh" "api" "user" "--jq" ".login")))
    (list :provider 'github :host "github.com" :repo slug :actor actor)))

(defun nsa/research-dashboard-local-dwim--tea-info (repo-root parsed)
  "Resolve Forgejo/Gitea actor metadata for REPO-ROOT using tea context."
  (let* ((raw (nsa/research-dashboard-local-dwim--command
               repo-root "tea" "api" "/user"))
         (actor (nsa/research-dashboard-local-dwim--json-login raw))
         (host (plist-get parsed :host))
         (slug (plist-get parsed :slug)))
    (when actor
      (list :provider 'forgejo
            :host host
            :repo (and slug host (format "%s/%s" host slug))
            :actor actor))))

(defun nsa/research-dashboard-local-dwim--forge-info (repo-root remote)
  "DWIM forge metadata for REPO-ROOT from REMOTE.

GitHub is resolved through `gh'.  Other remotes are offered to `tea', which
uses the checkout's Git remote context to select the matching Forgejo/Gitea
login.  Plain Git parsing remains the final fallback."
  (let* ((parsed (nsa/research-dashboard-local-dwim--parse-remote remote))
         (host (plist-get parsed :host))
         (slug (plist-get parsed :slug)))
    (cond
     ((string= host "github.com")
      (nsa/research-dashboard-local-dwim--github-info repo-root parsed))
     ((and parsed
           (nsa/research-dashboard-local-dwim--tea-info repo-root parsed)))
     (parsed
      (list :provider 'git
            :host host
            :repo (if (and host slug) (format "%s/%s" host slug) slug)
            :actor nil))
     (t nil))))

(defun nsa/research-dashboard-local-dwim--checkout-info-advice
    (original directory)
  "Augment checkout info with DWIM provider and authenticated actor identity."
  (let* ((info (funcall original directory))
         (repo-root (plist-get info :repo-root))
         (remote (nsa/research-dashboard-local--git
                  repo-root "remote" "get-url" "origin"))
         (forge (and remote
                     (nsa/research-dashboard-local-dwim--forge-info
                      repo-root remote))))
    (when forge
      (let ((repo (plist-get forge :repo)))
        (when (and repo (not (string-empty-p repo)))
          (setq info (plist-put info :repo repo))))
      (setq info (plist-put info :provider (plist-get forge :provider))
            info (plist-put info :host (plist-get forge :host))
            info (plist-put info :actor (plist-get forge :actor))
            info (plist-put info :remote remote)))
    info))

(defun nsa/research-dashboard-local-dwim--item-advice (original info file)
  "Remember forge identity metadata for the item built from INFO and FILE."
  (let ((item (funcall original info file)))
    (when item
      (unless (hash-table-p nsa/research-dashboard-local-dwim--identities)
        (setq nsa/research-dashboard-local-dwim--identities
              (make-hash-table :test #'equal)))
      (puthash
       (nsa/research-dashboard-local--id item)
       (list :provider (plist-get info :provider)
             :host (plist-get info :host)
             :actor (plist-get info :actor)
             :remote (plist-get info :remote))
       nsa/research-dashboard-local-dwim--identities))
    item))

(defun nsa/research-dashboard-local-dwim--scan-advice (original &rest args)
  "Reset identity metadata before rescanning local sources."
  (setq nsa/research-dashboard-local-dwim--identities
        (make-hash-table :test #'equal))
  (apply original args))

(defun nsa/research-dashboard-local-dwim--identity (item)
  "Return DWIM forge identity metadata for ITEM."
  (and (hash-table-p nsa/research-dashboard-local-dwim--identities)
       (gethash (nsa/research-dashboard-local--id item)
                nsa/research-dashboard-local-dwim--identities)))

(defun nsa/research-dashboard-local-dwim--decide-advice
    (original state item file)
  "Use the forge-authenticated actor as the local decision prompt default."
  (let* ((identity (nsa/research-dashboard-local-dwim--identity item))
         (actor (plist-get identity :actor))
         (nsa/research-dashboard--login
          (or actor nsa/research-dashboard--login)))
    (funcall original state item file)))

(defun nsa/research-dashboard-local-dwim--install ()
  "Install DWIM provider/identity behavior once."
  (unless (advice-member-p
           #'nsa/research-dashboard-local-dwim--checkout-info-advice
           'nsa/research-dashboard-local--checkout-info)
    (advice-add 'nsa/research-dashboard-local--checkout-info :around
                #'nsa/research-dashboard-local-dwim--checkout-info-advice)
    (advice-add 'nsa/research-dashboard-local--item :around
                #'nsa/research-dashboard-local-dwim--item-advice)
    (advice-add 'nsa/research-dashboard-local--scan-current-buffer :around
                #'nsa/research-dashboard-local-dwim--scan-advice)
    (advice-add 'nsa/research-dashboard-local--decide :around
                #'nsa/research-dashboard-local-dwim--decide-advice)))

;;;###autoload
(add-hook 'nsa/research-dashboard-mode-hook
          #'nsa/research-dashboard-local-dwim--install)

(provide 'research-dashboard-local-dwim)
;;; research-dashboard-local-dwim.el ends here
