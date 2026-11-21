;;; engagement.el --- Generate authorized engagement scope documents -*- lexical-binding: t; -*-

;; Generate OSINT target scoping and pentest engagement documents from one
;; generic template.  Every engagement requires written authorization on
;; file; the generated document records its reference and signatory before
;; any work starts.

;;; Code:

(require 'subr-x)

(defgroup engagement nil
  "Authorized engagement scope documents."
  :group 'tools)

(defcustom nsa/engagement-directory "~/Documents/engagements/"
  "Directory where generated engagement documents are saved."
  :type 'directory)

(defconst nsa/engagement--template
  "RECORD TYPE : ENGAGEMENT SCOPE / AUTHORIZATION
TITLE        : %T
TYPE         : %k
STATUS       : DRAFT
DATE         : %d
ENGAGEMENT WINDOW : %s .. %e

== AUTHORIZATION ==
Written authorization: ON FILE (ref: %r)
Signatory: %a (%o)
Authorization date: %A
No work may start or continue outside the authorized scope above.
All findings and evidence are for this authorized engagement only.

== TARGET IDENTITY ==
Primary target: %P
Also known as / aliases: %m
Scope level: ALL ASSETS IN SCOPE

== IN SCOPE (ALL ASSETS) ==
- Apex domain and all subdomains (wildcard scope)
- Hosted web applications, forums, APIs, CDNs and static assets
- Infrastructure directly controlled by the target: servers, IP
  space, nameservers, mail (MX), TLS certificates
- Third-party assets only where target-operated or explicitly
  named in the authorization

== OUT OF SCOPE ==
- Denial of service, flooding, or resource exhaustion
- Social engineering of personnel; physical intrusion
- Compromise, alteration, or destruction of target data
- Any asset not owned or operated by the target (upstream hosts,
  registrars, CDN providers) unless the authorization names them

== RULES OF ENGAGEMENT ==
- Passive collection first; rate-limit all active probing
- No disruption of service availability at any time
- Log every query, URL, timestamp, and retrieval hash
- Stop and escalate immediately on discovery of exposed personal
  data or third-party impact
- Test activity stays inside the engagement window

== EVIDENCE AND PROVENANCE ==
- Every observation carries: source URL, publisher, retrieval
  time (UTC), locator, archive URL where applicable
- Observations, attributed claims, and analysis stay distinct
- Negative searches are logged; conflicts preserved, not averaged
- No manufactured or unsourced data enters the record

== DATA HANDLING ==
- Store under dataset: %D
- Retention per authorization terms
- Do not publish findings outside authorized channels

== NOTES ==
%n
")

(defun nsa/engagement--slug (string)
  "Return STRING as a filename-safe slug."
  (let ((slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase string))))
    (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug)))

(defun nsa/engagement--field (fields key)
  "Return FIELDS value for KEY or signal a user error when unset."
  (or (plist-get fields key)
      (user-error "Engagement field missing: %s" key)))

(defun nsa/engagement--render (fields)
  "Render the engagement template with FIELDS (a plist)."
  (format-spec nsa/engagement--template
               (list (cons ?T (nsa/engagement--field fields :title))
                     (cons ?k (nsa/engagement--field fields :kind))
                     (cons ?d (nsa/engagement--field fields :date))
                     (cons ?s (nsa/engagement--field fields :start))
                     (cons ?e (nsa/engagement--field fields :end))
                     (cons ?r (nsa/engagement--field fields :auth-ref))
                     (cons ?a (nsa/engagement--field fields :signatory))
                     (cons ?o (nsa/engagement--field fields :role))
                     (cons ?A (nsa/engagement--field fields :auth-date))
                     (cons ?P (nsa/engagement--field fields :target))
                     (cons ?m (nsa/engagement--field fields :aliases))
                     (cons ?D (nsa/engagement--field fields :dataset))
                     (cons ?n (nsa/engagement--field fields :notes)))))

(defun nsa/engagement--read-fields (kind)
  "Prompt for engagement fields and return them as a plist with KIND."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (target (string-trim (read-string "Primary target (domain or organization): ")))
         (title (read-string (format "Title [%s - All Assets]: " target)
                             nil nil (format "%s - All Assets" target)))
         (aliases (read-string "Aliases, mirrors, parked domains [none]: " nil nil "NONE"))
         (auth-ref (read-string "Authorization reference (e.g. AUTH-2026-001): "))
         (signatory (read-string "Authorization signatory: "))
         (role (read-string "Signatory role / organization: "))
         (auth-date (read-string (format "Authorization date [%s]: " today) nil nil today))
         (start (read-string (format "Window start [%s]: " today) nil nil today))
         (end (read-string "Window end [OPEN]: " nil nil "OPEN"))
         (dataset (read-string "Dataset name: "))
         (notes (read-string "Notes [none]: " nil nil "NONE")))
    (list :kind kind
          :title (string-trim title)
          :target target
          :aliases aliases
          :auth-ref auth-ref
          :signatory signatory
          :role role
          :auth-date auth-date
          :date today
          :start start
          :end end
          :dataset dataset
          :notes notes)))

(defun nsa/engagement--display (document)
  "Show rendered DOCUMENT in a buffer and offer to save it."
  (let* ((slug (nsa/engagement--slug (plist-get document :title)))
         (buffer (get-buffer-create (format "*engagement: %s*" slug))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (nsa/engagement--render document))
      (text-mode)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    (when (y-or-n-p "Save engagement document to file? ")
      (let* ((directory (file-name-as-directory
                         (expand-file-name nsa/engagement-directory)))
             (default (expand-file-name
                       (format "%s-%s.txt" slug (plist-get document :date))
                       directory)))
        (make-directory directory t)
        (write-file (read-file-name "Save engagement document: "
                                    directory default nil
                                    (file-name-nondirectory default))
                    t)))))

;;;###autoload
(defun nsa/engagement-target-document ()
  "Generate an OSINT target scoping document from the generic template."
  (interactive)
  (nsa/engagement--display (nsa/engagement--read-fields "OSINT TARGET SCOPING")))

;;;###autoload
(defun nsa/engagement-pentest-document ()
  "Generate a pentest engagement document from the generic template."
  (interactive)
  (nsa/engagement--display (nsa/engagement--read-fields "PENTEST ENGAGEMENT")))

(provide 'engagement)
;;; engagement.el ends here
