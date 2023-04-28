(in-package #:nyxt-user)

;; Define buffer search-engines slot to be a list of several
;; nx-search-engines-provided ones.
;;
;(define-search-engine cve-mitre
;  (:shortcut "cve"
;   :fallback-url (quri:uri "https://cve.mitre.org/cve/search_cve_list.html")
;   :base-search-url "https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=~a"
;   :documentation "Search the CVE database. mitre allows you to search them by keyword"))
(define-configuration (buffer web-buffer)
  ((search-engines (list (engines:google :shortcut "gmaps"
                                         :object :maps)
                         (engines:wordnet :shortcut "wn"
                                          :show-word-frequencies t)
                         (engines:google :shortcut "g"
                                         :safe-search nil)
                         (engines:duckduckgo :theme :terminal
                                             :help-improve-duckduckgo nil
                                             :homepage-privacy-tips nil
                                             :privacy-newsletter nil
                                             :newsletter-reminders nil
                                             :install-reminders nil
                                             :install-duckduckgo nil)
                         (engines:github :object :advanced :shortcut "git")
                         (engines:brave :shortcut "b")
                         (nyxt:make-search-engine "cve" "https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=~a" "https://cve.mitre.org/cve/search_cve_list.html")

                         (engines:brave :shortcut "b")
))))
