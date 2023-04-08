(in-package #:nyxt-user)

;; Define buffer search-engines slot to be a list of several
;; nx-search-engines-provided ones.
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
                         (engines:brave :shortcut "b")))))
