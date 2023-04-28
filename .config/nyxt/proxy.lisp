(in-package :nyxt-user)

;; source https://discourse.atlas.engineer/t/tor-v3-onion-address-auto-mode-rule/395/6
(define-mode tor-proxy-mode (nyxt/proxy-mode:proxy-mode)
  "Set proxy to local Tor SOCKS5 proxy."
  ((nyxt/proxy-mode:proxy (make-instance 'proxy
                                         :url (quri:uri "socks5://localhost:9050")
                                         :allowlist '("localhost")
                                         :proxied-downloads-p t))))


(define-mode i2p-proxy-mode (nyxt/proxy-mode:proxy-mode)
  "Set proxy to local i2p SOCKS5 proxy."
  ((nyxt/proxy-mode:proxy (make-instance 'proxy
                                         :url (quri:uri "socks5://localhost:9090")
                                         :allowlist '("localhost" "127.0.0.1")
                                         :proxied-downloads-p t))))


(define-mode zap-mode (nyxt/proxy-mode:proxy-mode)
  "ZAP Web app proxy."
  ((nyxt/proxy-mode:proxy (make-instance 'proxy
                                         :url (quri:uri "http://127.0.0.1:8080")
                                         :proxied-downloads-p t))))
