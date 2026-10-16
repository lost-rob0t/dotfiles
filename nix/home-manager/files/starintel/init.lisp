;; Operator-owned after first start. This file is executable Common Lisp.
(in-package :star)
(setf *couchdb-host* "127.0.0.1"
      *couchdb-port* 5984
      *couchdb-user* (or (uiop:getenv "COUCHDB_USER") "starintel")
      *couchdb-password* (environment-secret "COUCHDB_PASSWORD" "COUCHDB_PASSWORD_FILE")
      *rabbit-address* "127.0.0.1"
      *rabbit-port* 5672
      *rabbit-user* "starintel"
      *rabbit-password* (environment-secret "RABBITMQ_PASSWORD" "RABBITMQ_PASSWORD_FILE")
      *http-api-address* "127.0.0.1"
      *http-api-port* 5000
      *http-cors-allowed-origins* '("http://127.0.0.1:8080")
      *auth-mode* "api-key"
      *auth-dev-bypass* nil
      *auth-pepper* (environment-secret "STAR_AUTH_PEPPER" "STAR_AUTH_PEPPER_FILE")
      *auth-bootstrap-secret* (environment-secret "STAR_AUTH_BOOTSTRAP_SECRET" "STAR_AUTH_BOOTSTRAP_SECRET_FILE")
      *auth-initial-password* (environment-secret "STAR_AUTH_INITIAL_PASSWORD" "STAR_AUTH_INITIAL_PASSWORD_FILE")
      *public-mode* nil
      *lease-store-backend* "valkey"
      *valkey-lease-host* "127.0.0.1"
      *valkey-lease-port* 6379
      *valkey-lease-password-file* (uiop:getenv "VALKEY_PASSWORD_FILE"))
;; Add trusted local actor/addon loading here. Never put passwords in this file.
