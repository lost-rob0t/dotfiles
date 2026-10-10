;; Operator-owned after first start. This file is executable Common Lisp.
(in-package :star)
(setf *couchdb-host* "127.0.0.1"
      *couchdb-port* 5984
      *couchdb-user* "starintel"
      *couchdb-password* (or (uiop:getenv "COUCHDB_PASSWORD") "")
      *rabbit-address* "127.0.0.1"
      *rabbit-port* 5672
      *rabbit-user* "starintel"
      *rabbit-password* (or (uiop:getenv "RABBITMQ_PASSWORD") "")
      *http-api-address* "127.0.0.1"
      *http-api-port* 5000
      *public-mode* nil
      *lease-store-backend* "valkey"
      *valkey-lease-host* "127.0.0.1"
      *valkey-lease-port* 6379
      *valkey-lease-password-file* (uiop:getenv "VALKEY_PASSWORD_FILE"))
;; Add trusted local actor/addon loading here. Never put passwords in this file.
