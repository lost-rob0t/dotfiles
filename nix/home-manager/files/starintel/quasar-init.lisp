;; Operator-owned after first start. Credentials stay in runtime secret sources.
(in-package #:quasar.config)
(setf *autodig-persistence-backend* :tek9
      *log-sink* :stdout)
