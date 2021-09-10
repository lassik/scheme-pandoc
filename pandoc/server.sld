(define-library (pandoc server)
  (export pandoc-server)
  (cond-expand
    (chicken
     (import (scheme)
             (chicken base)
             (cjson)
             (only (scheme base) utf8->string)
             (only (chicken io) read-string)
             (only (http-client) with-input-from-request)
             (only (intarweb) headers make-request)
             (only (medea) write-json)
             (only (uri-common) uri-reference))
     (include "pandoc/server.chicken.scm"))))
