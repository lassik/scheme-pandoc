(define-library (pandoc internal json)
  (export json-read
          json-read-from-bytevector)
  (import (scheme base))
  (cond-expand
    (chicken
     (import (cjson)
             (rename (only (medea) read-json)
                     (read-json json-read)))
     (begin (define (json-read-from-bytevector bytes)
              (cjson-schemify (string->cjson (utf8->string bytes))))))
    (gauche
     (import (only (srfi 180) json-read))
     (begin (define (json-read-from-bytevector bytes)
              (call-with-port (open-input-bytevector bytes)
                              json-read))))))
