(module (pandoc server)

  (#;export
   pandoc-server)

  (import (scheme)
          (chicken base)
          (cjson)
          (only (scheme base) utf8->string)
          (only (chicken io) read-string)
          (only (http-client) with-input-from-request)
          (only (intarweb) headers make-request)
          (only (medea) write-json)
          (only (uri-common) uri-reference))

  (define (pandoc-server base-url)
    (lambda (input-format bytevectors)
      (with-input-from-request
       (make-request
        method: 'POST
        uri: (uri-reference (string-append base-url "convert-batch"))
        headers: (headers '((content-type "application/json")
                            (accept "application/json"))))
       (lambda ()
         (let ((input-format (symbol->string input-format)))
           (write-json
            (list->vector
             (map (lambda (bytevector)
                    (list (cons 'from input-format)
                          (cons 'to "json")
                          (cons 'text (utf8->string bytevector))))
                  bytevectors)))))
       (lambda ()
         (let ((array (string->cjson (read-string))))
           (unless (eq? cjson/array (cjson-type array))
             (error "Got unexpected JSON from pandoc-server"))
           (let loop ((i (- (cjson-array-size array) 1)) (results '()))
             (if (< i 0) results
                 (loop (- i 1)
                       (cons (cjson-schemify
                              (string->cjson
                               (cjson-schemify
                                (cjson-array-ref array i))))
                             results))))))))))
