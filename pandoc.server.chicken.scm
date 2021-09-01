(module (pandoc server)

  (pandoc-server-base-url
   pandoc-server-strings->json
   pandoc-server-strings->sxml
   pandoc-server-files->json
   pandoc-server-files->sxml)

  (import (scheme)
          (chicken base)
          (cjson)
          (only (chicken port) with-input-from-string)
          (only (chicken io) read-string)
          (only (http-client) with-input-from-request)
          (only (intarweb) headers make-request)
          (only (medea) read-json write-json)
          (only (uri-common) uri-reference)
          (only (pandoc) pandoc-json->sxml))

  (define pandoc-server-base-url
    (make-parameter "http://localhost:8080/"))

  (define (pandoc-server-strings->json input-format input-strings)
    (with-input-from-request
     (make-request
      method: 'POST
      uri: (uri-reference (string-append (pandoc-server-base-url)
                                         "convert-batch"))
      headers: (headers '((content-type "application/json")
                          (accept "application/json"))))
     (lambda ()
       (let ((input-format (symbol->string input-format)))
         (write-json
          (list->vector
           (map (lambda (input-string)
                  (list (cons 'from input-format)
                        (cons 'to "json")
                        (cons 'text input-string)))
                input-strings)))))
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
                           results))))))))

  (define (pandoc-server-files->json input-format input-filenames)
    (pandoc-server-strings->json
     input-format
     (map (lambda (filename) (with-input-from-file filename read-string))
          input-filenames)))

  (define (pandoc-server-strings->sxml input-format input-strings)
    (map pandoc-json->sxml
         (pandoc-server-strings->json input-format input-strings)))

  (define (pandoc-server-files->sxml input-format input-filenames)
    (map pandoc-json->sxml
         (pandoc-server-files->json input-format input-filenames))))
