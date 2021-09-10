(define-library (pandoc tar)
  (export pandoc-tar)
  (import (scheme base) (scheme case-lambda)
          (pandoc internal json)
          (pandoc internal subprocess))
  (include "tar.r7rs.scm")
  (begin

    (define (generator->list generator)  ; SRFI 158
      (let loop ((list '()))
        (let ((elem (generator)))
          (if (eof-object? elem) (reverse list) (loop (cons elem list))))))

    (define pandoc-tar
      (case-lambda
        (()
         (pandoc-tar "pandoc-tar"))
        ((command-name)
         (lambda (input-format bytevectors)
           (subprocess
            (list command-name
                  "--from" (symbol->string input-format)
                  "--to" "json")
            (call-with-port
             (open-output-bytevector)
             (lambda (out)
               (parameterize ((current-output-port out))
                 (write-all-to-tar bytevectors)
                 (get-output-bytevector out))))
            (lambda (port)
              (map json-read-from-bytevector
                   (parameterize ((current-input-port port))
                     (generator->list tar-read-file)))))))))))
