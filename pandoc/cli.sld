(define-library (pandoc cli)
  (export pandoc-cli)
  (import (scheme base) (scheme case-lambda)
          (pandoc internal json)
          (pandoc internal subprocess))
  (begin
    (define pandoc-cli
      (case-lambda
        (()
         (pandoc-cli "pandoc"))
        ((command-name)
         (lambda (input-format bytevectors)
           (map (lambda (bytevector)
                  (subprocess (list command-name
                                    "--from" (symbol->string input-format)
                                    "--to" "json")
                              bytevector
                              json-read))
                bytevectors)))))))
