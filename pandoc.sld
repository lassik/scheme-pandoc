(define-library (pandoc)
  (export

   pandoc-json->sxml

   pandoc-port->json
   pandoc-port->sxml

   pandoc-file->json
   pandoc-file->sxml

   pandoc-files->json
   pandoc-files->sxml

   pandoc-bytevector->json
   pandoc-bytevector->sxml

   pandoc-bytevectors->json
   pandoc-bytevectors->sxml

   pandoc-string->json
   pandoc-string->sxml

   pandoc-strings->json
   pandoc-strings->sxml)
  (import (scheme base)
          (scheme file)
          (scheme write))
  (begin (define inexact->exact exact)

         (define (read-bytevector-all port)
           (let loop ((whole (bytevector)))
             (let ((part (read-bytevector 1000 port)))
               (if (eof-object? part) whole
                   (loop (bytevector-append whole part)))))))
  (include "pandoc/pandoc.r5rs.scm"))
