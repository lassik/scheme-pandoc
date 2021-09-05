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
  (cond-expand
    (gauche (import (only (srfi 180) json-read)
                    (only (gauche base) copy-port)
                    (only (gauche process) call-with-process-io))))
  (cond-expand
    (gauche (include "pandoc.gauche.scm")))
  (begin (define inexact->exact exact)

         (define (call-with-binary-input-file filename proc)
           (call-with-port (open-binary-input-file filename) proc))

         (define (read-bytevector-all binary-input-port)
           (let loop ((whole (bytevector)))
             (let ((part (read-bytevector 1000)))
               (if (eof-object? part) whole
                   (loop (bytevector-append whole part)))))))
  (include "pandoc.r5rs.scm"))
