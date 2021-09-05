(module pandoc

  (#;export

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

  (import (scheme)
          (chicken base)
          (only (scheme base)
                bytevector
                bytevector-append
                read-bytevector
                string->utf8)
          (only (chicken io) read-byte write-byte)
          (only (chicken port) copy-port with-input-from-string)
          (only (chicken process) process process-wait)
          (only (scsh-process) run/port)
          (only (medea) read-json))

  (define (read-bytevector-all port)
    (let loop ((whole (bytevector)))
      (let ((part (read-bytevector 1000 port)))
        (if (eof-object? part) whole
            (loop (bytevector-append whole part))))))

  (define (call-with-binary-input-file filename proc)
    (let ((port (open-input-file filename #:binary)))
      (dynamic-wind (lambda () #f)
                    (lambda () (proc port))
                    (lambda () (close-input-port port)))))

  (include "pandoc.r5rs.scm"))
