(define-library (pandoc)
  (export pandoc-command-line
          pandoc-port->json
          pandoc-port->sxml
          pandoc-file->json
          pandoc-file->sxml
          pandoc-json->sxml)
  (import (scheme base)
          (scheme file)
          (scheme write))
  (cond-expand
   (gauche (import (only (srfi 180) json-read)
                   (only (gauche base) copy-port)
                   (only (gauche process) call-with-process-io))))
  (begin
    (define pandoc-command-line (make-parameter (list "pandoc")))
    (define (call-with-binary-input-file filename proc)
      (call-with-port (open-binary-input-file filename) proc)))
  (cond-expand
   (gauche (include "pandoc.gauche.scm")))
  (include "pandoc.r5rs.scm"))
