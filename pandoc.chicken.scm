(module pandoc

  (pandoc-command-line
   pandoc-port->json
   pandoc-port->sxml
   pandoc-file->json
   pandoc-file->sxml)

  (import (scheme)
          (chicken base)
          (only (chicken io) read-byte write-byte)
          (only (chicken port) copy-port)
          (only (chicken process) process process-wait)
          (only (scsh-process) run/port)
          (only (medea) read-json))

  (define (run-read-write/old args input-port read-output)
    (receive (from-sub to-sub sub) (process (car args) (cdr args))
      (copy-port input-port to-sub read-byte write-byte)
      (close-output-port to-sub)
      (let ((output (read-output from-sub)))
        (receive (sub clean-exit? exit-status) (process-wait sub)
          ;; Call `process-wait` before closing the last port to avoid
          ;; triggering the automatic `process-wait` done by `process`
          ;; when all ports are closed. If we relied on the implicit
          ;; `process-wait`, we couldn't find out the exit status.
          (close-input-port from-sub)
          (if (and clean-exit? (eqv? 0 exit-status)) output
              (error "Error running" args))))))

  (define (pandoc-port->json input-format input-port)
    (let ((pandoc (string->symbol (car (pandoc-command-line)))))
      (read-json (run/port (,pandoc --from ,input-format --to json)
                           (= 0 input-port)))))

  (define (call-with-binary-input-file filename proc)
    (let ((port (open-input-file filename #:binary)))
      (dynamic-wind (lambda () #f)
                    (lambda () (proc port))
                    (lambda () (close-input-port port)))))

  (define pandoc-command-line (make-parameter (list "pandoc")))

  (include "pandoc.r5rs.scm"))
