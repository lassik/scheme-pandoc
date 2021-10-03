(define-library (pandoc internal subprocess)
  (export subprocess)
  (import (scheme base))
  (cond-expand
    (chicken
     (import (only (scsh-process) run/port))
     (begin
       (define (subprocess command-line stdin-bytevector read-stdout)
         (read-stdout (run/port (,(car command-line) ,@(cdr command-line))
                                (<< ,(utf8->string stdin-bytevector))
                                (> 2 "/dev/null"))))))
    (gauche
     (import (only (gauche process) call-with-process-io))
     (begin
       (define (subprocess command-line stdin-bytevector read-stdout)
         (call-with-process-io
          command-line
          (lambda (from-sub to-sub)
            (write-bytevector stdin-bytevector to-sub)
            (close-output-port to-sub)
            (read-stdout from-sub))))))))
