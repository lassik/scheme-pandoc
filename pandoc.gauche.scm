(define (run-read-write args input-port read-output)
  (call-with-process-io
   args
   (lambda (from-sub to-sub)
     (copy-port input-port to-sub)
     (close-output-port to-sub)
     (read-output from-sub))))
