(module (pandoc cli)

  (#;export
   pandoc-cli)

  (import (scheme)
          (chicken base)
          (only (medea) read-json)
          (only (scheme base) utf8->string)
          (only (scsh-process) run/port))

  (define (pandoc-cli #!optional command-name)
    (let ((command-name (or command-name "pandoc")))
      (lambda (input-format bytevectors)
        (map (lambda (bytevector)
               (read-json
                (run/port (,command-name --from ,input-format --to json)
                          (<< ,(utf8->string bytevector)))))
             bytevectors)))))
