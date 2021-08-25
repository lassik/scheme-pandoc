(define (join-adjacent type? type-append list)
  (let loop ((new-list '()) (list list))
    (if (null? list) (reverse new-list)
        (loop (if (and (not (null? new-list))
                       (type? (car new-list))
                       (type? (car list)))
                  (cons (type-append (car new-list)
                                     (car list))
                        (cdr new-list))
                  (cons (car list) new-list))
              (cdr list)))))

(define (join-adjacent-strings list)
  (join-adjacent string? string-append list))

(define (vector-refs vec . indexes)
  (let loop ((obj vec) (indexes indexes))
    (if (null? indexes) obj
        (loop (vector-ref obj (car indexes))
              (cdr indexes)))))

(define (pandoc-json->sxml json)

  (define (convert-block-or-inline element)
    (if (string? element) element (convert-block element)))

  (define (convert-block block)
    (let ((type (cdr (assq 't block))))
      (define (contents) (cdr (assq 'c block)))
      (define (contents-list) (vector->list (contents)))
      (cond ((equal? type "Space")
             " ")
            ((equal? type "Str")
             (contents))
            ((equal? type "BulletList")
             `(ul
               ,@(map (lambda (list-element)
                        `(li ,@(convert-many (vector->list list-element))))
                      (contents-list))))
            ((equal? type "BlockQuote")
             `(blockquote
               ,@(convert-many (contents-list))))
            ((equal? type "Code")
             `(code ,@(convert-many (cdr (contents-list)))))
            ((equal? type "CodeBlock")
             `(pre (@ (data-syntax ,(join-adjacent-strings
                                     (vector->list
                                      (vector-refs (contents) 0 1)))))
                   ,@(convert-many (cdr (contents-list)))))
            ((equal? type "Emph")
             `(em ,@(convert-many (cdr (contents-list)))))
            ((equal? type "Header")
             (let* ((level (car (contents-list)))
                    (h-tag (string->symbol
                            (string-append "h" (number->string level)))))
               `(,h-tag ,@(convert-many (vector->list
                                         (list-ref (contents-list) 2))))))
            ((equal? type "Link")
             `(a (@ (href ,(join-adjacent-strings
                            (vector->list (vector-ref (contents) 2)))))
                 ,@(convert-many (vector->list (vector-ref (contents) 1)))))
            ((equal? type "Plain")
             `(span ,@(convert-many (contents-list))))
            ((equal? type "Para")
             `(p ,@(convert-many (contents-list))))
            ((equal? type "SoftBreak")
             "\n")
            ((equal? type "Strong")
             `(strong ,@(convert-many (contents-list))))
            ((equal? type "Table")
             (let ((headings (vector-refs (contents) 3 1 0 1)))
               `(table
                 (tr
                  ,@(map (lambda (cell)
                           (let ((elements (vector->list
                                            (vector-refs cell 4))))
                             `(th ,@(map convert-block-or-inline elements))))
                         (vector->list headings)))
                 ,@(map (lambda (row)
                          `(tr ,@(map (lambda (cell)
                                        (let ((elements
                                               (vector->list
                                                (vector-refs cell 4))))
                                          `(td ,@(map convert-block-or-inline
                                                      elements))))
                                      (vector->list (vector-refs row 1)))))
                        (vector->list (vector-refs (contents) 4 0 3))))))
            (else
             (error "Unknown type in pandoc JSON" type)))))

  (define (convert-many elements)
    (join-adjacent-strings (map convert-block-or-inline elements)))
  (convert-many (vector->list (cdr (assq 'blocks json)))))

(define (pandoc-port->json input-format input-port)
  (run-read-write (append (pandoc-command-line)
                          (list "--from" (symbol->string input-format)
                                "--to" "json"))
                  input-port
                  json-read))

(define (pandoc-port->sxml input-format input-port)
  (pandoc-json->sxml (pandoc-port->json input-format input-port)))

(define (pandoc-file->json input-format input-filename)
  (call-with-binary-input-file
   input-filename
   (lambda (input-port) (pandoc-port->json input-format input-port))))

(define (pandoc-file->sxml input-format input-filename)
  (pandoc-json->sxml (pandoc-file->json input-format input-filename)))
