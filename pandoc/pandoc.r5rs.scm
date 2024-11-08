(define (join-adjacent-strings list)
  (let loop ((new-list '()) (list list))
    (if (null? list) (reverse new-list)
        (loop (if (and (not (null? new-list))
                       (string? (car new-list))
                       (string? (car list)))
                  (cons (string-append (car new-list)
                                       (car list))
                        (cdr new-list))
                  (cons (car list) new-list))
              (cdr list)))))

(define (join-strings-into-one-string list)
  (let loop ((result "") (tail list))
    (cond ((null? tail) result)
          ((string? (car tail)) (loop (string-append result (car tail))
                                      (cdr tail)))
          (else (error "Expected all strings" list)))))

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
             `(pre (@ (data-syntax ,(join-strings-into-one-string
                                     (vector->list
                                      (vector-refs (contents) 0 1)))))
                   ,@(convert-many (cdr (contents-list)))))
            ((equal? type "Emph")
             `(em ,@(convert-many (contents-list))))
            ((equal? type "Header")
             (let* ((level (car (contents-list)))
                    (h-tag (string->symbol
                            (string-append
                             "h" (number->string (inexact->exact level))))))
               `(,h-tag ,@(convert-many (vector->list
                                         (list-ref (contents-list) 2))))))
            ((equal? type "Image")
             `(img (@ (src
                       ,(join-strings-into-one-string
                         (vector->list (vector-ref (contents) 2))))
                      #;
                      (alt
                      ,(join-strings-into-one-string
                      (vector->list (vector-ref (contents) 1)))))))
            ((equal? type "Link")
             `(a (@ (href ,(join-strings-into-one-string
                            (vector->list (vector-ref (contents) 2)))))
                 ,@(convert-many (vector->list (vector-ref (contents) 1)))))
            ((equal? type "Plain")
             `(span ,@(convert-many (contents-list))))
            ((equal? type "Para")
             `(p ,@(convert-many (contents-list))))
            ((equal? type "Quoted") ; TODO: What's this?
             `(blockquote
               ,@(convert-many '())))
            ((equal? type "SingleQuote") ; TODO: What's this?
             `(blockquote
               ,@(convert-many '())))
            ((equal? type "DoubleQuote") ; TODO: What's this?
             `(blockquote
               ,@(convert-many '())))
            ((equal? type "OrderedList") ; TODO: What's this?
             `(blockquote
               ,@(convert-many '())))
            ((equal? type "Superscript") ; TODO: What's this?
             `(blockquote
               ,@(convert-many '())))
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

  (define (assert-supported-version)
    (let* ((version (cdr (assq 'pandoc-api-version json)))
           (major (vector-ref version 0)))
      (unless (= major 1)
        (error "Pandoc API version is not 1.x" version))))

  (assert-supported-version)
  (convert-many (vector->list (cdr (assq 'blocks json)))))

;;

(define (pandoc-bytevectors->json pandoc input-format bytevectors)
  (pandoc input-format bytevectors))

(define (pandoc-strings->json pandoc input-format strings)
  (pandoc-bytevectors->json
   pandoc input-format
   (map string->utf8 strings)))

(define (pandoc-files->json pandoc input-format filenames)
  (pandoc-bytevectors->json
   pandoc input-format
   (map (lambda (filename)
          (call-with-port (open-binary-input-file filename)
                          read-bytevector-all))
        filenames)))

;;

(define (pandoc-bytevector->json pandoc input-format bytevector)
  (car (pandoc-bytevectors->json pandoc input-format (list bytevector))))

(define (pandoc-string->json pandoc input-format string)
  (car (pandoc-strings->json pandoc input-format (list string))))

(define (pandoc-file->json pandoc input-format filename)
  (car (pandoc-files->json pandoc input-format (list filename))))

;;

(define (pandoc-bytevectors->sxml pandoc input-format bytevectors)
  (map pandoc-json->sxml
       (pandoc-bytevectors->json pandoc input-format bytevectors)))

(define (pandoc-strings->sxml pandoc input-format strings)
  (map pandoc-json->sxml
       (pandoc-strings->json pandoc input-format strings)))

(define (pandoc-files->sxml pandoc input-format filenames)
  (map pandoc-json->sxml
       (pandoc-files->json pandoc input-format filenames)))

;;

(define (pandoc-bytevector->sxml pandoc input-format bytevector)
  (car (pandoc-bytevectors->sxml pandoc input-format (list bytevector))))

(define (pandoc-string->sxml pandoc input-format string)
  (car (pandoc-strings->sxml pandoc input-format (list string))))

(define (pandoc-file->sxml pandoc input-format filename)
  (car (pandoc-files->sxml pandoc input-format (list filename))))

;;

(define (pandoc-port->json pandoc input-format port)
  (pandoc-bytevector->json pandoc input-format (read-bytevector-all port)))

(define (pandoc-port->sxml pandoc input-format port)
  (pandoc-bytevector->sxml pandoc input-format (read-bytevector-all port)))
