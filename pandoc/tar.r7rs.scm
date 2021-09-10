(define (bytevector-every? predicate bytes)
  (let loop ((i 0))
    (or (= i (bytevector-length bytes))
        (and (predicate (bytevector-u8-ref bytes i))
             (loop (+ i 1))))))

(define (tar-read-file)

  (define (read-exactly-n-bytes n)
    (let ((bytes (read-bytevector n)))
      (if (or (eof-object? bytes) (< (bytevector-length bytes) n))
          (error "Short read")
          bytes)))

  (let ((header (read-bytevector 512)))

    (define (tar-octal-ref offset len)
      (let loop ((offset offset) (len len) (value 0))
        (if (<= len 0) value
            (let ((dig0 (char->integer #\0))
                  (dig7 (char->integer #\7))
                  (byte (bytevector-u8-ref header offset)))
              (loop (+ offset 1) (- len 1)
                    (if (<= dig0 byte dig7)
                        (let ((digit (- byte dig0)))
                          (+ digit (* value 8)))
                        value))))))

    (cond ((eof-object? header)
           (eof-object))
          ((bytevector-every? zero? header)
           (eof-object))
          (else
           (unless (= 512 (bytevector-length header))
             (error "Short read"))
           (let* ((nbyte (tar-octal-ref 124 12))
                  (nnull (- 512 (truncate-remainder nbyte 512)))
                  (bytes (read-exactly-n-bytes nbyte)))
             (read-exactly-n-bytes nnull)
             bytes)))))

(define (tar-write-file filename bytes)

  (define nulls (make-bytevector 512 0))
  (define blank-checksum (make-bytevector 7 (char->integer #\space)))

  (define (bytevector-sum bv)
    (let loop ((i (- (bytevector-length bv) 1)) (sum 0))
      (if (< i 0) sum (loop (- i 1) (+ sum (bytevector-u8-ref bv i))))))

  (define (tar-string nbyte string)
    (let* ((bytes (string->utf8 string))
           (nnull (- nbyte (bytevector-length bytes))))
      (when (< nnull 1) (error "tar: string too long"))
      (bytevector-append bytes (make-bytevector nnull 0))))

  (define (tar-octal nbyte number)
    (let* ((bytes (string->utf8 (number->string number 8)))
           (nzero (- nbyte 1 (bytevector-length bytes))))
      (bytevector-append (make-bytevector nzero (char->integer #\0))
                         bytes (bytevector 0))))

  (let* ((nbyte (bytevector-length bytes))
         (nnull (- 512 (truncate-remainder nbyte 512)))
         (header-before-checksum
          (bytevector-append
           (tar-string 100 filename)
           (tar-octal 8 #o444)
           (tar-octal 8 0)
           (tar-octal 8 0)
           (tar-octal 12 nbyte)
           (tar-octal 12 0)))
         (header-after-checksum
          (bytevector-append
           (bytevector (char->integer #\space))
           (bytevector (char->integer #\0))
           (tar-string 100 "")
           (tar-string 8 "ustar  ")
           (tar-string 32 "")
           (tar-string 32 "")
           (make-bytevector 183 0)))
         (checksum
          (let ((sum (+ (bytevector-sum header-before-checksum)
                        (bytevector-sum blank-checksum)
                        (bytevector-sum header-after-checksum))))
            (tar-octal 7 (truncate-remainder sum (expt 8 6))))))
    (write-bytevector header-before-checksum)
    (write-bytevector checksum)
    (write-bytevector header-after-checksum)
    (write-bytevector bytes)
    (write-bytevector nulls (current-output-port) 0 nnull)))

(define (write-all-to-tar inputs)
  (let loop ((i 0) (inputs inputs))
    (unless (null? inputs)
      (let ((filename (string-append (number->string i) ".md")))
        (tar-write-file filename (car inputs))
        (loop (+ i 1) (cdr inputs))))))
