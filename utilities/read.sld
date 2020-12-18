(define-library (utilities read)
  (export read-all)
  (import (scheme base)
          (scheme read))
  (begin

    (define (read-all)
      (let loop ((whole '()))
        (let ((part (read)))
          (if (eof-object? part) (reverse whole)
              (loop (cons part whole))))))

    (define (read-bytevector-all port)
      (let loop ((whole (bytevector)))
        (let ((part (read-bytevector 4096 port)))
          (if (eof-object? part) whole
              (loop (bytevector-append whole part))))))))
