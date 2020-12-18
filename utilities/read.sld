(define-library (utilities read)
  (export read-all)
  (import (scheme base)
          (scheme read))
  (begin

    (define (read-all)
      (let loop ((forms '()))
        (let ((form (read)))
          (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms))))))))
