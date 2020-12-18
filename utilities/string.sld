(define-library (utilities string)
  (export string-blank?)
  (import (scheme base)
          (scheme char))
  (begin

    (define (string-blank? s)
      (let loop ((i 0))
        (or (= i (string-length s))
            (and (char-whitespace? (string-ref s i))
                 (loop (+ i 1))))))))
