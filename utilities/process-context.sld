(define-library (utilities process-context)
  (export get-environment-string
          get-environment-natural)
  (import (scheme base)
          (scheme process-context)
          (utilities string))
  (begin

    (define (get-environment-string name)
      (let ((value (or (get-environment-variable name) "")))
        (if (not (string-blank? value)) value
            (error "Environment variable not set:" name))))

    (define (get-environment-natural name)
      (let ((n (string->number (get-environment-string name))))
        (if (and (integer? n) (exact-integer? n) (>= n 0)) n
            (error "Environment variable not a number:" name n))))))
