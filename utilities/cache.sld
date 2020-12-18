(define-library (utilities cache)
  (export cache-directory
          cache-pathname)
  (import (scheme base)
          (scheme process-context))
  (begin

    (define cache-directory
      (make-parameter
       (make-absolute-pathname (get-environment-string "CACHE") #f)))

    (define (cache-pathname symbol)
      (make-pathname (cache-directory) (symbol->string symbol)))))
