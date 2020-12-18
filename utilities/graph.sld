(define-library (utilities graph)
  (export define-source-value
          define-value)
  (import (scheme base))
  (begin

    (define-record-type <source-value>
      (make-source-value symbol schedule update)
      source-value?
      (symbol   source-value-symbol)
      (schedule source-value-schedule)
      (update   source-value-update))

    (define-record-type <derived-value>
      (make-derived-value symbol depends update)
      derived-value?
      (symbol   derived-value-symbol)
      (schedule derived-value-depends)
      (update   derived-value-update))

    (define graph '())

    (define (graph-add! value)
      (set! graph (cons value graph)))

    (define-syntax define-source-value
      (syntax-rules ()
        ((_ symbol schedule update-expr)
         (graph-add! (make-source-value
                      'symbol
                      'schedule
                      (lambda () update-expr))))))

    (define-syntax define-value
      (syntax-rules ()
        ((_ symbol (depends ...) update-expr)
         (graph-add! (make-derived-value
                      'symbol
                      '(depends ...)
                      (lambda () update-expr))))))))
