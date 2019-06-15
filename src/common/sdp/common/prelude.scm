;;; Include file defining a prelude with common, implementation-independent helpers.
;;; Commentary:

;;; Code:

(define-syntax assert
  (syntax-rules ()
    ((_ e)
     (if e
         e
         (error "Assertion failed" `e e)))
    ((_ e msg)
     (if e
         e
         (error "Assertion failed" msg `e e)))))

(define-syntax assert-pred
  (syntax-rules ()
    ((_ pred e)
     (if (pred e)
         e
         (error "Assertion failed" `pred `e e)))
    ((_ pred e msg)
     (if (pred e)
         e
         (error "Assertion failed" msg `pred `e e)))))

(define (displayln . args)
  (for-each display args)
  (newline))

(cond-expand
 (gauche
  (define (negate proc)
    ;; Return a procedure that will negate the result of given PROC.
    (compose proc not)))
 (else
  ;; Guile does already have `negate' defined, others not yet supported.
  #t))
