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
