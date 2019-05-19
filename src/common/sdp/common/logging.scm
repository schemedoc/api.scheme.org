;;; Include file defining support for some implementation-independent logging helpers.
;;; Commentary:

;;; Code:

(define %%log-off   0)
(define %%log-warn  300)
(define %%log-info  400)
(define %%log-debug 500)
(define %%log-all   999)

(define +%%debug-level+
  (make-parameter %%log-all
                  (lambda (v)
                    (let ((n (if (number? v) v (string->number v))))
                      (if (positive? n) n %%log-off)))))

(define (%%log out level prefix . messages)
  (when (>= (+%%debug-level+) level)
    (format out "~a: ~a~%" prefix messages))
  (if #f #f #;return-unspecified))

(define (debug      . messages) (apply %%log (current-output-port) %%log-debug "debug" messages))
(define (info       . messages) (apply %%log (current-output-port) %%log-info  "info " messages))
(define (warn       . messages) (apply %%log (current-error-port)  %%log-warn  "warn " messages))
(define (error-exit . messages) (apply %%log (current-error-port)  %%log-off   "error" messages) (exit 1))
