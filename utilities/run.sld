(define-library (utilities run)
  (export run
          run-in-directory
          run/output-bytevector
          run/output-string
          run/output-line)
  (import (scheme base)
          (srfi 8)
          (srfi 13)
          (utilities read))
  (cond-expand
    (chicken (import (chicken file posix)
                     (chicken process)
                     (chicken process-context))))
  (begin

    (define (launch thunk) (process-fork thunk #t))

    (define (wait-for child)
      (receive (child normal-exit? status) (process-wait child)
        (unless (and normal-exit? (= 0 status))
          (error "Child process failed"))))

    (define (launch+wait thunk) (wait-for (launch thunk)))

    (define (run name . args)
      (wait-for (launch (lambda ()
                          (process-execute name args)))))

    (define (run-in-directory directory name . args)
      (wait-for (launch (lambda ()
                          (change-directory directory)
                          (process-execute name args)))))

    (define (run/output-bytevector name . args)
      (receive (fileno/read fileno/write) (create-pipe)
        (let ((child (launch (lambda ()
                               (duplicate-fileno fileno/write fileno/stdout)
                               (file-close fileno/read)
                               (file-close fileno/write)
                               (process-execute name args)))))
          (file-close fileno/write)
          (let ((bytes (call-with-port (open-input-file* fileno/read)
                                       read-bytevector-all)))
            (wait-for child)
            bytes))))

    (define (run/output-string name . args)
      (utf8->string (apply run/output-bytevector name args)))

    (define (run/output-line name . args)
      (let* ((output (apply run/output-string name args))
             (end-of-line (string-index output #\newline)))
        (cond ((not end-of-line)
               (error "No newline in output"))
              ((= end-of-line (- (string-length output) 1))
               (substring output 0 end-of-line))
              (else
               (error "More than one line in output")))))))
