(define-module (sdp client client)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  ;; (web request) (web http) ; also used by various client
  ;; (system repl server) ; seems to be module of repl server
  ;;    /home/frank/localsrc/guile-2.9.1/module/system/repl/server.scm
  ;;    -> /home/frank/localsrc/guile-2.9.1/module/system/repl/command.scm -> *command-table*
  ;;       -> /home/frank/localsrc/guile-2.9.1/module/ice-9/documentation.scm -> object-documentation
  ;;                                                                          -> search-documentation-files
  ;;    related: (system repl error-handling)
  ;;    related ???: Use repl defined in ice-9 boot : (repl reader evaler printer)
  #:use-module (sdp common metadata-guile)
  #:export (main))

(receive (response body)
    ;; http://localhost:8080/api/query/guile/define
    (http-get (build-uri 'http
                         #:host "localhost" #:port 8080 #:path "/api/query/guile/define")
              #:headers `((Accept . "text/plain"))
              #:keep-alive? #t)
  (display (list response body (response-code response) (response-reason-phrase response)))
  (case (response-code response)
    ((200)
     (display (list 'OK (response-content-length response))))
    ((301 302 303 307 308)
     (display (list 'redirect
                    (response-content-length response)
                    (response-location response) ; #f; probably be set for redirect, e.g. 301, 302, ...
                    )))
    (else (display '?????))))

;; (define (http-get->str url port path)
;;   (let ((uri (build-uri 'http #:host url #:port port #:path path)))
;;     (call-with-values
;; 	(lambda () (http-get uri  #:keep-alive? #f))
;;       (lambda (request body) body))))
;; (display (http-get->str "www.foo.com" 8080 "/api/status"))

(define (main)
  (display 'main))
