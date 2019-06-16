;;; Module defining an API client fetching information from the API server for Guile Scheme

;;; Commentary:
;;   This module is not meant to provide some practically useful client CLI, but it implements a procedure `test' which
;;   allows more detailed tests for the API server responses than would be possible with `curl' and shell response
;;   parsing.

;;; Code:

(define-module (sdp client client)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module ((srfi srfi-13) #:prefix string:)
  #:use-module ((srfi srfi-64) #:prefix test:)
  #:use-module (sdp common metadata-guile)
  #:export (test))

(define-syntax with-response
  (syntax-rules (accept-header:)
    ((_ uri (response payload) body ...)
     (with-response uri accept-header: "text/plain" (response payload) body ...))
    ((_ uri accept-header: accept-header (response payload) body ...)
     (receive (response payload)
         (http-get uri #:headers `((Accept . accept-header))
                   #:keep-alive? #f)
       body ...))))

(define (test)

  (define (test-code+text response body-str exp-code exp-str)
    (display (list '>>>>>>>>>>>>>>> response body-str)) (newline)
    (if (= (response-code response) exp-code)
        (test:test-assert (string:string-contains body-str exp-str))
        (error "Unexpected HTTP code" (response-code response))))

  (test:test-begin "test-client-guile")
  (with-response
   (build-uri 'http #:host "localhost" #:port 8080 #:path "/api/query/guile/define")
   (response body)
   (test-code+text response body 200 "https://practical-scheme.net/wiliki/schemexref.cgi?define"))
  (with-response
   (build-uri 'http #:host "localhost" #:port 8080 #:path "/api/query/guile/define") accept-header: "text/plain"
   (response body)
   (test-code+text response body 200 "https://practical-scheme.net/wiliki/schemexref.cgi?define"))
  (with-response
   (build-uri 'http #:host "localhost" #:port 8080 #:path "/api/query/guile/define") accept-header: "text/html"
   (response body)
   (test-code+text response body 200 "https://practical-scheme.net/wiliki/schemexref.cgi?define"))
  (with-response
   (build-uri 'http #:host "localhost" #:port 8080 #:path "/api/query/guile/define") accept-header: "application/sexp"
   (response body)
   (test-code+text response body 200 "https://practical-scheme.net/wiliki/schemexref.cgi?define"))
  (with-response
   (build-uri 'http #:host "localhost" #:port 8080 #:path "/api/query/guile/define") accept-header: "application/json"
   (response body)
   (test-code+text response body 200 "https://practical-scheme.net/wiliki/schemexref.cgi?define"))
  (test:test-end "test-client-guile"))
