;;; Module wrapping access to the metadata for Guile Scheme
;;; Commentary:

;;; Code:

(define-module (sdp common metadata-guile)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)            ; get-string-all
  #:use-module ((sxml xpath) #:prefix xpath:)
  #:use-module ((srfi srfi-13) #:prefix string:)
  #:use-module ((srfi srfi-37) #:prefix args:)
  #:use-module ((srfi srfi-64) #:prefix test:)
  #:use-module ((srfi srfi-98) #:prefix env:)
  #:use-module (sdp common model-guile)
  #:export (+schemedoc-host-address+            ; command-line configurable parameters
            +schemedoc-port+ +schemedoc-repl+)
  #:export (get-metadata get-metadata-file)     ; metadata helpers
  #:export (test))                              ; test procedure

(include-from-path "sdp/common/prelude.scm")
(include-from-path "sdp/common/logging.scm")
(include-from-path "sdp/common/metadata.scm")

(define (%get-md-path)
  (or (env:get-environment-variable "MD_PATH") "MD_PATH_UNKNOWN"))

(define (get-metadata client-info)
  (%get-metadata (%get-md-path) client-info))

(define (get-metadata-file client-info)
  (let ((data (%get-metadata-file (%get-md-path) client-info get-string-all)))
    (if (and data (not (eof-object? data)))
        data
        "")))

(define (test)

  (define response-result
    ;; we don't export `response-result' just for testing, so access the private binding:
    (@@ (sdp common model-guile) response-result))

  (test:test-begin "test-metadata-guile")
  (let* ((ci-guile (make-client-info-guile))
         (md-guile (get-metadata ci-guile))
         (dispatch-handler (make-dispatch-handler
                            `((documentation-index-url atom ,(assoc-ref md-guile 'scheme-index-url))
                              (documentation-query-url atom ,(assoc-ref md-guile 'scheme-query-url))))))
    (test:test-assert (string:string-contains
                       (response-result
                        (request->response ci-guile dispatch-handler
                                           (make-request "documentation-index-url")))
                       "guile/manual/"))
    (test:test-assert (string:string-contains (response-result
                                               (request->response ci-guile dispatch-handler
                                                                  (make-request "documentation-query-url"
                                                                                #:text-at-point "format")))
                                              "schemexref.cgi?format")))

  (let* ((ci-unknown (make-unknown-client-info))
         (md-unknown (get-metadata ci-unknown))
         (dispatch-handler (make-dispatch-handler
                            `((documentation-index-url atom ,(assoc-ref md-unknown 'scheme-index-url))
                              (documentation-query-url atom ,(assoc-ref md-unknown 'scheme-query-url))))))
    (test:test-assert (string:string-contains
                       (response-result
                        (request->response ci-unknown dispatch-handler
                                           (make-request "documentation-index-url")))
                       "schemexref.cgi?R7RS"))
    (test:test-assert (string:string-contains (response-result
                                               (request->response ci-unknown dispatch-handler
                                                                  (make-request "documentation-query-url"
                                                                                #:text-at-point "format")))
                                              "schemexref.cgi?format")))

  (test:test-assert (string:string-contains (get-metadata-file (make-client-info-guile)) "(title \"Guile\")"))
  (test:test-assert (string=? (get-metadata-file (make-unknown-client-info)) ""))
  (test:test-end "test-metadata-guile"))
