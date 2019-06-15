;;; Module wrapping access to the metadata for Gauche Scheme

;;; Commentary:

;;; Code:

(define-module sdp.common.metadata-gauche
  (use file.util)
  (use util.match)
  (use gauche.parameter)
  (use gauche.net :only (sys-gethostbyname))
  (use sxml.sxpath :prefix xpath:)
  (use srfi-13 :prefix string:)
  (use srfi-37 :prefix args:)
  (use srfi-64 :prefix test:)
  (use srfi-98 :prefix env:)
  (use sdp.common.model-gauche)
  (export +schemedoc-host-address+      ; command-line configurable parameters
          +schemedoc-port+ +schemedoc-repl+)
  (export get-metadata get-metadata-file)
  (export test))
(select-module sdp.common.metadata-gauche)

(include "prelude.scm")
(include "logging.scm")
(include "metadata.scm")

(define (%get-md-path)
  (or (env:get-environment-variable "MD_PATH") "MD_PATH_UNKNOWN"))

(define (get-metadata client-info)
  (%get-metadata (%get-md-path) client-info))

(define (get-metadata-file client-info)
  (let ((data (%get-metadata-file (%get-md-path) client-info port->string)))
    (if (and data (not (eof-object? data)))
        data
        "")))

(define (test)

  (define response-result
    ;; we don't export `response-result' just for testing, so access the private binding:
    (global-variable-ref (find-module 'sdp.common.model-gauche) 'response-result))

  (test:test-begin "test-metadata-gauche")
  (let* ((ci-gauche (make-client-info-gauche))
         (md-gauche (get-metadata ci-gauche))
         (dispatch-handler (make-dispatch-handler
                            `((documentation-index-url atom  ,(assoc-ref md-gauche 'scheme-index-url))
                              (documentation-query-url atom  ,(assoc-ref md-gauche 'scheme-query-url))))))
    (test:test-assert (string:string-contains
                       (response-result
                        (request->response ci-gauche dispatch-handler
                                           (make-request "documentation-index-url")))
                       "index.html"))
    (test:test-assert (string:string-contains (response-result
                                               (request->response ci-gauche dispatch-handler
                                                                  (make-request "documentation-query-url"
                                                                                :text-at-point "format")))
                                              "man/?p=format")))

  (let* ((ci-unknown (make-unknown-client-info))
         (md-unknown (get-metadata ci-unknown))
         (dispatch-handler (make-dispatch-handler
                            `((documentation-index-url atom  ,(assoc-ref md-unknown 'scheme-index-url))
                              (documentation-query-url atom  ,(assoc-ref md-unknown 'scheme-query-url))))))
    (test:test-assert (string:string-contains
                       (response-result
                        (request->response ci-unknown dispatch-handler
                                           (make-request "documentation-index-url")))
                       "schemexref.cgi?R7RS"))
    (test:test-assert (string:string-contains (response-result
                                               (request->response ci-unknown dispatch-handler
                                                                  (make-request "documentation-query-url"
                                                                                :text-at-point "format")))
                                              "schemexref.cgi?format")))

  (test:test-assert (string:string-contains (get-metadata-file (make-client-info-gauche)) "(title \"Gauche\")"))
  (test:test-assert (string=? (get-metadata-file (make-unknown-client-info)) ""))
  (test:test-end "test-metadata-gauche"))
