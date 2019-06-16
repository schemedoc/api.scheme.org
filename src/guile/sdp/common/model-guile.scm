;;; Module wrapping access to the data model for Guile Scheme

;;; Commentary:

;;; Code:

(define-module (sdp common model-guile)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (sdp common html-guile)
  #:export (<client-info?> client-info-implementation-name client-info-implementation-version
                           client-info-implementation-mode client-info-module-name)
  #:export (<request?> request-id request-method request-text-document request-text-at-point request-params
                       request-content-type request-content-encoding
                       request-accept-type request-accept-encoding request-accept-documentation-format)
  #:export (<response?> response-http-code response-result response-error-message
                        response-content-type response-content-encoding response-content-documentation-format)
  #:export (make-client-info-guile make-generic-client-info make-unknown-client-info)
  #:export (make-request request->response make-dispatch-handler))

(include-from-path "sdp/common/prelude.scm")
(include-from-path "sdp/common/logging.scm")
(include-from-path "sdp/common/model.scm")

(define (make-dispatch-handler handler-list)
  (%make-dispatch-handler handler-list sxml->html-string))

(define* (make-client-info-guile #:key (implementation-version #f) (implementation-mode #f))
  (%make-client-info "guile" implementation-version implementation-mode))

(define* (make-request method #:key
                       (text-document #f) (text-at-point #f) (params '()) (trace-level #f) (accept-type #f))
  (%make-request method text-document text-at-point params trace-level accept-type))
