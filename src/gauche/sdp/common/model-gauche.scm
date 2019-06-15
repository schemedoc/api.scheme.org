;;; Module wrapping access to the data model for Gauche Scheme

;;; Commentary:

;;; Code:

(define-module sdp.common.model-gauche
  (use util.match)
  (use gauche.parameter)
  (use srfi-9)

  (export  <client-info?> client-info-implementation-name client-info-implementation-version
           client-info-implementation-mode client-info-module-name)
  (export  <request?> request-id request-method request-text-document request-text-at-point request-params
           request-content-type request-content-encoding
           request-accept-type request-accept-encoding request-accept-documentation-format)
  (export <response?> response-http-code response-result response-error-message
          response-content-type response-content-encoding response-content-documentation-format)
  (export make-client-info-gauche make-generic-client-info make-unknown-client-info)
  (export make-request request->response make-dispatch-handler))
(select-module sdp.common.model-gauche)

(include "prelude.scm")
(include "logging.scm")
(include "model.scm")

(define (make-client-info-gauche :key (implementation-version #f) (implementation-mode #f))
  (%make-client-info "gauche" implementation-version implementation-mode))

(define (make-request method :key (text-document #f) (text-at-point #f)
                      (params '()) (trace-level #f) (accept-type #f))
  (%make-request method text-document text-at-point params trace-level accept-type))
