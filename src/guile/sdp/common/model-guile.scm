;;; Module wrapping access to the data model for Guile Scheme
;;; Commentary:

;;; Code:

(define-module (sdp common model-guile)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (<client-info?> make-generic-client-info make-unknown-client-info client-info-implementation-name)
  #:export (make-request request->response make-dispatch-handler)
  #:export (make-client-info-guile))

(include-from-path "sdp/common/prelude.scm")
(include-from-path "sdp/common/logging.scm")
(include-from-path "sdp/common/model.scm")

(define* (make-client-info-guile #:key (implementation-version #f) (implementation-mode #f))
  (%make-client-info "guile" implementation-version implementation-mode))

(define* (make-request method #:key
                       (text-document #f) (text-at-point #f) (params '()) (trace-level #f) (accept-type #f))
  (%make-request method text-document text-at-point params trace-level accept-type))
