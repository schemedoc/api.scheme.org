;;; Module wrapping access to the REPL helpers supporting reading built-in documentation for Guile Scheme

;;; Commentary:
;;   Below we define some of the handler procedures dispatched by the handler created with `make-dispatch-handler'.
;;   These handler procedures will usually be called with an argument list, where the first element is the client info
;;   and - where required - the second element is the text-at-point.

;;; Code:

(define-module (sdp common repl-guile)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 documentation)
  #:use-module ((srfi srfi-13) #:prefix string:)
  #:use-module ((srfi srfi-64) #:prefix test:)
  #:use-module (sdp common model-guile)
  #:export (built-in-describe-object built-in-apropos-fragment)
  #:export (test))

(include-from-path "sdp/common/prelude.scm")
(include-from-path "sdp/common/logging.scm")

(define (built-in-describe-object client-info text-at-point)
  ;; (system repl server) ; main module of repl server. That one is not required here, but it's calling:
  ;;   -> {guile-root}/module/system/repl/server.scm
  ;;      -> {guile-root}/module/system/repl/command.scm -> *command-table*
  ;;         -> {guile-root}/module/ice-9/documentation.scm -> object-documentation, search-documentation-files
  ;; Note: Object documentation is only available for a few symbols, otherwise returns #f.
  ;; Note: `object-documentation' also uses `search-documentation-files', but only in the branch that is initially
  ;;   eval'ing the symbol-at-point, so we won't find all results with the code below as we do with the `,describe'
  ;;   command in the REPL - as long as we won't allow `eval' here. A compromise might be to allow eval when running in
  ;;   the local API middleware but not when running the internet API server. It's a TODO: to support that.
  (object-documentation
   (module-ref (current-module) (string->symbol text-at-point))))

(define (built-in-apropos-fragment client-info text-at-point)
  ;; Searches documentation in list of files defined by `documentation-files'.
  (search-documentation-files (string->symbol text-at-point)))

(define (test)
  (test:test-begin "test-repl-guile")
  (test:test-assert (string:string-contains (built-in-describe-object (make-client-info-guile) "or-map")
                                            "Apply F to successive elements of LST"))
  (test:test-assert (string:string-contains (built-in-apropos-fragment (make-client-info-guile) "bind")
                                            "Scheme Procedure: bind"))
  (test:test-end "test-repl-guile"))
