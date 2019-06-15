;;; Module wrapping access to the REPL helpers supporting reading built-in documentation for Gauche Scheme

;;; Commentary:
;;   Below we define some of the handler procedures dispatched by the handler created with `make-dispatch-handler'.
;;   These handler procedures will usually be called with an argument list, where the first element is the client info
;;   and - where required - the second element is the text-at-point.

;;; Code:

(define-module sdp.common.repl-gauche
  (use util.match)
  (use gauche.parameter)
  (use gauche.modutil :prefix modutil:)
  (use gauche.interactive :prefix repl:)
  (use srfi-13 :prefix string:)
  (use srfi-64 :prefix test:)
  (use sdp.common.model-gauche)
  (export built-in-describe-object built-in-apropos-fragment)
  (export test))
(select-module sdp.common.repl-gauche)

(include "prelude.scm")
(include "logging.scm")

(define (built-in-describe-object client-info text-at-point)
  ;; {gauche-root}/lib/gauche/interactive.scm -> define-method describe -> describe-symbol-bindings
  ;;   -> {gauche-root}/lib/gauche/modutil.scm
  ;;      -> (use gauche.modutil) (describe-symbol-bindings 'format)
  ;; Note: also supports module argument; to find module by symbol, use: (find-module module)
  (with-output-to-string
    (lambda () (modutil:describe-symbol-bindings (string->symbol text-at-point)))))

(define (built-in-apropos-fragment client-info text-at-point)
  ;; {gauche-root}/lib/gauche/interactive.scm -> (%apropos item module stay-in-module)
  (with-output-to-string
    (lambda () (repl:apropos (string->symbol text-at-point)))))

(define (test)
  (test:test-begin "test-repl-gauche")
  (test:test-assert (string:string-contains (built-in-describe-object (make-client-info-gauche) "format")
                                             "#<closure (format . args)>"))
  (test:test-assert (string:string-contains (built-in-apropos-fragment (make-client-info-gauche) "open")
                                            "%open-input-file/conv"))
  (test:test-end "test-repl-gauche"))
