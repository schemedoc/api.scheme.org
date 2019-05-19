;;; Include file defining some commonly used record types and related helper procedures.
;;; Commentary:

;;; Code:

(define-record-type <client-info>
  (%%make-client-info implementation-name implementation-version implementation-mode module-name)
  <client-info?>
  (implementation-name    client-info-implementation-name)
  (implementation-version client-info-implementation-version)
  (implementation-mode    client-info-implementation-mode)
  (module-name            client-info-module-name))

(define (%make-client-info implementation-name implementation-version implementation-mode)
  (%%make-client-info (assert-pred string? implementation-name)
                      (assert-pred string? (or implementation-version "(default)"))
                      (assert-pred string? (or implementation-mode "(default)"))
                      #f))              ; module-name not yet used

(define (make-generic-client-info)
  ;; -> <client-info?>; Used whenever no specific Scheme implementation information is available.
  (%make-client-info "generic" #f #f))

(define (make-unknown-client-info)
  ;; -> <client-info?>; Used whenever an explicitly not existing Scheme implementation is to be used, e.g. for testing.
  (%make-client-info "unknown" #f #f))

(define-record-type <request>
  (%%make-request id method text-document text-at-point params trace-level
                  content-type content-encoding accept-type accept-encoding accept-documentation-format)
  <request?>
  (id                          request-id)
  (method                      request-method)
  (text-document               request-text-document)
  (text-at-point               request-text-at-point)
  (params                      request-params)
  (trace-level                 request-trace-level)
  (content-type                request-content-type)
  (content-encoding            request-content-encoding)
  (accept-type                 request-accept-type)
  (accept-encoding             request-accept-encoding)
  (accept-documentation-format request-accept-documentation-format))

(define *request-counter* 0)
(define (%make-request method text-document text-at-point params trace-level accept-type)

  (define (or/false pred)
    ;; (-> boolean? (-> any boolean?)); the returned predicate returns true iff the passed object matches the given
    ;;   predicate or its value is #f.
    (lambda (obj) (if obj (pred obj) #t)))

  (set! *request-counter* (+ *request-counter* 1))
  (let* ((content-type                "application/sexp")
         (content-encoding            "utf-8")
         (accept-type                 (cond
                                       ((and accept-type (string=? accept-type "application/sexp"))
                                        accept-type)
                                       ((and accept-type (string=? accept-type "application/json"))
                                        accept-type)
                                       ((and accept-type (string=? accept-type "text/html"))
                                        accept-type)
                                       ((and accept-type (string=? accept-type "text/plain"))
                                        accept-type)
                                       ((not accept-type)
                                        "application/sexp")
                                       (else
                                        (error "Bad accept type" accept-type))))
         (accept-encoding             "utf-8")
         (accept-documentation-format (cond
                                       ((and accept-type (string=? accept-type "application/sexp"))
                                        (list "plaintext" "markdown"))
                                       ((and accept-type (string=? accept-type "application/json"))
                                        (list "plaintext" "markdown"))
                                       ((and accept-type (string=? accept-type "text/html"))
                                        (list "plaintext"))
                                       ((and accept-type (string=? accept-type "text/plain"))
                                        (list "plaintext" "markdown"))
                                       (else
                                        (error "Bad accept type" accept-type)))))

    (%%make-request *request-counter*
                    (assert-pred string?             method)
                    (assert-pred (or/false string?)  text-document)
                    (assert-pred (or/false string?)  text-at-point)
                    (assert-pred list?               (or params '()))
                    (assert-pred (lambda (l) (member l '(off messages verbose))) (or trace-level 'verbose))
                    (assert-pred string?             content-type)
                    (assert-pred string?             content-encoding)
                    (assert-pred string?             accept-type)
                    (assert-pred string?             accept-encoding)
                    (assert-pred pair? #|non-empty|# accept-documentation-format))))

(define-record-type <response>
  (%%make-response id method text-document text-at-point result error-code error-message
                   content-type content-encoding content-documentation-format)
  <response?>
  (id                           response-id)
  (method                       response-method)
  (text-document                response-text-document)
  (text-at-point                response-text-at-point)
  (result                       response-result)
  (error-code                   response-error-code)
  (error-message                reponse-error-message)
  (content-type                 response-content-type)
  (content-encoding             response-content-encoding)
  (content-documentation-format response-content-documentation-format))

(define (%make-response request result)
  (assert-pred <request?> request)
  (%%make-response (request-id                          request)
                   (request-method                      request)
                   (request-text-document               request)
                   (request-text-at-point               request)
                   (assert-pred string?                 result)
                   #f
                   #f
                   (request-accept-type                 request)
                   (request-accept-encoding             request)
                   (request-accept-documentation-format request)))

(define (%make-error-response request error-code error-message)
  (assert-pred <request?> request)
  (%%make-response (request-id                          request)
                   (request-method                      request)
                   (request-text-document               request)
                   (request-text-at-point               request)
                   #f
                   (assert-pred number?                 error-code)
                   (assert-pred string?                 error-message)
                   (request-accept-type                 request)
                   (request-accept-encoding             request)
                   (request-accept-documentation-format request)))
(define (make-dispatch-handler handler-list)

  (define (%atom->string obj for-sexp?)
    (let ((quoter (if for-sexp?
                      (lambda (s) (string-append (string #\") s (string #\")))
                      identity)))
      (cond
       ((string? obj) (quoter obj))
       ((number? obj) (quoter (number->string obj)))
       ((symbol? obj) (quoter (symbol->string obj)))
       (else (error "Unexpected atom" obj)))))

  (define (other->string obj)           ; no quotes
    (with-output-to-string (lambda () (display obj))))

  (define (sexp->string obj)            ; with quotes
    (with-output-to-string (lambda () (write obj))))

  (define (atom->other-string obj)
    (%atom->string obj #f))

  (define (atom->sexp-string obj)
    (%atom->string obj #t))

  (define (->list ->string)             ; ((->list atom->other-string) '(1 2 3))
    (lambda (l) (map ->string l)))

  (define (->alist ->string)            ; ((->alist atom->other-string) '((a . 1) (b . 2) (b . 3)))
    (lambda (al) (map (lambda (p) (cons (->string (car p)) (->string (cdr p)))) al)))

  (define (->alists ->string)           ; ((->alists atom->other-string) '(((a . 1) (b . 2) (b . 3))))
    (lambda (als) (map (lambda (al) ((->alist ->string) al)) als)))

  (define (make-result-formatter request result-type)
    (let* (;; for now we simply pick the first accepted document format, no negotiation:
           (df (car (request-accept-documentation-format request)))
           (text-proc (cond
                       ;; TODO: selecting markdown or plaintext is not orthogonal to the accept-type as markdown won't
                       ;;   make much sense for e.g. HTML, so check this when already creating the request or here?
                       ;; TODO: implement markdown formatting, then use `text-proc'.
                       ((string=? df "plaintext") identity)
                       ((string=? df "markdown")  identity)
                       (else (error "Bad documentation format" df))))
           (at (request-accept-type request)))
      (cond
       ((string=? at "application/sexp")
        (case result-type
          ((atom)   atom->sexp-string)
          ((list)   (->list atom->sexp-string))
          ((alist)  (->alist atom->sexp-string))
          ((alists) (->alists atom->sexp-string))
          (else (error "Bad result type" result-type))))
       ((string=? at "application/json")
        (case result-type
          ((atom)   atom->other-string)
          ((list)   (->list atom->other-string))
          ((alist)  (->alist atom->other-string))
          ((alists) (->alists atom->other-string))
          (else (error "Bad result type" result-type))))
       ((string=? at "text/html")
        (case result-type
          ((atom)   atom->other-string)
          ((list)   (->list atom->other-string))
          ((alist)  (->alist atom->other-string))
          ((alists) (->alists atom->other-string))
          (else (error "Bad result type" result-type))))
       ((string=? at "text/plain")
        (case result-type
          ((atom)   atom->other-string)
          ((list)   (->list atom->other-string))
          ((alist)  (->alist atom->other-string))
          ((alists) (->alists atom->other-string))
          (else (error "Bad result type" result-type))))
       (else (error "Bad accept type" at)))))

  (lambda (request key)
    ;; 1st level of dispatch: find handler procedure for given `key':
    (let loop ((handler-search-list handler-list))
      (cond
       ((null? handler-search-list)
        (error "Cannot find handler" key (map car handler-list)))
       ((eq? (car (car handler-search-list)) key)
        (let* ((handler (assert-pred list? (car handler-search-list)))
               (result-type (assert-pred symbol? (cadr handler)))
               (handler-proc (assert-pred procedure? (caddr handler)))
               ;; 2nd level of dispatch: make formatting procedure for request's accept parameters and `result-type':
               (result-formatter (assert-pred procedure? (make-result-formatter request result-type))))
          (lambda args
            ;; return a handler procedure, supporting various argument list formats, which will also format the result:
            (let* ((result (apply handler-proc args))
                   (formatted (assert-pred string? (result-formatter result))))
              formatted))))
       (else
        (loop (cdr handler-search-list)))))))

(define *request-method-unknown*  1000)
(define *no-text-at-point*        1001)

(define (request->response client-info dispatch-handler request)
  (assert-pred <client-info?> client-info)
  (assert-pred procedure? dispatch-handler)
  (assert-pred <request?> request)
  (assert-pred <response?>
               (let ((method (request-method request)))
                 (cond
                  ((string=? method "documentation-index-url")
                   (%make-response request ((dispatch-handler request 'documentation-index-url) client-info)))
                  ((string=? method "documentation-query-url")
                   (let ((tap (request-text-at-point request)))
                     (if tap
                         (%make-response request ((dispatch-handler request 'documentation-query-url) client-info tap))
                         (%make-error-response request *no-text-at-point* "No text at point"))))
                  ((string=? method "built-in-describe-object")
                   (let ((tap (request-text-at-point request)))
                     (if tap
                         (%make-response request ((dispatch-handler request 'built-in-describe-object) client-info tap))
                         (%make-error-response request *no-text-at-point* "No text at point"))))
                  ((string=? method "built-in-apropos-fragment")
                   (let ((tap (request-text-at-point request)))
                     (if tap
                         (%make-response request ((dispatch-handler request 'built-in-apropos-fragment) client-info tap))
                         (%make-error-response request *no-text-at-point* "No text at point"))))
                  (else
                   (%make-error-response request *request-method-unknown* (string-append "Request method unknown: " method)))))))
