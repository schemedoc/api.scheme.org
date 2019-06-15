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
    ;;   predicate or if its value is #f.
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
  (%%make-response id method text-document text-at-point http-code result error-message
                   content-type content-encoding content-documentation-format)
  <response?>
  (id                           response-id)
  (method                       response-method)
  (text-document                response-text-document)
  (text-at-point                response-text-at-point)
  (http-code                    response-http-code)
  (result                       response-result)
  (error-message                response-error-message)
  (content-type                 response-content-type)
  (content-encoding             response-content-encoding)
  (content-documentation-format response-content-documentation-format))

(define (%make-response request http-code result)
  (assert-pred <request?> request)
  (%%make-response (request-id                          request)
                   (request-method                      request)
                   (request-text-document               request)
                   (request-text-at-point               request)
                   (assert-pred number?                 http-code)
                   ;; `result' is not necessarily a string, might also be an s-exp; it just cannot be #f:
                   (assert-pred identity                result)
                   #f                                   ; error-message
                   (assert-pred string?                 (request-accept-type request))
                   (assert-pred string?                 (request-accept-encoding request))
                   (assert-pred pair?                   (request-accept-documentation-format request))))

(define (%make-error-response request http-error-code error-message)
  (assert-pred <request?> request)
  (%%make-response (request-id                          request)
                   (request-method                      request)
                   (request-text-document               request)
                   (request-text-at-point               request)
                   (assert-pred number?                 http-error-code)
                   #f                                   ; result
                   ;; `error-message' is not necessarily a string, might also be an s-exp; it just cannot be #f:
                   (assert-pred identity                error-message)
                   (request-accept-type                 request)
                   (request-accept-encoding             request)
                   (request-accept-documentation-format request)))
(define (make-dispatch-handler handler-list)

  (define (%atom->string obj quoted?)
    (let ((quoter (if quoted?
                      (lambda (s) (string-append (string #\") s (string #\")))
                      identity)))
      (cond
       ((string? obj) (quoter obj))
       ((number? obj) (quoter (number->string obj)))
       ((symbol? obj) (quoter (symbol->string obj)))
       (else (error "Unexpected atom" obj)))))

  (define (atom->string obj)
    (%atom->string obj #f))

  (define (atom->qstring obj)
    (%atom->string obj #t))

  ;; (define (unquoted->string obj)        ; no quotes
  ;;   (with-output-to-string (lambda () (display obj))))

  ;; (define (quoted->string obj)          ; with quotes
  ;;   (with-output-to-string (lambda () (write obj))))

  (define (->list ->string)             ; ((->list atom->string) '(1 2 3))
    (lambda (l) (map ->string l)))

  (define (->alist ->string)            ; ((->alist atom->string) '((a . 1) (b . 2) (b . 3)))
    (lambda (al) (map (lambda (p) (cons (->string (car p)) (->string (cdr p)))) al)))

  (define (->alists ->string)           ; ((->alists atom->string) '(((a . 1) (b . 2) (b . 3))))
    (lambda (als) (map (lambda (al) ((->alist ->string) al)) als)))

  ;; --- plain text builders

  (define (alist->plain l)
    (map (lambda (kv) (string-append (car kv) ": " (cdr kv))) ((->alist atom->string) l)))

  (define (alists->plain ll)
    (define ll-strs ((->alists atom->string) ll))
    ;; '(((c1 . HC1) (c2 . HC2)) ((c1 . R1C1) (c2 . R1C2)) ((c1 . R2C1) (c2 . R2C2)))
    ;;   -> HC1 \t HC2 \n R1C1 \t R1C2 \n R2C1 \t R2C2
    (define headers (map cdr (car ll-strs)))
    (define rows (map (lambda (l) (map cdr l)) (cdr ll-strs)))
    (define (format-line l) (string-join l "\t"))
    (string-join (map format-line (cons headers rows)) "\n"))

  ;; --- html builders

  (define (atom->html a)
    ;; "some-string-arg" -> '(div some-string-arg)
    `(div ,(atom->string a)))

  (define (list->html l)
    ;; '(foo bar) -> '(ul (li foo) (li bar))
    `(ul . ,(map (lambda (i) `(li ,i)) ((->list atom->string) l))))

  (define (alist->html l)
    ;; '((k1 . v1) (k2 . v2)) -> '(dl (dt k1) (dd v1) (dt k2) (dd v2))
    (define (flat-map-1 proc l) (apply append (map proc l)))
    `(dl . ,(flat-map-1 (lambda (kv) (list `(dt ,(car kv)) `(dd ,(cdr kv)))) ((->alist atom->string) l))))

  (define (alists->html ll)
    ;; '(((c1 . h1) (c2 . h2)) ((c1 . r11) (c2 . r12)) ((c1 . r21) (c2 . r22)))
    ;;   -> '(table (tr (th h1) (th h2)) (tr (td r11) (td r12)) (tr (td r21) (td r22)))
    (define ll-strs ((->alists atom->string) ll))
    (define headers (map (lambda (h) `(th ,(cdr h))) (car ll-strs)))
    (define rows (map (lambda (l) `(tr . ,(map (lambda (kv) `(td ,(cdr kv))) l))) (cdr ll-strs)))
    `(table . ,(cons `(tr . ,headers) rows)))

  ;; --- (very limited) JSON builders

  (define (%join-items items)
    (string-append "[" (string-join items ",") "]"))

  (define (%join-object obj)
    (string-join (map (lambda (kv) (string-append (car kv) ":" (cdr kv))) obj) ","))

  (define (list->json l)
    ;; '(foo bar) -> ["foo","bar"]
    (%join-items ((->list atom->qstring) l)))

  (define (alist->json l)
    ;; '((k1 . v1) (k2 . v2)) -> {"k1":"v1","k2":"v2"}
    (string-append "{" (%join-object ((->alist atom->qstring) l)) "}"))

  (define (alists->json ll)
    ;; '(((c1 . h1) (c2 . h2)) ((c1 . r11) (c2 . r12)) ((c1 . r21) (c2 . r22)))
    ;;   -> "["c1":"HC1","c2":"HC2","c1":"R1C1","c2":"R1C2","c1":"R2C1","c2":"R2C2"]"
    (%join-items (map %join-object ((->alists atom->qstring) ll))))

  ;; ---

  (define (make-result-formatter request result-type)
    (let* (;; for now we simply pick the first accepted document format, no negotiation:
           (df (car (request-accept-documentation-format request)))
           (at (request-accept-type request)))
      (cond
       ((and (string=? at "text/plain") (string=? df "plaintext"))
        (case result-type
          ((atom)   atom->string)
          ((list)   (->list atom->string))
          ((alist)  alist->plain)
          ((alists) alists->plain)
          (else (error "Bad result type" result-type))))
       ((and (string=? at "text/plain") (string=? df "markdown"))
        (case result-type               ; TODO: implement formatting for markdown
          ((atom)   atom->string)
          ((list)   (->list atom->string))
          ((alist)  alist->plain)
          ((alists) alists->plain)
          (else (error "Bad result type" result-type))))
       ((string=? at "text/html")
        (unless (string=? df "plaintext") (error "Bad documentation format" df))
        (case result-type
          ((atom)   atom->html)
          ((list)   list->html)
          ((alist)  alist->html)
          ((alists) alists->html)
          (else (error "Bad result type" result-type))))
       ((string=? at "application/sexp")
        (unless (string=? df "plaintext") (error "Bad documentation format" df))
        (case result-type
          ((atom)   atom->qstring)
          ((list)   (->list atom->qstring))
          ((alist)  (->alist atom->qstring))
          ((alists) (->alists atom->qstring))
          (else (error "Bad result type" result-type))))
       ((string=? at "application/json")
        (unless (string=? df "plaintext") (error "Bad documentation format" df))
        (case result-type
          ((atom)   atom->qstring)      ; JSON now allows single string as value (RFC4627, RFC7159)
          ((list)   list->json)
          ((alist)  alist->json)
          ((alists) alists->json)
          (else (error "Bad result type" result-type))))
       (else (error "Bad accept type" at)))))

  (define ext-handler-list (append handler-list
                                   ;; append generic handlers - if also user-provided, the generic handler is not used:
                                   `((error-handler alist ,identity))))

  (lambda (request api-key)
    ;; 1st level of dispatch: find handler procedure for given `api-key':
    (let loop ((handler-search-list ext-handler-list))
      (cond
       ((null? handler-search-list)
        (error "Cannot find handler" api-key (map car ext-handler-list)))
       ((eq? (car (car handler-search-list)) api-key)
        (let* ((handler (assert-pred list? (car handler-search-list)))
               (result-type (assert-pred symbol? (cadr handler)))
               (handler-proc (assert-pred procedure? (caddr handler)))
               ;; 2nd level of dispatch: make formatting procedure for request's accept parameters and `result-type':
               (result-formatter (assert-pred procedure? (make-result-formatter request result-type))))
          ;; Return the handler procedure, supporting various argument list formats, which will either return the
          ;; formatted result, if found or #f:
          (lambda args
            (let ((result (apply handler-proc args)))
              (and result (result-formatter result))))))
       (else
        (loop (cdr handler-search-list)))))))
(define (request->response client-info dispatch-handler request)
  (assert-pred <client-info?> client-info)
  (assert-pred procedure? dispatch-handler)
  (assert-pred <request?> request)
  (assert-pred <response?>
               (let ((error-handler (dispatch-handler request 'error-handler))
                     (method (request-method request)))

                 (define (dispatch-nullary api-key http-ok-code http-error-code)
                   (let ((result ((dispatch-handler request api-key) client-info)))
                     (if result
                         (%make-response request http-ok-code result)
                         (%make-error-response request http-error-code
                                               (error-handler `((error   . no-result)
                                                                (api-key . ,api-key)))))))

                 (define (dispatch-unary api-key http-ok-code http-error-code arg-1 arg-missing-tag)
                   (if arg-1
                       (let ((result ((dispatch-handler request api-key) client-info arg-1)))
                         (if result
                             (%make-response request http-ok-code result)
                             (%make-error-response request http-error-code
                                                   (error-handler `((error   . no-result)
                                                                    (api-key . ,api-key)
                                                                    (arg-1   . ,arg-1))))))
                       (%make-error-response request http-error-code
                                             (error-handler `((error   . ,arg-missing-tag)
                                                              (api-key . ,api-key))))))
                 (cond
                  ((string=? method "documentation-index-url")
                   (dispatch-nullary 'documentation-index-url 200 500))
                  ((string=? method "documentation-query-url")
                   (dispatch-unary 'documentation-query-url 200 500 (request-text-at-point request) 'no-text-at-point))
                  ((string=? method "built-in-describe-object")
                   (dispatch-unary 'built-in-describe-object 200 500 (request-text-at-point request) 'no-text-at-point))
                  ((string=? method "built-in-apropos-fragment")
                   (dispatch-unary 'built-in-apropos-fragment 200 500 (request-text-at-point request) 'no-text-at-point))
                  (else
                   (%make-error-response request 500
                                         (error-handler `((error   . api-method-unknown)
                                                          (method  . ,method)))))))))
