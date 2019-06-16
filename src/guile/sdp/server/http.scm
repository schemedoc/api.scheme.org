(define-module (sdp server http)
  #:use-module (ice-9 binary-ports)     ; get-bytevector-all
  #:use-module (ice-9 match)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module ((srfi srfi-1)  #:prefix list:)
  #:use-module ((srfi srfi-13) #:prefix string:)
  #:use-module (srfi srfi-26)           ; cut, cute
  #:use-module ((srfi srfi-64) #:prefix test:)
  #:use-module ((sdp common model-guile) #:prefix model:)
  #:use-module (sdp common repl-guile)
  #:use-module (sdp common metadata-guile)
  #:use-module (sdp server html)
  #:export (http-server test-http-server)
  #:export (test-render test-response))

(include-from-path "sdp/common/prelude.scm")
(include-from-path "sdp/common/logging.scm")

(define* (request-header-ref request header #:optional dflt)
  (or (assoc-ref (request-headers request) header) dflt))

(define (request-query-components request)
  ;; request -> (alist-of (cons name value)), where name is a symbol and value is a string

  (let ((query (uri-query (request-uri request))))
    (if query
        (map (lambda (query-component)
               (let ((name+value (string-split query-component #\=)))
                 (if (null? (cdr name+value))
                     (cons (string->symbol (car name+value)) #t)                    ; ?foo
                     (cons (string->symbol (car name+value)) (cadr name+value)))))  ; ?foo=bar
             (filter
              (negate string-null?)
              (map uri-decode (string-split query #\&))))
        '())))

(define* (request-query-component request component #:optional dflt)
  (or (assoc-ref (request-query-components request) component) dflt))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (css-link name)
  `(link (@ (rel "stylesheet")
            (href ,(string-append "/css/" name ".css")))))

(define (href content uri)
  `(a (@ (href ,uri)) ,content))

(define (render-plain title line/lines)
  (define lines (if (list? line/lines) line/lines (list line/lines)))
  (define titled-lines (cons (simple-format #f "~a: ~a" title (car lines)) (cdr lines)))
  (values '((content-type . (text/plain))) ; defaults to  #:code 200
          (lambda (port) (write (string-join titled-lines "\n") port))))

(define (render-sexp title sexp)
  (define title-sym (if (symbol? title) title (string->symbol title)))
  (values '((content-type . (application/sexp))) ; defaults to  #:code 200
          (lambda (port) (write (cons title-sym sexp) port))))

(define (render-json _ json-str/strs)
  (define json-strs (if (list? json-str/strs) json-str/strs (list json-str/strs)))
  (values '((content-type . (application/json))) ; defaults to  #:code 200
          (lambda (port) (write (string-join json-strs "\n") port))))

(define (render-html sxml)
  (values '((content-type . (text/html))) ; defaults to  #:code 200
          (lambda (port) (sxml->html sxml port))))

(define (render-simple-html title body)

  (define (template title body)
    `((doctype "html")
      (html
       (head
        (meta (@ (charset "utf-8")))
        (title ,title))
       (body
        (div (@ (id "body"))
             (div (h1 ,title))
             ,body
             (div (span "Powered by GNU Guile")))))))

  (render-html (template title body)))

(define (render-file mime-type file-name)
  (values `((content-type . (,mime-type))) ; defaults to  #:code 200
          (call-with-input-file file-name get-bytevector-all)))

(define* (http-ok #:optional msg)
  (values (build-response #:code 200 #:headers '((content-type . (text/plain))))
          (if msg (lambda (port) (write msg port)) "")))

(define* (http-created uri #:optional msg)
  (values (build-response #:code 201 #:headers `((content-type . (text/plain)) (Location . ,uri)))
          (if msg (lambda (port) (write msg port)) "")))

(define (redirect uri)
  (values (build-response #:code 303 #:headers `((Location . ,uri))) ""))

(define (bad-request error-msg)
  (values (build-response #:code 400 #:headers '((content-type . (text/plain))))
          (lambda (port) (write error-msg port))))

(define (http-forbidden)
  (values (build-response #:code 403)
          "Forbidden"))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request))
                         ", Method: " (symbol->string (request-method request))
                         ", Path: "   (string-join (request-path-components request) "/"))))

(define (server-error)
  (values (build-response #:code 500) "Internal Server Error"))

(define dispatch-handler
  ;; Initialize the request dispatch handler, using the central `make-dispatch-handler' procedure and passing both some
  ;; handlers for test calls as well as the few already handlers featuring fetching of Scheme documentation.

  (let ((md (get-metadata (model:make-client-info-guile))))
    (model:make-dispatch-handler
     `(;; testing
       (test-render-atom          atom   ,identity)
       (test-render-list          list   ,identity)
       (test-render-alist         alist  ,identity)
       (test-render-alists        alists ,identity)
       ;; metadata-based helpers
       (documentation-index-url   atom   ,(assoc-ref md 'scheme-index-url))
       (documentation-query-url   atom   ,(assoc-ref md 'scheme-query-url))
       ;; REPL helpers
       (built-in-describe-object  atom   ,built-in-describe-object)
       (built-in-apropos-fragment atom   ,built-in-apropos-fragment)))))

(define (test-render)
  ;; This procedure - respectively the make target `test-render' - renders the responses to our various
  ;; documentation-fetching handlers, requesting different output formats. Note that it currently does mostly not
  ;; execute tests asserting expected results, but it simply displays the rendered output, assuming that that output
  ;; format is anyway due to change. So the code here is rather presenting how to use the API.

  (define (sxml->html-file sxml file-path)
    (call-with-output-file file-path
      (lambda (port)
        (sxml->html sxml port))))

  (define (sxml->html-string sxml)
    (call-with-output-string
      (lambda (port)
        (sxml->html sxml port))))

  (define (rendered->string rendering-thunk)
    (define-values (_ port-writer) (rendering-thunk))
    (call-with-output-string port-writer))

  (test:test-begin "test-render-guile")
  (test:test-assert (sxml->html-string '(ul (li foo) (li bar)))
                    "<ul><li>foo</li><li>bar</li></ul>")
  (test:test-assert (sxml->html-string '(dl (dt k1) (dd v1) (dt k2) (dd v2)))
                    "<dl><dt>k1</dt><dd>v1</dd><dt>k2</dt><dd>v2</dd></dl>")

  (let ((plain-request (model:make-request "test-render-plain" #:accept-type "text/plain"))
        (html-request  (model:make-request "test-render-html"  #:accept-type "text/html"))
        (sexp-request  (model:make-request "test-render-sexp"  #:accept-type "application/sexp"))
        (json-request  (model:make-request "test-render-json"  #:accept-type "application/json"))
        (atom-arg      "some-string-arg")
        (list-arg      '(some-symbol-arg-1 "some-string-arg-2" 3.14159))
        (alist-arg     '((k1 . v1) (k2 . v2)))
        (alists-arg    '(((c1 . HC1) (c2 . HC2)) ((c1 . R1C1) (c2 . R1C2)) ((c1 . R2C1) (c2 . R2C2)))))

    (define (render-test render-proc sdp-request api-key arg)
      (assert-pred procedure? render-proc)
      (assert-pred model:<request?> sdp-request)
      (assert-pred symbol? api-key)
      (rendered->string (lambda () (render-proc ((dispatch-handler sdp-request api-key) arg)))))

    (define* (exec-test render-proc sdp-request api-key arg #:optional exp-pred/spec)
      (let ((response-str (render-test render-proc sdp-request api-key arg)))
        (cond
         ((procedure? exp-pred/spec)
          (test:test-assert (exp-pred/spec response-str)))
         ((string? exp-pred/spec)
          (test:test-assert (string:string-contains response-str exp-pred/spec)))
         ((eq? exp-pred/spec #t)
          (test:test-assert (string:string-contains response-str arg)))
         (else
          (displayln (list api-key response-str))
          (newline) (newline)))))

    (exec-test (cut render-plain       "plain:test-render-atom"   <>) plain-request 'test-render-atom   atom-arg #t)
    (exec-test (cut render-plain       "plain:test-render-list"   <>) plain-request 'test-render-list   list-arg
               (lambda (response-str)
                 (list:every
                  (lambda (elem)
                    (string:string-contains response-str (format #f "~a" elem)))
                  list-arg)))
    (exec-test (cut render-plain       "plain:test-render-alist"  <>) plain-request 'test-render-alist  alist-arg
               (lambda (response-str)
                 (list:every
                  (lambda (elem)
                    (and (string:string-contains response-str (format #f "~a" (car elem)))
                         (string:string-contains response-str (format #f "~a" (cdr elem)))))
                  alist-arg)))
    (exec-test (cut render-plain       "plain:test-render-alists" <>) plain-request 'test-render-alists alists-arg
               (lambda (response-str)
                 (list:every
                  (lambda (alist-arg)
                    (list:every
                     (lambda (elem)
                       (string:string-contains response-str (format #f "~a" (cdr elem))))
                     alist-arg))
                  alists-arg)))

    (exec-test (cut render-simple-html "html:test-render-atom"    <>) html-request  'test-render-atom   atom-arg #t)
    (exec-test (cut render-simple-html "html:test-render-list"    <>) html-request  'test-render-list   list-arg)
    (exec-test (cut render-simple-html "html:test-render-alist"   <>) html-request  'test-render-alist  alist-arg)
    (exec-test (cut render-simple-html "html:test-render-alists"  <>) html-request  'test-render-alists alists-arg)

    (exec-test (cut render-sexp        "sexp:test-render-atom"    <>) sexp-request  'test-render-atom   atom-arg #t)
    (exec-test (cut render-sexp        "sexp:test-render-list"    <>) sexp-request  'test-render-list   list-arg)
    (exec-test (cut render-sexp        "sexp:test-render-alist"   <>) sexp-request  'test-render-alist  alist-arg)
    (exec-test (cut render-sexp        "sexp:test-render-alists"  <>) sexp-request  'test-render-alists alists-arg)

    (exec-test (cut render-json        "json:test-render-atom"    <>) json-request  'test-render-atom   atom-arg #t)
    (exec-test (cut render-json        "json:test-render-list"    <>) json-request  'test-render-list   list-arg)
    (exec-test (cut render-json        "json:test-render-alist"   <>) json-request  'test-render-alist  alist-arg)
    (exec-test (cut render-json        "json:test-render-alists"  <>) json-request  'test-render-alists alists-arg))
  (test:test-end "test-render-guile"))

(define (test-response)
  ;; This test procedure - respectively the make target `test-response' - builds the complete response for the given
  ;; request for each of our Scheme documentation APIs, using the default response renderer. The test procedure also
  ;; tests correct error handling for some typical error constellations. Again currently the test rather displays the
  ;; rendered output; it will be changed to assert expected output once the output format has been stabilized.

  (test:test-begin "test-response-guile")
  (let ((client-info (model:make-client-info-guile)))

    (define (render-test sdp-request)
      (assert-pred model:<request?> sdp-request)
      (model:request->response client-info dispatch-handler sdp-request))

    (define* (exec-test sdp-request #:optional exp-pred/spec)
      (let ((response (render-test sdp-request)))
        (cond
         ((procedure? exp-pred/spec)
          (test:test-assert (exp-pred/spec response)))
         ((string? exp-pred/spec)
          (test:test-assert (= (model:response-http-code response) 200))
          (test:test-assert (string:string-contains (model:response-result response) exp-pred/spec))
          (test:test-assert (not (model:response-error-message response))))
         ((number? exp-pred/spec)
          (test:test-assert (= (model:response-http-code response) exp-pred/spec))
          (test:test-assert (not (model:response-result response)))
          (test:test-assert (model:response-error-message response)))
         ((eq? exp-pred/spec #t)
          (test:test-assert (= (model:response-http-code response) 200))
          (test:test-assert (positive? (string-length (model:response-result response))))
          (test:test-assert (not (model:response-error-message response))))
         (else
          (displayln (list sdp-request '--> response))
          (newline) (newline)))))

    (exec-test (model:make-request "documentation-index-url")
               "https://www.gnu.org/software/guile/manual/")
    (exec-test (model:make-request "documentation-query-url"   #:text-at-point "format")
               "https://practical-scheme.net/wiliki/schemexref.cgi?format")
    (exec-test (model:make-request "built-in-describe-object"  #:text-at-point "or-map") #t)
    (exec-test (model:make-request "built-in-apropos-fragment" #:text-at-point "bind") #t)
    ;; api method not found:
    (exec-test (model:make-request "---totally-unknown") 500)
    ;; no result found:
    (exec-test (model:make-request "built-in-apropos-fragment" #:text-at-point "---totally-unknown") 500)
    ;; test for other response formats; TODO: also check error response formats
    (exec-test (model:make-request "documentation-index-url"   #:accept-type "text/plain"))
    (exec-test (model:make-request "documentation-index-url"   #:accept-type "text/html"))
    (exec-test (model:make-request "documentation-index-url"   #:accept-type "application/sexp"))
    (exec-test (model:make-request "documentation-index-url"   #:accept-type "application/json"))
    )
  (test:test-end "test-response-guile"))

(define (render-response sdp-response)
  (assert-pred model:<response?> sdp-response)

  (if (model:response-error-message sdp-response)
      (values (build-response #:code (model:response-http-code sdp-response)
                              #:headers `((content-type . (,(string->symbol (model:response-content-type sdp-response))))))
              (lambda (port) (write (model:response-error-message sdp-response) port)))
      (values (build-response #:code (model:response-http-code sdp-response)
                              #:headers `((content-type . (,(string->symbol (model:response-content-type sdp-response))))))
              (lambda (port) (write (or (model:response-result sdp-response) "") port)))))

(define (guile-request->response request method impl text-at-point test-mode?)
  (let* ((client-version   (request-query-component request 'version))
         (client-mode      (request-query-component request 'mode))
         (client-info      (case impl
                             ((guile)
                              (model:make-client-info-guile #:implementation-version client-version
                                                            #:implementation-mode client-mode))
                             ((gauche)
                              (model:make-generic-client-info))
                             (else
                              (model:make-unknown-client-info))))
         (request-ctype    (request-content-type request '(text/html)))
         (request-charset  (or (assq-ref (cdr request-ctype) 'charset) "utf-8"))
         (request-atypes   (request-accept request '((text/html))))
         (query-format     (request-query-component request 'format))
         (accept-type      (let ((header-atype (symbol->string (car (car request-atypes)))))
                             (if query-format
                                 (cond
                                  ((string=? query-format "sexp")  "application/sexp")
                                  ((string=? query-format "json")  "application/json")
                                  ((string=? query-format "html")  "text/html")
                                  ((string=? query-format "plain") "text/plain")
                                  (else header-atype))
                                 header-atype))))
    (if (request-query-component request 'debug)
        (if test-mode?
            (render-simple-html (simple-format #f "API-method ~s" method)
                                (simple-format #f "content-type: ~s; accept-types: ~s; accept-mime: ~s~%\
implementation: ~s; client-info: ~s; text-at-point: ~s"
                                        request-ctype request-atypes accept-type
                                        impl client-info text-at-point))
            (not-found request))
        (render-response (model:request->response client-info dispatch-handler
                                                  (model:make-request method
                                                                      #:text-at-point text-at-point
                                                                      #:accept-type accept-type))))))

(define* (make-api-handler #:optional (test-mode? #f))
  ;; Define our HTTP router. The makefile targets for the un-instrumented HTTP server and for the debug-server are
  ;; `run-http-server' and `run-test-http-server,' respectively.

  (lambda (request body)
    ;; https://www.gnu.org/software/guile/manual/html_node/Requests.html#Requests
    (let ((uri      (request-uri request))
          (method   (request-method request))
          (path-cs  (request-path-components request))
          (query-cs (request-query-components request))
          (atypes   (list 'acc:  (map (lambda (i) (format #f "|~s|" i)) (request-accept request))
                          'acs:  (request-accept-charset request)    ; Accept-Charset
                          'aenc: (request-accept-encoding request)   ; Accept-Encoding
                          'acl:  (request-accept-language request))) ; Accept-Language
          (ctype    (request-content-type request))
          ;; (body    (if body (utf8->string body) #f))
          ;; (body-string (utf8->string body))
          ;; (member 'application/x-www-form-urlencoded ctype)
          (upath   (uri-path (request-uri request)))
          (query   (uri-query (request-uri request))))

      ;; http://localhost:8080/
      ;;   #<<uri> scheme: #f userinfo: #f host: #f port: #f path: "/" query: #f fragment: #f>
      ;;      ...  GET/#f#f
      ;; http://localhost:8080/foo/bar?dumm=doedel&dooh=ouch
      ;;   #<<uri> scheme: #f userinfo: #f host: #f port: #f path: "/foo/bar" query: "dumm=doedel&dooh=ouch" fragment: #f>
      ;;           method         GET
      ;;           path-cs        foobar
      ;;           upath          /foo/bar
      ;;           atypes         |(text/html)||(application/xhtml+xml)||(application/xml (q . 900))||(image/webp)||(image/apng)||(*/* (q . 800))|
      ;;                          acs:aenc:(1000 . gzip)(1000 . deflate)(1000 . br)acl:(1000 . en-US)(900 . en)
      ;;           ctype          #f
      ;;           query          dumm=doedel&dooh=ouch
      ;;           query-cs       (dumm . doedel)(dooh . ouch)
      (match (cons method path-cs)
             (('GET . '())                     ; http://localhost:8080/
              (render-simple-html "Index-Request"
                                  (list uri method path-cs upath atypes ctype query query-cs)))
             (('GET . ("api" "index" impl))
              ;; http://localhost:8080/api/index/guile
              ;; http://localhost:8080/api/index/guile?debug
              ;; http://localhost:8080/api/index/guile?format=sexp
              (guile-request->response request "documentation-index-url" impl #f test-mode?))
             (('GET . ("api" "query" impl text-at-point))
              ;; http://localhost:8080/api/query/guile/define
              ;; http://localhost:8080/api/query/guile/define?debug
              ;; http://localhost:8080/api/query/guile/define?format=sexp
              (guile-request->response request "documentation-query-url" impl text-at-point test-mode?))
             (('GET . ("internal" "kill"))     ; http://localhost:8080/internal/kill
              (if test-mode?
                  (begin
                    (info "Test server shutdown: " (+schemedoc-host-address+) (+schemedoc-port+))
                    (kill (getpid) SIGINT))
                  (not-found request)))
             (_ (not-found request))))))

(define (http-server)
  (info "Server start: " (+schemedoc-host-address+) (+schemedoc-port+))
  (run-server (make-api-handler)))

(define (test-http-server)
  (info "Test server start: " (+schemedoc-host-address+) (+schemedoc-port+))
  (run-server (make-api-handler #t)))
