(define-module (sdp server http)
  #:use-module (ice-9 binary-ports)     ; get-bytevector-all
  #:use-module (ice-9 match)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module ((srfi srfi-13) #:prefix string:)
  #:use-module ((srfi srfi-64) #:prefix test:)
  #:use-module (sdp common model-guile)
  #:use-module (sdp common repl-guile)
  #:use-module (sdp common metadata-guile)
  #:use-module (sdp server html)
  #:export (http-server)
  #:export (test))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (request-query-components request)
  ;; -> (alist-of (cons name value))
  (let ((query (uri-query (request-uri request))))
    (if query
        (map (lambda (query-component)
               (let ((name+value (string-split query-component #\=)))
                 (cons (string->symbol (car name+value)) (car (cdr name+value)))))
             (filter
              (lambda (query-component) (not (string-null? query-component)))
              (map uri-decode (string-split query #\&))))
        '())))

(define (request-header-ref request header)
  (assoc-ref (request-headers request) header))

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
             ,@body                       ; TODO: ,body or ,@
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
(include-from-path "sdp/common/prelude.scm")
(include-from-path "sdp/common/logging.scm")

(define dispatch-handler
  (let ((md (get-metadata (make-client-info-guile))))
    (make-dispatch-handler
     `((documentation-index-url   atom  ,(assoc-ref md 'scheme-index-url))
       (documentation-query-url   atom  ,(assoc-ref md 'scheme-query-url))
       (built-in-describe-object  atom  ,built-in-describe-object)
       (built-in-apropos-fragment atom  ,built-in-apropos-fragment)))))

(define (test)
  (test:test-begin "test-http-server-guile")
  (let ((client-info (make-client-info-guile)))
    (display (request->response client-info dispatch-handler (make-request "documentation-index-url")))
    (display (request->response client-info dispatch-handler (make-request "documentation-query-url" #:text-at-point "format"))))
  (test:test-end "test-http-server-guile"))
(define (api-handler request body)
  ;; https://www.gnu.org/software/guile/manual/html_node/Requests.html#Requests
  (let ((uri      (request-uri request))
        (method   (request-method request))
        (path-cs  (request-path-components request))
        (query-cs (request-query-components request))
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
    ;;      ...  GETfoobar/foo/bar#fdumm=doedel&dooh=ouch
    ;;           method         GET
    ;;           path-cs        foobar
    ;;           upath          /foo/bar
    ;;           ctype          #f
    ;;           query          dumm=doedel&dooh=ouch
    ;;           query-cs       (dumm . doedel)(dooh . ouch)
    (match (cons method path-cs)
      (('GET . '())                     ; http://localhost:8080/
       (render-simple-html "Index-Request"
                           (list uri method path-cs upath ctype query query-cs)))
      (('GET . ("api" "index" impl))   ; http://localhost:8080/api/index/guile
       (render-simple-html "api-index-Request"
                           (list uri method path-cs upath ctype query query-cs impl)))
      (('GET . ("api" "query" impl symbol)) ; http://localhost:8080/api/query/guile/define
       ;; TODO: add version -> query param or path part?
       ;; TODO: add "mode", e.g. native, R7rs, ... -> query param or path part?
       (render-simple-html "api-query-Request"
                           (list uri method path-cs upath ctype query query-cs impl symbol)))
      (('GET . ("foo" "bar"))          ; http://localhost:8080/foo/bar, http://localhost:8080/foo/bar?foo=f1&bar=b1...
       (render-simple-html "Path-Request"
                           (list uri method path-cs upath ctype query query-cs)))
      (_ (not-found request)))))

(define (http-server)
  (info "Server start: " (+schemedoc-host-address+) (+schemedoc-port+))
  (run-server api-handler))
