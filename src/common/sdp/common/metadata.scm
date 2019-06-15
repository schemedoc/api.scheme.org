(define +schemedoc-host-address+
  (make-parameter "github.com" (lambda (v) (format #f "~a" v))))
(define +schemedoc-port+
  (make-parameter 9090 (lambda (v) (if (number? v) v (string->number v)))))
(define +schemedoc-repl+                ; guile default REPL port: 37146
  (make-parameter 37146 (lambda (v) (if (number? v) v (string->number v)))))

;; TODO: It's probably best to put this into a `init-args' procedure and call that from each including program. Then we
;;   can also pass arguments for version and usage.
;;   Also for now, I'm just collecting whatever arguments might be interesting, even if not every CLI using that will
;;   require all arguments.
(let ((cl-args (cond-expand (gauche (command-line)) (guile (cdr (command-line))))))

  (define (try-host-address arg host-address)
    (cond-expand
     (gauche
      (if (sys-gethostbyname host-address)
          ;; No `getaddrinfo' support with Gauche, and `sys-getaddrinfo' works differently, so use `sys-gethostbyname'
          ;; (which is IPv4 only). `sys-gethostbyname' returns #f, if address cannot be resolved.
          host-address
          (begin
            (warn "Cannot find host" arg host-address)
            host-address)))
     (guile
      ;; If Guile's `getaddrinfo' cannot resolve the host address, it will raise an exception that cannot be caught by
      ;; SRFI-34's `with-exception-handler' and `guard', so we need to use the Guile-specific catch instead.
      (catch 'getaddrinfo-error
        (lambda ()
          (getaddrinfo host-address)
          host-address)
        (lambda (key error-code)
          (begin
            (warn "Cannot find host" arg host-address key error-code)
            host-address))))))

  (define (must-host-address arg host-address)
    (cond-expand
     (gauche                            ; see `try-host-address'
      (if (sys-gethostbyname host-address)
          host-address
          (error-exit "Cannot find host" arg host-address)))
     (guile                             ; see `try-host-address'
      (catch 'getaddrinfo-error
             (lambda ()
               (getaddrinfo host-address)
               host-address)
             (lambda (key error-code)
               (error-exit "Cannot find host" arg host-address key error-code))))))

  (define (must-number arg val)
    (if (number? val)
        val
        (let ((i (string->number val)))
          (if i
              i
              (error-exit "Not an integer" arg val)))))

  (define (must-number-or-boolean arg val f-val t-val)
    (cond
     ((number? val) val)
     ((or (and (boolean? val) (not val))
          (and (string? val) (string=? val "#f")))
      f-val)
     ((or (and (boolean? val) val)
          (and (string? val) (string=? val "#t")))
      t-val)
     (else (must-number arg val))))

  (info "Defaults: "
        (+%%debug-level+) (+schemedoc-host-address+) (+schemedoc-port+) (+schemedoc-repl+))

  (receive (p-debug-level p-host p-port p-repl)
      (args:args-fold cl-args
                      (let ((display-and-exit-proc
                             (lambda (msg)
                               (lambda (opt name arg . seeds)
                                 (display msg) (quit)))))
                        (list (args:option '(#\v "version") #f #f
                                           (display-and-exit-proc "Foo version 42.0\n"))
                              (args:option '(#\h "help") #f #f
                                           (display-and-exit-proc
                                            "Usage: foo scheme-file ..."))
                              (args:option '(#\d "debug") #f #t
                                           (lambda (opt name arg debug host port repl)
                                             (values (or arg (+%%debug-level+)) host port repl)))
                              (args:option '(#\a "hostaddress") #f #t
                                           (lambda (opt name arg debug host port repl)
                                             (let ((addr (and arg (try-host-address 'host arg))))
                                               (values debug (or arg (+schemedoc-host-address+)) port repl))))
                              (args:option '(#\p "port") #f #t
                                           (lambda (opt name arg debug host port repl)
                                             (values debug host (or arg (+schemedoc-port+)) repl)))
                              (args:option '(#\r "repl") #f #t
                                           (lambda (opt name arg debug host port repl)
                                             (lambda (opt name arg debug host port repl)
                                               (values debug host port (or arg (+schemedoc-repl+))))))))
                      (lambda (opt name arg . seeds)
                        (simple-format (current-error-port) "Unrecognized option `~A'" name))
                      (lambda (op debug host port repl)
                        (values debug host port repl))
                      (+%%debug-level+)
                      (+schemedoc-host-address+)
                      (+schemedoc-port+)
                      (+schemedoc-repl+))
    ;; initialize defaults:
    (+%%debug-level+          (must-number-or-boolean 'debug-level p-debug-level 0 999))
    (+schemedoc-host-address+ (try-host-address       'host        p-host))
    (+schemedoc-port+         (must-number            'port        p-port))
    (+schemedoc-repl+         (must-number            'repl        p-repl)))

  (info "Command line parsed: "
        (+%%debug-level+) (+schemedoc-host-address+) (+schemedoc-port+) (+schemedoc-repl+)))
;; Note: below we define some of the handler procedures dispatched by the handler created with `make-dispatch-handler'.
;;   These handler procedures will usually be called with an argument list, where the first element is the client info
;;   and - where required - the second element is the text-at-point.

(define +dflt-get-scheme-index-url+
  ;; Symbol index URL used as default for the case where Scheme-implementation metadata cannot be found.
  (make-parameter (lambda _ "https://practical-scheme.net/wiliki/schemexref.cgi?R7RS")))

(define +dflt-get-scheme-query-url+
  ;; Symbol query URL used as default for the case where Scheme-implementation metadata cannot be found.
  ;; This URL is supposed to work as a prefix for the symbol to be searched for.
  (make-parameter (lambda args (string-append "https://practical-scheme.net/wiliki/schemexref.cgi?" (cadr args)))))

(define mappers
  `((scheme-index-url (// documentation web-url *text*)
                      ,(lambda (child) (lambda _ child))
                      ,(+dflt-get-scheme-index-url+))
    (scheme-query-url (// documentation search-url *text*)
                      ,(lambda (child) (lambda args (string-append child (cadr args))))
                      ,(+dflt-get-scheme-query-url+))))

(define (%%try-get-metada-file-name md-path-name client-info)
  (assert-pred <client-info?> client-info)
  (let ((file-name (string-append md-path-name "/" (client-info-implementation-name client-info) ".scm")))
    (debug '%%try-get-metada-file-name md-path-name file-name (file-exists? file-name))
    (and (file-exists? file-name) file-name)))

(define (%get-metadata-file md-path-name client-info slurp)
  (let ((file-name (%%try-get-metada-file-name md-path-name client-info)))
    (debug '%get-metadata-file md-path-name file-name)
    (and file-name (call-with-input-file file-name slurp))))

(define (%get-metadata md-path-name client-info)

  (define (try-get-match form path builder)

    (define (assert-procedure? p)
      (if (procedure? p) p (error "Not a procedure" p)))

    ;; Prepend *TOP*, so that we can add the car of the metadata from to the sxpath.
    (let ((child ((xpath:sxpath path) (cons '*TOP* form))))
      (if (and child (not (null? child)))
          (assert-procedure? (builder (car child)))
          #f)))

  (define (add-defaults alist mappers)
    (map
     (match-lambda ((tag _ builder dflt)
                    (let ((pair (assoc tag alist)))
                      (or pair (cons tag dflt)))))
     mappers))

  (let ((file-name (%%try-get-metada-file-name md-path-name client-info)))
    (if file-name
        (call-with-input-file file-name
          (lambda (p)
            (let loop ((form (read p))
                       (alist '()))
              (if (eof-object? form)
                  (add-defaults alist mappers)
                  (loop (read p)
                        (append alist
                                (filter identity
                                        (map
                                         (match-lambda ((tag path builder _)
                                                        (let ((p (try-get-match form path builder)))
                                                          (and p (cons tag p)))))
                                         mappers))))))))
        (add-defaults '() mappers))))
