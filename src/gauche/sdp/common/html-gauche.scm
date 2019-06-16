(define-module sdp.common.html-gauche
  (use util.match)
  (use gauche.parameter)
  (use srfi-9)
  (export sxml->html sxml->html-string))
(select-module sdp.common.html-gauche)

(define (sxml->html sxml port)
  ;; https://practical-scheme.net/gauche/man/gauche-refe/Manipulating-SXML-structure.html
  ;; https://practical-scheme.net/gauche/man/gauche-refe/Serializing-XML-and-HTML-from-SXML.html#Serializing-XML-and-HTML-from-SXML
  (error "TODO:Implement"))

(define (sxml->html-string sxml)
  (call-with-output-string
    (lambda (port)
      (sxml->html sxml port))))
