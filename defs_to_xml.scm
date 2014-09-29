(display "<gtk-signals>\n")

(define (print-signal name object return when . fragments)
  (display (string-concatenate (list "<signal name=\""name"\" of-object=\""object"\" return-type=\""return"\" when=\""when"\">\n")))
  (display (apply string-append fragments))
  (display "</signal>\n")
)

(define-syntax define-signal
  (syntax-rules ()
    ((_ signalname fragment ...) (print-signal (symbol->string 'signalname) fragment ...))
  )
)

(define (print-property bar . foo) (display ""))
(define-syntax define-property
  (syntax-rules ()
    ((_ a b c d e f g) (print-property (symbol->string 'a) b c d e f g))
  )
)


(define (print-child-property bar . foo) (display ""))
(define-syntax define-child-property
  (syntax-rules ()
    ((_ a b c d e f g) (print-child-property (symbol->string 'a) b c d e f g))
  )
)

(define (of-object obj) obj)

(define (return-type ret) ret)

(define (when wh) wh)

(define (parameter param1)
  (string-concatenate (list "<parameter type=\"" (car param1) "\">" (car (cdr param1)) "</parameter>\n"))
)

(define (parameters . params)
  (string-concatenate (map parameter params))
)


;unused properties
  (define (prop-type foo)foo)
  (define (docs foo)foo)
  (define (readable foo)foo)
  (define (writable foo)foo)
  (define (construct-only foo)foo)

(load "gtk_signals.defs")

(display "</gtk-signals>\n")
