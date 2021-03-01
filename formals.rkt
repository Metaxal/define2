#lang racket/base

;;; Adapted from syntax/parse/lib/function-header

(require syntax/parse
         racket/dict
         racket/list
         racket/syntax)

(provide keyword-argument
         argument
         splicing-arguments
         arguments+rest
         syntax->keyword)

;; We need to make sure that `no-value` is available at runtime,
;; if the surrounding module is required `for-syntax`.
;; But there is no `begin-for-template`, so we need a workaround.
(module no-value-mod racket/base
  (provide no-value no-value?)
  (define no-value (string->uninterned-symbol "no-value"))
  (define (no-value? x)
    (eq? x no-value)))

(require (for-template 'no-value-mod
                       racket/base))

;; This class ensures that the order of the arguments is preserved.
(define-splicing-syntax-class splicing-arguments
  #:attributes (names kws defaults? binders call-args header)
  (pattern (~seq arg:argument ...)
           #:attr names    #'(arg.name ...)
           #:attr kws      #'((~? arg.kw #f) ...) ; `#:! a` is `#:a`
           #:attr defaults? (datum->syntax
                             #f
                             (map (λ (d) (if d #'#true #'#false))
                                  (attribute arg.default))) ; (list-of boolean?)
           ;#'((~? arg.default #f) ...)
           #:attr binders  #'((~? arg.binder) ...)
           #:attr header   #'((~@ (~? arg.kw) (~? (arg.gen-name arg.default) arg.name)) ...)
           #:attr call-args #'((~? (~@ arg.kw arg.name) arg.name) ...)
           #:fail-when (check-duplicate-identifier (attribute arg.name))
                       "duplicate argument identifier"
           #:fail-when (check-duplicates (attribute arg.kw)
                                         (λ (x y)
                                           (and x y (equal? (syntax-e x)
                                                            (syntax-e y)))))
                       "duplicate argument keyword"
           #:fail-when (invalid-option-placement (attribute arg.kw)
                                                 (attribute arg.name)
                                                 (attribute arg.default))
                       "mandatory positional argument after optional positional argument"))

(define-syntax-class arguments+rest
  #:attributes (names kws defaults? binders call-args header)
  (pattern (~or* (args:splicing-arguments)
                 (args:splicing-arguments . rest-id:id))
           #:attr names    #'((~@ . args.names) (~? rest-id))
           #:attr kws      #'args.kws
           #:attr defaults? (attribute args.defaults?)
           #:attr binders  #'args.binders
           #:attr header (if (attribute rest-id)
                           #'((~@ . args.header) . rest-id)
                           #'args.header)
           #:attr call-args #'args.call-args
           #:fail-when (and (attribute rest-id)
                            (member #'rest-id (syntax->list #'args.names) bound-identifier=?)
                            #'rest-id)
                       "duplicate argument identifier"))

(define (syntax->keyword stx)
  (string->keyword (format "~a" (syntax-e stx))))

(define-splicing-syntax-class keyword-argument
  #:commit ; force greedy matching, no backtracking
  #:attributes (name gen-name kw default binder)
  (pattern (~seq #:! name:id)
           #:attr gen-name #f
           #:with kw2 (syntax->keyword #'name)
           #:attr kw #'kw2
           #:attr default #f
           #:attr binder #f)
  (pattern (~seq #:? name:id)
           #:attr gen-name #'name ; no need to generate a temporary, but serves to say optional arg
           #:with kw2 (syntax->keyword #'name)
           #:attr kw #'kw2
           #:attr default #'no-value
           #:attr binder #f)
  (pattern (~seq #:? [name:id given-default:expr])
           #:with gen-name (generate-temporary)
           #:with kw2 (syntax->keyword #'name)
           ;#:attr name #'name
           #:attr default #'no-value
           #:attr kw #'kw2
           #:attr binder #'[name (if (eq? gen-name no-value) given-default gen-name)])
  (pattern (~seq kw:keyword name:id)
           #:attr gen-name #f
           #:attr default #f
           #:attr binder #f)
  (pattern (~seq kw:keyword [name:id given-default:expr])
           #:with gen-name (generate-temporary)
           #:attr default #'no-value
           #:attr binder #'[name (if (eq? gen-name no-value) given-default gen-name)]))

(define-splicing-syntax-class argument
  #:commit ; force greedy matching, no backtracking
  #:attributes (name gen-name kw default binder)
  (pattern name:id
           #:attr gen-name #f
           #:attr kw #f
           #:attr default #f
           #:attr binder #f)
  (pattern [name:id default:expr]
           #:attr gen-name #'name ; no need to generate a temporary, but serves to say optional arg
           #:attr kw #f
           #:attr binder #f)
  (pattern kw-arg:keyword-argument
           #:attr name     (attribute kw-arg.name)
           #:attr gen-name (attribute kw-arg.gen-name)
           #:attr default  (attribute kw-arg.default)
           #:attr kw       (attribute kw-arg.kw)
           #:attr binder   (attribute kw-arg.binder)))

;; invalid-option-placement : (Listof Keyword) (Listof Id) (Listof Syntax/#f) -> Id/#f
;; Checks for mandatory argument after optional argument; if found, returns
;; identifier of mandatory argument.
;; Fix from original version: order does not matter in keyword arguments.
(define (invalid-option-placement kws names defaults)
  ;; find-mandatory : (Listof Keyword) (Listof Id) (Listof Syntax/#f) -> Id/#f
  ;; Finds first name w/o corresponding default.
  (define (find-mandatory kws names defaults)
    (for/first ([kw      (in-list kws)]
                [name    (in-list names)]
                [default (in-list defaults)]
                #:when (and (not kw) (not default)))
      name))
  ;; Skip through mandatory args until first optional found, then search
  ;; for another mandatory.
  (let loop ([kws kws] [names names] [defaults defaults])
    (cond [(or (null? names) (null? defaults))
           #f]
          [(or (car kws) ; keywords don't count
               (eq? (car defaults) #f)) ;; mandatory
           (loop (cdr kws) (cdr names) (cdr defaults))]
          [else ;; found optional
           (find-mandatory (cdr kws) (cdr names) (cdr defaults))])))

(module+ test
  (require rackunit)

  ;; Backward compatibility with lambda for weird argument positions
  (check-equal?
   (syntax->datum
    (syntax-parse #'(a #:b [b 1] c #:d d [e 2] #:f [f 3] [g 4] . rest)
      [fmls:arguments+rest #'(fmls fmls.names)]))
   '((a #:b (b 1) c #:d d (e 2) #:f (f 3) (g 4) . rest) (a b c d e f g rest)))

  ;; Default value let* binding
  (check-match
   (syntax-parse #'(#:? [x 3] #:y [y (+ x 3)] #:? [z (+ y 10)])
     [fmls:arguments+rest
      (syntax->datum
       #'(lambda fmls.header
           (let* fmls.binders
             'xxx)))])
   `(lambda (#:x (,x-gen no-value) #:y (,y-gen no-value) #:z (,z-gen no-value))
      (let* ((x (if (eq? ,x-gen no-value) 3 ,x-gen))
             (y (if (eq? ,y-gen no-value) (+ x 3) ,y-gen))
             (z (if (eq? ,z-gen no-value) (+ y 10) ,z-gen)))
        'xxx)))

  )
