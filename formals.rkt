#lang racket/base

;;; Adapted from syntax/parse/lib/function-header

(require syntax/parse
         racket/dict
         racket/list)

(provide keyword-argument
         argument
         splicing-arguments
         arguments+rest
         syntax->keyword)

;; This class ensures that the order of the arguments is preserved.
(define-splicing-syntax-class splicing-arguments
  #:attributes (names kws binders call-args header)
  (pattern (~seq arg:argument ...)
           #:attr names    #'(arg.name ...)
           #:attr kws      #'((~? arg.kw #f) ...) ; #:! a is #:a
           #:attr binders  #'((~? arg.binder) ...)
           #:attr header   #'((~@ (~? arg.kw) (~? (arg.name arg.default) arg.name)) ...)
           #:attr call-args #'((~? (~@ arg.kw arg.name) arg.name) ...)
           #:fail-when (check-duplicate-identifier (attribute arg.name))
                       "duplicate argument identifier"
           #:fail-when (check-duplicates (attribute arg.kw)
                                         (λ (x y)
                                           (and x y (equal? (syntax-e x)
                                                            (syntax-e y)))))
                       "duplicate argument keyword"
           #:fail-when (invalid-option-placement (attribute arg.kw) (attribute arg.name) (attribute arg.default))
                       "mandatory positional argument after optional positional argument"))

(define-syntax-class arguments+rest
  #:attributes (names kws binders call-args header)
  (pattern (~or* (args:splicing-arguments)
                 (args:splicing-arguments . rest-id:id))
           #:attr names    #'((~@ . args.names) (~? rest-id))
           #:attr kws      #'args.kws
           #:attr binders  #'args.binders
           #:attr header (if (attribute rest-id)
                           #'((~@ . args.header) . rest-id)
                           #'args.header)
           #:attr call-args #'args.call-args
           #:fail-when (and (attribute rest-id)
                            (member #'rest-id (syntax->list #'args.names) bound-identifier=?)
                            #'rest-id)
                       "duplicate argument identifier"))

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

(define (syntax->keyword stx)
  (string->keyword (format "~a" (syntax-e stx))))

(define-splicing-syntax-class keyword-argument
  #:commit ; force greedy matching, no backtracking
  #:attributes (name kw default binder)
  (pattern (~seq #:! name:id)
           #:with kw2 (syntax->keyword #'name)
           #:attr kw #'kw2
           #:attr default #f
           #:attr binder #f)
  (pattern (~seq #:? name:id)
           #:with kw2 (syntax->keyword #'name)
           #:attr kw #'kw2
           #:attr default #'no-value
           #:attr binder #f)
  (pattern (~seq #:? [name:id default])
           #:with kw2 (syntax->keyword #'name)
           #:attr kw #'kw2
           #:attr binder #'[name (if (eq? name no-value) default name)])
  (pattern (~seq kw:keyword name:id)
           #:attr default #f
           #:attr binder #f)
  (pattern (~seq kw:keyword [name:id default])
           #:attr binder #f))

(define-splicing-syntax-class argument
  #:commit ; force greedy matching, no backtracking
  #:attributes (name kw default binder)
  (pattern name:id
           #:attr kw #f
           #:attr default #f
           #:attr binder #f)
  (pattern [name:id default]
           #:attr kw #f
           #:attr binder #f)
  (pattern kw-arg:keyword-argument
           #:attr name    (attribute kw-arg.name)
           #:attr default (attribute kw-arg.default)
           #:attr kw      (attribute kw-arg.kw)
           #:attr binder  (attribute kw-arg.binder)))

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
  (check-equal?
   (syntax->datum
    (syntax-parse #'(a [b 'bb] #:! x #:? [y 'yy] #:? z #:r r #:q [q 'qq] #:? [u 'uu])
      [(args:splicing-arguments)
       #'args.binders]))
   '([y (if (eq? y no-value) 'yy y)]
     [u (if (eq? u no-value) 'uu u)]))

  ;; Backward compatibility with lambda for weird argument positions
  (check-equal?
   (syntax->datum
    (syntax-parse #'(a #:b [b 1] c #:d d [e 2] #:f [f 3] [g 4] . rest)
      [fmls:arguments+rest #'(fmls fmls.names)]))
   '((a #:b (b 1) c #:d d (e 2) #:f (f 3) (g 4) . rest) (a b c d e f g rest)))

  )
