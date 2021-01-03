#lang racket/base

;;; Adapted from syntax/parse/lib/function-header

(require syntax/parse
         racket/dict
         racket/list)

(provide #;function-header formal formals syntax->keyword)

#;
(define-syntax-class function-header ; not used
  #:attributes (name params args)
  (pattern ((~or header:function-header name*:id) . args:formals)
           #:attr params #'((~@ . (~? header.params ())) . args.params)
           #:attr name   #'(~? header.name name*)))

(define-syntax-class formals
  #:attributes (params header binders)
  (pattern (arg:formal ...)
           #:attr params #'(arg.name ...)
           #:attr header #'((~@ (~? arg.kw) (~? (arg.name arg.default) arg.name)) ...)
           #:attr binders #'((~? arg.binder) ...)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
                       "duplicate argument identifier"
           #:fail-when (check-duplicates (attribute arg.kw)
                                         (λ (x y)
                                           (and x y (equal? (syntax-e x)
                                                            (syntax-e y)))))
                       "duplicate argument keyword"
           #:fail-when (invalid-option-placement
                        (attribute arg.name) (attribute arg.name) (attribute arg.default))
                       "default-value expression missing or wrong argument order")
  (pattern (arg:formal ... . rest:id)
           #:attr params #'(arg.name ... rest)
           #:attr binders #'((~? arg.binder) ...)
           #:attr header #'((~@ (~? arg.kw) (~? (arg.name arg.default) arg.name)) ... . rest)
           #:fail-when (check-duplicate-identifier (syntax->list #'params))
                       "duplicate argument identifier"
           #:fail-when (check-duplicates (attribute arg.kw)
                                         (λ (x y)
                                           (and x y (equal? (syntax-e x)
                                                            (syntax-e y)))))
                       "duplicate argument keyword"
           #:fail-when (invalid-option-placement
                        (attribute arg.name) (attribute arg.name) (attribute arg.default))
                       "default-value expression missing or wrong argument order"))

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

(define-splicing-syntax-class formal
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
  (pattern name:id
           #:attr kw #f
           #:attr default #f
           #:attr binder #f)
  (pattern [name:id default]
           #:attr kw #f
           #:attr binder #f)
  (pattern (~seq kw:keyword name:id)
           #:attr default #f
           #:attr binder #f)
  (pattern (~seq kw:keyword [name:id default])
           #:attr binder #f))

;; invalid-option-placement : (Listof Keyword) (Listof Id) (Listof Syntax/#f) -> Id/#f
;; Checks for mandatory argument after optional argument; if found, returns
;; identifier of mandatory argument.
;; Fix from original version: order does not matter in keyword arguments.
(define (invalid-option-placement kws names defaults)
  ;; find-mandatory : (Listof Id) (Listof Syntax/#f) -> Id/#f
  ;; Finds first name w/o corresponding default.
  (define (find-mandatory kws names defaults)
    (for/first ([kw (in-list kws)]
                [name (in-list names)]
                [default (in-list defaults)]
                #:when (and (not kw) (not default)))
      name))
  ;; Skip through mandatory args until first optional found, then search
  ;; for another mandatory.
  (let loop ([kws kws] [names names] [defaults defaults])
    (cond [(or (null? names) (null? defaults))
           #f]
          [(eq? (car defaults) #f) ;; mandatory
           (loop (cdr kws) (cdr names) (cdr defaults))]
          [else ;; found optional
           (find-mandatory (cdr kws) (cdr names) (cdr defaults))])))
