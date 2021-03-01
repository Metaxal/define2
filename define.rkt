#lang racket/base
(require (for-syntax "formals.rkt"
                     racket/base
                     syntax/parse))

(provide lambda2 define2)

;; The `arguments` are parsed in this macro, and the #:context ensures that
;; the reported name is correct.
(define-syntax (lambda2/context stx)
  (syntax-parse stx
    #:context (syntax-parse stx [(_ orig-stx rst ...) #'orig-stx])
    [(_ _orig-stx fmls:arguments+rest body ...+)
     #'(lambda fmls.header
         (let* fmls.binders
           body ...))]))

(define-syntax (lambda2 stx)
  (syntax-parse stx
    [(_ args ...)
     #`(lambda2/context #,stx args ...)]))

(define-syntax (define2 stx)
  (syntax-parse stx
    [(_ identifier:id expr:expr)
     #'(define identifier expr)]

    [(_ (name:id . args:arguments+rest) body ...+)
     #'(define name
         (lambda2/context #,stx args body ...))]

    #; ; not ready yet
    [(_ (name:id . args:arguments+rest) body ...+)
     ; Note that arguments+rest parses args here and in lambda2. Not ideal.
     #:attr defaults? (attribute args.defaults?)
     #`(begin
         (define name/proc
           (lambda2/context #,stx args body ...))
         (define-syntax (name call-stx)
           (syntax-parse call-stx
             [(_ call-arg (... ...))
              #:cut ; this should only succeed, otherwise syntax-parse will try the next branch
              ;
              ;#:attr mand-pos (filter values
              ;                        (map (λ (d k id) (and (not d) (not k) id))
              ;                             'defaults?
              ;                             (syntax->datum #'args.kws)
              ;                             (syntax->datum #'args.names))) ; WARNING: May contain the rest id
              ; If a keyword is optional, arg.default is #f, otherwise it's a syntax object
              ; (possibly containing #f as a default value).
              #:attr mand-kws (filter values
                                      (map (λ (d k) (and (not d) k))
                                           'defaults?
                                           (syntax->datum #'args.kws)))
              #:attr opt-kws (filter values
                                     (map (λ (d k) (and d k))
                                          'defaults?
                                          (syntax->datum #'args.kws)))
              #:fail-when
              (let ([call-kws (filter keyword? (map syntax-e (syntax->list #'(call-arg (... ...)))))])
                #;(writeln (list (attribute mand-kws) (attribute opt-kws) call-kws))
                (or
                 (for/or ([kw (in-list (attribute mand-kws))]
                          #:when (keyword? kw))
                   (and (not (memq kw call-kws))
                        call-stx))))
              (format "missing keywords\n mandatory: ~a\n optional: ~a\n"
                      (attribute mand-kws)
                      (attribute opt-kws))
              
              #:fail-when
              ; TODO: Gather only mandatory keywords
              (let ([def-kws (filter values (syntax->datum #'args.kws))])
                #;(writeln def-kws)
                (for/or ([kw (in-list (syntax->list #'(call-arg (... ...))))]
                          #:when (keyword? (syntax-e kw)))
                   (and (not (memq (syntax-e kw) def-kws))
                        kw)))
              (format "unknown keyword\n mandatory: ~a\n optional: ~a\n"
                      (attribute mand-kws)
                      (attribute opt-kws))
              #;(format "Unknown keyword for ~a" (syntax->datum #'(name . args.call-args)))

              #'(name/proc call-arg (... ...))]
             
             [_ #'name/proc])
           ))]
    
    [(_ (header . args) body ...+)
     #`(define2 header
         (lambda2/context #,stx args body ...))]))

(module+ test
  (require rackunit
           racket/dict)
  
  (define2 (my-dict-ref d k #:? [default (λ () (error "Unknown key: ~a" k))])
    (dict-ref d k default))
  (define2 ((my-curried-dict-ref k #:? default) d)
    (my-dict-ref d k #:default default))
  
  (check-equal? ((my-curried-dict-ref 'a) '((a . aa) (b . bb))) 'aa)
  (check-exn exn:fail? (λ () ((my-curried-dict-ref 'aa) '((a . aa) (b . bb)))))
  (check-equal? ((my-curried-dict-ref 'aa #:default 4) '((a . aa) (b . bb))) 4)

  ; This should not raise an argument order exception
  (let ()
    (define2 (foo #:x [x 3] #:y y)
      (list x y))
    (check-equal? (foo #:x 2 #:y 3)
                  '(2 3))
    (check-equal? (foo #:y 3)
                  '(3 3)))

  (let ()
    (define2 (foo #:? [c #f]
                  #:? [a #f]
                  . rest-args)
      (list a c rest-args))
    (check-equal? (foo #:a 1 #:c 2 3 4)
                  '(1 2 (3 4))))

  (let ()
    (define2 ((foo #:? [c #f] . rest-args1)
              #:? [a #f]
              . rest-args)
      (list a c rest-args1 rest-args))
    (check-equal? ((foo #:c 2 'x) #:a 1 3 4)
                  '(1 2 (x) (3 4))))

  (check-equal?
   ((lambda2 (#:? [c #f]
              #:? [a #f]
              . rest-args)
             (list a c rest-args))
    #:a 1 #:c 2 3)
   '(1 2 (3)))  
  )
