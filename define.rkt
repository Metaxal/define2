#lang racket/base
(require (for-syntax "formals.rkt"
                     racket/base
                     syntax/parse
                     racket/syntax))

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

(begin-for-syntax
  ;; Takes arguments attributes as parsed by arguments+rest and returns
  ;; * the list of mandatory positional identifiers
  ;; * the list of optional positional identifiers
  ;; * the list of mandatory keyword identifiers
  ;; * the list of optional keyword identifiers
  ;; * rest-id or #false
  ;;
  ;; names can have one more value than kws and defaults because it may contain rest-id.
  ;; names : (listof symbol?)
  ;; kws : (listof keyword?)
  ;; defaults : (listof boolean?)
  (define (arg-seqs->arg-lists names kws defaults?)
    (let loop (#;[:-O] ; ugly indentation trick
               [names names] [kws kws] [defaults? defaults?] 
               [mand-pos '()] [opt-pos '()] [mand-kws '()] [opt-kws '()])
      (cond
        [(null? kws)
         (values (reverse mand-pos)
                 (reverse  opt-pos)
                 (reverse mand-kws)
                 (reverse  opt-kws)
                 (and (not (null? names)) (car names)))]
        [else
         (define d (car defaults?))
         (define k (car kws))
         (define n (car names))
         (loop (cdr names) (cdr kws) (cdr defaults?)
               (if (and (not k) (not d)) (cons n mand-pos) mand-pos)
               (if (and (not k)      d)  (cons n  opt-pos)  opt-pos)
               (if (and      k  (not d)) (cons k mand-kws) mand-kws)
               (if (and      k       d)  (cons k  opt-kws)  opt-kws))])))
  
  (define (call/check mand-pos opt-pos mand-kws opt-kws rest-id proc-id call-stx)
    (with-syntax ([proc-id proc-id])
      (syntax-parse call-stx
        [(_ call-arg ...)
         #:cut ; this should only succeed, otherwise syntax-parse will try the next branch
         #:do [(define-values (call-vals call-kws-stx)
                 (for/fold ([vals '()] [kws '()] #:result (values (reverse vals) (reverse kws)))
                           ([arg (in-list (syntax->list #'(call-arg ...)))])
                   (if (keyword? (syntax-e arg))
                     (values vals (cons arg kws))
                     (values (cons arg vals) kws))))
               (define call-kws (map syntax-e call-kws-stx))
               ; number of positional arguments
               (define call-n-pos (- (length call-vals) (length call-kws-stx)))
               #;(writeln (list 'call: call-vals call-kws call-n-pos))]

         #:fail-when
         (and (< call-n-pos (length mand-pos))
              call-stx)
         (format "missing mandatory positional arguments")
         
         #:fail-when
         (for/or ([kw (in-list mand-kws)])
           (and (not (memq kw call-kws))
                call-stx))
         (format "missing keywords\n mandatory: ~a\n optional: ~a\n" mand-kws opt-kws)
              
         #:fail-when
         (for/or ([kw (in-list call-kws-stx)])
           (define k (syntax-e kw))
           (and (not (memq k mand-kws))
                (not (memq k opt-kws))
                kw))
         (format "unknown keyword\n mandatory: ~a\n optional: ~a\n" mand-kws opt-kws)
         
         #'(proc-id call-arg ...)]
             
        [_ #'proc-id]))))

;; TODO:
;; * make these defines instead of define-syntax so as to test exceptions?

(define-syntax (define2 stx)
  (syntax-parse stx
    [(_ identifier:id expr:expr)
     #'(define identifier expr)]

    #;
    [(_ (name:id . args:arguments+rest) body ...+)
     #'(define name
         (lambda2/context #,stx args body ...))]
     ; NOT READY YET
    [(_ (name:id . args:arguments+rest) body ...+)
     ; Note that arguments+rest parses args here and in lambda2. Not ideal.
     ;#:attr defaults? 
     #:with proc-id (generate-temporary)
     #:do [(define-values (mand-pos opt-pos mand-kws opt-kws rest-id)
             (arg-seqs->arg-lists (syntax->datum #'args.names)
                                  (syntax->datum #'args.kws)
                                  (syntax->datum (attribute args.defaults?))))
           #;(writeln (list mand-pos opt-pos mand-kws opt-kws rest-id))]
     #`(begin
         (define proc-id
           ;; Make sure to print the correct name for the procedure.
           ;; https://docs.racket-lang.org/reference/syntax-model.html#(part._infernames)
           #,(syntax-property #'(lambda2/context #,stx args body ...)
                               'inferred-name
                               (syntax-e #'name)))
         (define-syntax (name call-stx)
           (call/check '#,mand-pos '#,opt-pos '#,mand-kws '#,opt-kws '#,rest-id
                       #'proc-id
                       call-stx)))]

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
