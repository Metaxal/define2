#lang racket/base
(require (for-syntax "formals.rkt"
                     racket/base
                     syntax/parse
                     racket/syntax)
         racket/format
         racket/list)

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
  (print-reader-abbreviations #true)

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
  
  (define (call/check name-sym mand-pos opt-pos mand-kws opt-kws rest-id proc-id call-stx)
    (with-syntax ([proc-id proc-id])
      (syntax-parse call-stx
        #:context call-stx
        [(_ call-arg ...)
         #:cut ; this should only succeed, otherwise syntax-parse will try the next branch
         #:do [(define n-mand-pos (length mand-pos))
               (define n-opt-pos (length opt-pos))
               (define-values (call-vals call-kws-stx)
                 (for/fold ([vals '()] [kws '()] #:result (values (reverse vals) (reverse kws)))
                           ([arg (in-list (syntax->list #'(call-arg ...)))])
                   (if (keyword? (syntax-e arg))
                     (values vals (cons arg kws))
                     (values (cons arg vals) kws))))
               (define call-kws (map syntax-e call-kws-stx))
               ; number of positional arguments
               (define call-n-pos (- (length call-vals) (length call-kws-stx)))
               #;(writeln (list 'call: call-vals call-kws call-n-pos))
               
               (define header (cons name-sym (append mand-pos
                                                     (if (null? opt-pos)
                                                       '()
                                                       (list opt-pos))
                                                     mand-kws
                                                     (if (null? opt-kws)
                                                       '()
                                                       (list opt-kws))
                                                     (or rest-id '()))))]

         #:fail-when
         (and (< call-n-pos n-mand-pos)
              call-stx)
         (format "missing mandatory positional arguments\n  header: ~a" header)

         #:fail-when
         (and (not rest-id)
              (> call-n-pos (+ (length mand-pos) (length opt-pos)))
              call-stx)
         (format "too many positional arguments\n  header: ~a" header)
         
         #:fail-when
         (for/or ([kw (in-list mand-kws)])
           (and (not (memq kw call-kws))
                call-stx))
         (format "missing keywords\n  header: ~a" header)
              
         #:fail-when
         (for/or ([kw (in-list call-kws-stx)])
           (define k (syntax-e kw))
           (and (not (memq k mand-kws))
                (not (memq k opt-kws))
                kw))
         (format "unknown keyword\n  header: ~a" header)
         
         #'(proc-id call-arg ...)]
             
        [_ #'proc-id]))))

(begin-for-syntax
  ;; Shared helper for define2's function-definition clauses.
  ;; fail-case? : boolean — whether #:fail-case was present
  (define (make-define2-body stx name-stx args-stx body-stxs fail-case?)
    (syntax-parse args-stx
      #:context stx
      [args:arguments+rest
       (define-values (mand-pos opt-pos mand-kws opt-kws rest-id)
         (arg-seqs->arg-lists (syntax->datum #'args.names)
                              (syntax->datum #'args.kws)
                              (syntax->datum (attribute args.defaults?))))
       (define name-sym (syntax-e name-stx))
       (with-syntax ([name name-stx]
                     [(body ...) body-stxs]
                     [(arg-name ...) #'args.names]
                     [(saved-id ...) (generate-temporaries #'args.names)]
                     [proc-id (generate-temporary)])
         #`(begin
             (define proc-id
               #,(syntax-property
                  (if fail-case?
                    #`(lambda2/context #,stx args
                        (let ([saved-id arg-name] ...)
                          (with-handlers
                              ([exn:fail?
                                (λ (e)
                                  (eprintf "Reproducible failure case:\n")
                                  (define prog
                                    (parameterize ([print-as-expression #true])
                                      (fail-case-call->string 'name
                                                              '#,(syntax->datum #'args.kws)
                                                              (list saved-id ...))))
                                  (displayln prog (current-error-port))
                                  (newline (current-error-port))
                                  (raise e))])
                            body ...)))
                    #`(lambda2/context #,stx args body ...))
                  'inferred-name
                  name-sym))
             (define-syntax (name call-stx)
               (call/check '#,name-sym '#,mand-pos '#,opt-pos '#,mand-kws '#,opt-kws '#,rest-id
                           #'proc-id
                           call-stx))))])))

(define-syntax (define2 stx)
  (syntax-parse stx
    [(_ identifier:id expr:expr)
     #'(define identifier expr)]

    #;
    [(_ (name:id . args:arguments+rest) body ...+)
     #'(define name
         (lambda2/context #,stx args body ...))]

    [(_ (name:id . args) #:fail-case body ...+)
     (make-define2-body stx #'name #'args #'(body ...) #true)]

    [(_ (name:id . args) body ...+)
     (make-define2-body stx #'name #'args #'(body ...) #false)]

    [(_ (header . args) body ...+)
     #`(define2 header
         (lambda2/context #,stx args body ...))]))

;; Builds a string representing a reproducible call expression for #:fail-case.
;; name : symbol?
;; kws : (listof (or/c keyword? #f)) — one per arg (excluding rest), #f for positional
;; vals : (listof any/c) — the saved values, same length as kws + possibly one more for rest
(define (fail-case-call->string name kws vals)
  (define n-named (length kws))
  (define named-vals (take vals n-named))
  (define rest-vals (if (> (length vals) n-named)
                      (last vals) ; rest argument is the last value
                      '()))
  (~a
   `(,name
     ,@(for/list ([kw (in-list kws)]
                  [v (in-list named-vals)])
         (if kw
           (~a kw " " (~v v))
           (~v v)))
     ,@(map ~v rest-vals))))
