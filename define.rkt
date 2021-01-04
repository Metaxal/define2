#lang racket/base
(require (for-syntax "formals.rkt"
                     racket/base
                     syntax/parse))

(provide lambda2 define2)

;; The `formals` are parsed in this macro, and the #:context ensures that
;; the reported name is correct
(define-syntax (lambda2/derived stx)
  (syntax-parse stx
    #:context (syntax-parse stx [(_ orig-stx rst ...) #'orig-stx])
    [(_ _orig-stx fmls:arguments body ...+)
     #'(lambda fmls.header
         (let fmls.binders
           body ...))]))

(define-syntax (lambda2 stx)
  (syntax-parse stx
    [(_ args ...)
     #`(lambda2/derived #,stx args ...)]))

(define-syntax (define2 stx)
  (syntax-parse stx
    #:context stx
    [(_ identifier:id expr:expr)
     #'(define identifier expr)]
    [(_ ((header ...) args ...) body ...+)
     #`(define2 (header ...)
         (lambda2/derived #,stx (args ...) body ...))]
    [(_ (name:id args ...) body ...+)
     #`(define name
         (lambda2/derived #,stx (args ...) body ...))]))

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
  (define2 (foo #:x [x 3] #:y y)
    (list x y))
  
)