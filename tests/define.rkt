#lang racket/base

(require define2/define
         syntax/macro-testing
         syntax/parse/define
         rackunit)

(define-simple-macro (check-syntax-exn msg form)
  (check-exn msg (λ () (convert-syntax-error (let () form (void))))))

(check-syntax-exn #rx"define2: expected more terms"
                  (define2 x))

(check-syntax-exn #rx"define2: unexpected term"
                  (define2 x y z))

(check-syntax-exn #rx"define2: duplicate argument keyword"
                (define2 (foo #:! a #:a b) #f))

(check-syntax-exn #rx"define2: duplicate argument keyword"
                (define2 (foo #:a a #:a b) #f))

(check-syntax-exn #rx"define2: duplicate argument keyword"
                (define2 (foo #:? a #:a b) #f))

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo #:! a . a) #f)) ; rest-id

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo a . a) #f)) ; rest-id

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo #:! a #:? a) #f))

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo #:? [a 3] #:? a) #f))

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo a #:! a) #f))

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo #:! a #:b a) #f))

(check-syntax-exn #rx"define2: mandatory positional argument after optional positional argument"
                  (define2 (foo a [b 1] c) #f))

;; Should not raise a default-value missing error
(check-equal? (let ()
                (define2 (foo a #:b [b 1] c) (list a b c))
                (foo 'a 'c #:b 'b))
              '(a b c))

(check-equal? (let ()
                (define2 (foo) (list 'aa))
                (foo))
              '(aa))

(check-equal? (let ()
                (define2 (foo a) (list a))
                (foo 'aa))
              '(aa))

(check-equal? (let ()
                (define2 (foo #:! a) (list a))
                (foo #:a 'aa))
              '(aa))

(check-equal? (let ()
                (define2 (foo #:? [a 'a]) (list a))
                (define2 (foo2 #:? a) (foo #:a a))
                (define2 (foo3 #:? [a 'aaa]) (foo #:a a))
                (list (foo)
                      (foo #:a 'aa)
                      (foo2)
                      (foo2 #:a 'aa)
                      (foo3)
                      (foo3 #:a 'aa)))
              '((a) (aa) (a) (aa) (aaa) (aa)))


(check-equal? (let ()
                (define2 (foo #:! a)
                  (define b (+ a 1)) ; multiple body + internal define
                  (list a b))
                (foo #:a 3))
              '(3 4))

(check-equal? (let ()
                (define2 (foo #:? [x 3] #:y [y (+ x 3)])
                  (list x y))
                (foo #:x 10))
              '(10 13))

(check-equal? (let ()
                (define2 (foo #:x [x 3] #:z z #:? [y (+ x z)])
                  (list x y z))
                (foo #:x 10 #:z 100))
              '(10 110 100))

(require (for-syntax "../formals.rkt"
                     racket/base))

(begin
  (define-simple-macro (define3 (name:id . argsr:arguments+rest)
                         body ...)
    (define (name . argsr.header)
      (let argsr.binders
        body ...)))

  ;; ERROR: arguments+rest does not preserve the order,
  ;; hence x is unbound in y (but should be bound).
  #;(check-equal? (let ()
                (define3 (foo #:x [x 3] #:? [y (+ x 3)] . rst)
                  (list x y rst))
                (foo #:x 10 'a 'b))
              '(10 13 (a b)))
  )
