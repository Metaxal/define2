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
                (define2 (foo #:! a #:? a) #f))

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo #:? [a 3] #:? a) #f))

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo a #:! a) #f))

(check-syntax-exn #rx"define2: duplicate argument identifier"
                (define2 (foo #:! a #:b a) #f))

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
