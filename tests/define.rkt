#lang racket/base

(require "../define.rkt"
         "../define-wrapper.rkt"
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

(check-syntax-exn #rx"foo: missing mandatory positional argument"
                  (let ()
                    (define2 (foo aa #:? [aaa 'a]) #f)
                    (foo)))

(check-syntax-exn #rx"foo: too many positional arguments"
                  (let ()
                    (define2 (foo a [b 'bb]) #f)
                    (foo 'a 'b 'c)))

(check-syntax-exn #rx"foo: missing keyword"
                  (let ()
                    (define2 (foo #:aa aaa) #f)
                    (foo)))

(check-syntax-exn #rx"foo: missing keyword"
                  (let ()
                    (define2 (foo #:! aaa) #f)
                    (foo)))

; curried
(check-syntax-exn #rx"foo: missing keyword"
                  (let ()
                    (define2 ((foo #:! aaa) #:b b) #f)
                    (foo)))

(check-syntax-exn #rx"foo: unknown keyword"
                  (let ()
                    (define2 (foo) #f)
                    (foo #:aaa 'a)))

;; No missing keyword exception
(check-equal?
 (let ()
   (define2 (foo #:? aaa) #t)
   (foo))
 #t)
;
(check-equal?
 (let ()
   (define2 (foo #:aa aaa) #t)
   (foo #:aa #t))
 #t)

(check-equal? (let ()
                (define2 (foo a [b 'b] . rst)
                  (list a b rst))
                (list (foo 'aa) (foo 'aa 'bb) (foo 'aa 'bb 'cc 'dd)))
              '((aa b ()) (aa bb ()) (aa bb (cc dd))))

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

;; Arguments can be reused.
(check-equal? (let ()
                (define2 (foo #:? [x 3] #:y [y (+ x 3)] #:? [z (+ y 10)])
                  (list x y z))
                (foo #:x 10))
              '(10 13 23))

(check-equal? (let ()
                (define2 (foo #:x [x 3] #:z z #:? [y (+ x z)])
                  (list x y z))
                (foo #:x 10 #:z 100))
              '(10 110 100))

(let ()
  (define bar 'bb)

  (define2 (foo #:? [bar bar])
    bar)

  (define2 (foo2 #:? bar)
    (foo #:bar bar))

  ; This is a tricky one (h/t sorawee)
  ; The second `bar` in [bar bar] is actually used, as an identifier, in two different ways:
  ; The first time as a normal default argument,
  ; The second time it becomes bound to the first `bar` identifier because of templates.
  ; The answer would be to use generate temporaries
  (check-equal? (foo2) 'bb))

(check-equal?
 (let ()
   (define2 (foo [x 'xx]) ; Positional optional argument
     x)
   (define2 (bar #:? x)   ; Optional keyword argument, defaults to no-value if not provided
     (foo x))            ; Call foo with the value of x from bar
   (bar))                ; Call bar without providing #:x, so x in bar is no-value
 'xx                     ; Expected result after the fix in formals.rkt
 "Issue #4: Positional default argument not propagating from keyword call")

;; For contrast (this was already working)
(check-equal?
 (let ()
   (define2 (foo #:? [x 'xx]) ; Keyword optional argument
     x)
   (define2 (bar #:? x)     ; Optional keyword argument
     (foo #:x x))          ; Call foo with x from bar, passing as keyword
   (bar))
 'xx
 "Issue #4: Control case with keyword arguments (should already pass)")

;; Non-duplicate side effects
(let ()
  (define c 0)
  (define2 (foo #:? [bar (set! c (+ c 1))])
    (list bar bar))
  (check-equal? c 0)
  (foo #:bar 'a)
  (check-equal? c 0)
  (foo)
  (check-equal? c 1)
  (foo #:bar 'a)
  (check-equal? c 1)
  (foo)
  (check-equal? c 2))

(require (for-syntax "../formals.rkt"
                     racket/base))

(begin
  ;; A simple definer like one a user could write
  (define-simple-macro (define3 (name:id . argsr:arguments+rest)
                         body ...)
    (define (name . argsr.header)
      (let* argsr.binders
        body ...)))

  (check-equal? (let ()
                (define3 (foo #:x [x 3] #:? [y (+ x 3)] . rst)
                  (list x y rst))
                (foo #:x 10 'a 'b))
              '(10 13 (a b))))
