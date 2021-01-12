#lang racket
(require define2
         syntax/parse/define
         (for-syntax define2/formals))

(provide define-wrapper)

;; The additional arguments of fun-args cannot be positional, as they may
;; be ambiguous w.r.t. wrapped.
(define-simple-macro (define-wrapper (fun [wrapped . wrapped-args:arguments+rest]
                                          fun-args:splicing-arguments)
                       (~optional (~seq #:call-wrapped call-wrapped))
                       body ...)
  #:fail-when (ormap (λ (n kw) (and (not (syntax-e kw)) n))
                     (syntax->list #'fun-args.names)
                     (syntax->list #'fun-args.kws))
              "positional arguments not allowed in additional arguments"
  ; TODO: check duplicate keywords in wrapped+fun-args
  #:with new-body (if (attribute call-wrapped)
                    #'((let ([call-wrapped (λ () (wrapped . wrapped-args.call-args))])
                        body ...)) ; use a syntax-rule instead?
                    #'(body ... (wrapped . wrapped-args.call-args)))
  ; debug: add a ' before (define ...) to see the generated code
  ;'
  (define (fun (~@ . wrapped-args.header) (~@ . fun-args.header))
     (let ((~@ . wrapped-args.binders)
           (~@ . fun-args.binders))
       (~@ . new-body))))

(module+ test
  (require rackunit)

  (define (foo #:! a #:? [b 3] #:? [c 2])
    (values a b c))

  (define-wrapper (bar [foo #:? [a 1] #:? b #:? [c 'cc]]))

  (define-wrapper (baz [foo #:! a #:? b #:? [c 'cc]]
                       #:! d #:? [e 'e])
    #:call-wrapped call-foo
    (define f e)
    (set! a (+ a d))
    (define-values (res-a res-b res-c) (call-foo))
    (list res-a res-b f))

  (check-equal? (call-with-values bar list) '(1 3 cc))
  (check-equal? (call-with-values (λ () (bar #:a 'a #:b 'b)) list)
                '(a b cc))

  (check-equal? (baz #:a 10 #:d 100) '(110 3 e))

  ;; positional arguments not allowed:
  #;(define-wrapper (fuzz [foo #:! a #:? b #:? [c 'cc]]
                          x))

  #;
  (define-wrapper (my-plot (plot
                            ; pass-through arguments to plot
                            renderer-tree
                            #:? [x-min 0]
                            #:? x-max
                            #:? [y-min 0]
                            #:? y-max
                            #:? width
                            #:? height
                            #:? title
                            #:? x-label
                            #:? y-label
                            #:? legend-anchor
                            #:? out-file
                            #:? out-kind)))
  )