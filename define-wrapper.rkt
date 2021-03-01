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
  ; Possibly: merge the two into a temporary list and parse them again with arguments+rest
  #:with new-body (if (attribute call-wrapped)
                    #'((let ([call-wrapped (λ () (wrapped . wrapped-args.call-args))])
                        body ...)) ; use a syntax-rule instead?
                    #'(body ... (wrapped . wrapped-args.call-args)))
  ; debug: add a ' before (define ...) to see the generated code
  ;'
  (define (fun (~@ . wrapped-args.header) (~@ . fun-args.header))
     (let* ((~@ . wrapped-args.binders)
            (~@ . fun-args.binders))
       (~@ . new-body))))
