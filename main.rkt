#lang racket/base

(require "define.rkt"
         (submod "formals.rkt" no-value-mod))

(provide (rename-out [lambda2 lambda]
                     [define2 define])
         no-value
         no-value?)

(module+ test
  (require rackunit))
