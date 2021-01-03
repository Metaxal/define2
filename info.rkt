#lang info
(define collection "define2")
(define deps '("base"))
(define build-deps '("sandbox-lib"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/define2.scrbl" ())))
(define pkg-desc "Like lambda and define, but simplifies keyword arguments and wrapper functions")
(define version "0.0")
(define pkg-authors '(orseau))
