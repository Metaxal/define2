#lang scribble/manual
@(require racket/sandbox
          scribble/example
          (for-label define2))

@(module def-racket racket/base
   (require scribble/manual
            (for-label racket/base))
   (define rkt-define @racket[define])
   (define rkt-lambda @racket[lambda])
   (define rkt-λ @racket[λ])
   (provide (all-defined-out)))
@(require 'def-racket)

@(define my-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base '(require define2 racket/list))))

@title{define2}
@author{Laurent Orseau}

@defmodule[define2]

@margin-note{There may be incompatibility with code that uses the keywords @racket[#:!]
 and @racket[#:?].}
The @racketmodname[define2] collection redefines @|rkt-lambda| and @|rkt-define| in a
(almost entirely) backward compatible way to provide the following functionalities:
@itemlist[
 @item{a shortcut definition for keyword arguments to avoid the ubiquitous
  @racket[#:some-arg some-arg] repetition,}
 @item{a pass-through mechanism for optional keyword arguments to propagate
  default values without having to know them.}]


@examples[
 #:eval my-eval #:label @elem{@bold{Example:} Mandatory @racket[#:!] argument}
 
 (define (make-fruits fruit #:! number)
   (make-list number fruit))
 
 @code:line[(make-fruits 'apple #:number 4) @code:comment{Notice the keyword name}]
 ]

@examples[
 #:eval my-eval #:label @elem{@bold{Example:} Optional @racket[#:?] argument}
 
 (define (make-fruits2 fruit #:? [number 3])
   (make-list number fruit))

 (make-fruits2 'pear)
 (make-fruits2 'pear #:number 4)
 ]

@examples[
 #:eval my-eval #:label @elem{@bold{Example:} Pass-through @racket[#:?] argument}
 
 @code:comment{Let's write a function that uses `make-fruits2` without changing}
 @code:comment{the default value for `number`—whatever value this is.}
 (define (make-two-fruits fruit1 fruit2 #:? number)
   (list (make-fruits2 fruit1 #:number number)
         (make-fruits2 fruit2 #:number number)))

 (make-two-fruits 'apple 'banana)
 (make-two-fruits 'apple 'banana #:number 2)
 ]

@deftogether[(@defthing[no-value symbol?]{}
               @defproc[(no-value? [x any/c]) boolean?]{})]{
 @racket[no-value] is an
  @techlink[#:key "uninterned"
            #:doc '(lib "scribblings/reference/reference.scrbl")
            ]{uninterned symbol}
 representing the default value of pass-through arguments.
 @racket[no-value] does not normally need to be used, but is provided
 for clarity and possibly for user enhancements.
}

@deftogether[
 (@defform[(lambda args body ...+)]{}
   @defform[(λ args body ...+)
            #:grammar
            ([args
              (pos-id ... [opt-id opt-expr] ... kw-arg ...)
              (pos-id ... [opt-id opt-expr] ... kw-arg ... . rest-id)
              rest-id]
             [kw-arg
              (code:line #:! id)
              (code:line #:? id)
              (code:line #:? [id expr])
              (code:line keyword id)
              (code:line keyword [id expr])])]{})]{
Like @|rkt-lambda| and @|rkt-λ| from @racketmodname[racket/base], but
with support for @racket[#:!] mandatory keyword arguments and @racket[#:?] optional keyword arguments.

 An argument of the form @racket[#:! name] is equivalent to @racket[#:name name].
An argument of the form @racket[#:? [name val]] is equivalent to @racket[#:name [name val]]
 but binds @racket[name] to @racket[val] only if @racket[name] is @racket[no-value].
An argument of the form @racket[#:? name] is equivalent to @racket[#:name [name no-value]].

This means in particular that @racket[(lambda (#:a the-a #:! a) ...)] is a syntax error
(duplicate argument keyword),
as well as @racket[(lambda (#:a the-a #:! the-a) ...)] (duplicate argument identifier).
}

@defform*[
 ((define id expr)
  (define (head args) body ...+))]{
Like @|rkt-define| from @racketmodname[racket/base],
 but uses @racket[lambda] from @racketmodname[define2] instead.
 Also supports the curried form.
}

@section{Acknowledgements}

Thanks to
Sorawee Porncharoenwase,
Jack Firth,
Jens-Axel Soegaard,
Sam Tobin-Hochstadt,
Greg Hendershott,
Bogdan Popa,
and Leif Anderson
for their help.