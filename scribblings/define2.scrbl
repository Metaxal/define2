#lang scribble/manual
@(require racket/sandbox
          scribble/example
          (for-label define2
                     (rename-in racket/base
                                [lambda rkt:lambda]
                                [define rkt:define])))

@;{(define-syntax-rule (racket arg)
   (racketmodname arg #:indirect))}

@(define my-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base '(require define2 racket/list))))

@title{define2}
@author{Laurent Orseau}

@defmodule[define2]

@margin-note{There may be incompatibility with code that uses keywords like @racket[#:!]
 and @racket[#:?].}
The @racketid[define2] collection redefines @racket[lambda] and @racket[define] in a
(almost entirely) backward compatible way to provide the following functionalities:
@itemlist[
 @item{a shortcut definition for keyword arguments to avoid the ubiquitous @racket[#:some-arg some-arg]
  repetition,}
 @item{a pass-through mechanism for optional keyword arguments to simplify the
       definitions of 'wrapper' functions.}]


@examples[
 #:eval my-eval

 @code:comment{Mandatory #:! argument:}
 (define (make-fruits fruit #:! number)
   (make-list number fruit))
 
 @code:line[(make-fruits 'apple #:number 4) @code:comment{Notice the keyword name}]

 @code:comment{ }
 @code:comment{Optional #:? argument:}

 (define (make-fruits2 fruit #:? [number 3])
   (make-list number fruit))

 (make-fruits2 'pear)
 (make-fruits2 'pear #:number 4)

 @code:comment{ }
 @code:comment{Pass-through #:? argument:}
 
 @code:comment{Let's write a 'wrapper' function for `make-fruits2`}
 @code:comment{that gives `fruit` a default value,}
 @code:comment{but keeps the default value for `number`—whatever value this is.}
 (define (make-fruits3 #:? [fruit 'banana] #:? number)
   (make-fruits2 fruit #:number number))

 (make-fruits3)
 (make-fruits3 #:fruit 'apple)
 (make-fruits3 #:number 2)
 ]

@deftogether[(@defthing[no-value symbol?]{}
               @defproc[(no-value? [x any/c]) boolean?]{})]{
 @racket[no-value] is an uninterned symbol representing the default value of pass-through arguments.
 @racket[no-value] does not normally need to be used, but is provided
 for clarity and possibly for user enhancements.
}

@defform[(lambda arg body ...)
            #:grammar
            ([arg
              id
              (pos-arg ...
               opt-pos-arg ...
               kw-arg ...
               . maybe-rest)]
             [pos-arg id]
             [opt-pos-arg [id expr]]
             [kw-arg
              (code:line #:! id)
              (code:line #:? id)
              (code:line #:? [id expr])
              (code:line keyword id)
              (code:line keyword [id expr])]
             [maybe-rest (code:line) (code:line id)])]{
Like racket's lambda (see @racket[rkt:lambda])
with support for @racket[#:!] mandatory keyword arguments and @racket[#:?] optional keyword arguments.

 An argument of the form @racket[#:! name] is equivalent to @racket[#:name name].
An argument of the form @racket[#:? [name val]] is equivalent to @racket[#:name [name val]]
 but binds @racket[name] to @racket[val] only if @racket[name] is @racket[no-value].
An argument of the form @racket[#:? name] is equivalent to @racket[#:name [name no-value]].
}

@defform[(define ....)]{
Like racket's define (see @racket[rkt:define]),
 but uses @racket[lambda] from @racketmodname[define2].
 Also supports the curried form.
}


