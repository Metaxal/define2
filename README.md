define2
=======

Provide new definitions of `lambda` and `define` that simplify keyword arguments
and designing wrapper functions.

The keywords `#:!` for mandatory keyword arguments
and `#:?` for optional keyword arguments
are special keywords.
For example:
```racket
(define (foo #:! an-argument #:? [some-other-argument 3])
  ...)
```
is equivalent to
```racket
(define (foo #:an-argument an-argument #:some-other-argument [some-other-argument 3])
  ...)
```
In both cases above, a call to `foo` is written
```racket
(foo #:an-argument ... #:some-other-argument ...)
```

Furthermore (and that was the original intent of this package),
designing wrapper functions is greatly simplified:
```racket
; First let's define a function with an optional keyword that has a default value,
; and one mandatory argument:
(define (foo #:? [x 3] #:! y)
  (+ x y))

; Now let's define a wrapper function `foo2` that merely calls `foo` but specifies 
; a value for `y` while keeping the default value for `x`—whichever value this is:
(define (foo2 #:? x)
  (foo #:x x #:y 1))

(foo2) ; 3
(foo2 #:x 0) ; 1
```




