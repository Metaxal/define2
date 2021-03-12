define2
=======

Provides new but backward-compatible[*] definitions of `lambda` and `define` that simplify keyword arguments
and designing wrapper functions. 
Also features compile-time checking of the number of arguments and the keyword names in function calls.
See the [[docs](https://docs.racket-lang.org/define2/index.html)]

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
#lang racket
(require define2)

; First let's define a function with an optional keyword that has a default value,
; and one mandatory argument:
(define (foo #:? [x 3] #:! y)
  (+ x y))

; Now let's define a wrapper function `foo2` that merely calls `foo` but specifies 
; a value for `y` while keeping the default value for `x`—whichever value this is:
(define (foo2 #:? x)
  (foo #:x x #:y 1))

(foo2) ; 4
(foo2 #:x 0) ; 1
```


[*] Unless the original code uses one of the special keywords `#:!` or the `#:?`. Note that `(define (foo #:x xx) ...)` is still a valid definition with `define2`.

## Wrappers

Have you ever tried to write a simpler wrapper function for a function with several optional arguments, but eventually quitted because it was so ugly?
Say, you really just wanted to set `x-min` to 0 by default instead of `#f` in [`plot`](https://docs.racket-lang.org/plot/plotting.html?q=plot#%28def._%28%28lib._plot%2Fmain..rkt%29._plot%29%29)...

In plain Racket, you would have to write it like this:
```racket
(define (my-plot renderer-tree
                 #:x-min [x-min 0]
                 #:x-max [x-max #f]
                 #:y-min [y-min #f]
                 #:y-max [y-max #f]
                 #:width [width (plot-width)]
                 #:height [height (plot-height)]
                 #:title [title (plot-title)]
                 #:x-label [x-label (plot-x-label)]
                 #:y-label [y-label (plot-y-label)]
                 #:legend-anchor [legend-anchor (plot-legend-anchor)]
                 #:out-file [out-file #f]
                 #:out-kind [out-kind 'auto]
                 )
  (plot renderer-tree
        #:x-min x-min
        #:x-max x-max
        #:y-min y-min
        #:y-max y-max
        #:width width
        #:height height
        #:title title
        #:x-label x-label
        #:y-label y-label
        #:legend-anchor legend-anchor
        #:out-file out-file
        #:out-kind out-kind))
```
Not only does this require you to know all the default values, but every optional name is written 4 times... That's really not worth it for a mere `#:xmin [x-min 0]`!

But suppose that `plot` had been defined using `define2` instead with optional arguments with `#:?`, then we would just have to use the new `define-wrapper` form and write
```racket
(define-wrapper (my-plot (plot renderer-tree
                               #:? [x-min 0]
                               #:? x-max
                               #:? y-min
                               #:? y-max
                               #:? width
                               #:? height
                               #:? title
                               #:? x-label
                               #:? y-label
                               #:? legend-anchor
                               #:? out-file
                               #:? out-kind)))
```
which is certainly more bearable, and doesn't require to know the actual default values.
See some more options in the [docs](https://docs.racket-lang.org/define2/index.html).


