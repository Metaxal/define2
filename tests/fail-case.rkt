#lang racket/base

(require "../define.rkt"
         rackunit
         racket/port
         racket/fixnum)

;; ==================== Basic #:fail-case ====================

;; Without #:fail-case, define2 works as normal
(let ()
  (define2 (f x) (+ x 1))
  (check-equal? (f 3) 4))

;; With #:fail-case but no error, body returns normally
(let ()
  (define2 (f x)
    #:fail-case
    (+ x 1))
  (check-equal? (f 3) 4))

;; With #:fail-case, error prints reproducible call
(let ()
  (define2 (f x)
    #:fail-case
    (error "boom!"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"boom!"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f 42))))
      (get-output-string out)))

  (check-regexp-match #rx"Reproducible failure case:" err-output)
  (check-regexp-match #rx"\\(f 42\\)" err-output))


;; ==================== Keyword arguments ====================

;; Mandatory keyword
(let ()
  (define2 (f #:x x)
    #:fail-case
    (error "boo!"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"boo!"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f #:x "hello"))))
      (get-output-string out)))

  (check-regexp-match #rx"#:x \"hello\"" err-output))

;; Optional keyword with default
(let ()
  (define2 (f x #:y [y 10])
    #:fail-case
    (when (= x y) (error "equal!"))
    (list x y))

  ;; No error case
  (check-equal? (f 1 #:y 2) '(1 2))

  ;; Error case
  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"equal!"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f 5 #:y 5))))
      (get-output-string out)))
  (check-regexp-match #rx"\\(f 5 #:y 5\\)" err-output))

;; Mixed positional + keyword
(let ()
  (define2 (f x #:y y #:z [z '()])
    #:fail-case
    (error "fail"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"fail"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f 1 #:y 2 #:z 3))))
      (get-output-string out)))

  (check-regexp-match #rx"\\(f 1 #:y 2 #:z 3\\)" err-output))

;; ==================== Rest arguments ====================

;; Positional + rest
(let ()
  (define2 (f x . rest)
    #:fail-case
    (error "rest-fail"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"rest-fail"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f 1 2 3 4))))
      (get-output-string out)))

  (check-regexp-match #rx"\\(f 1 2 3 4\\)" err-output))

;; Keyword + rest
(let ()
  (define2 (f #:x x . rest)
    #:fail-case
    (error "kw-rest-fail"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"kw-rest-fail"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f #:x 'a 'b 'c))))
      (get-output-string out)))

  (check-regexp-match #rx"#:x 'a" err-output)
  (check-regexp-match #rx"'b 'c\\)" err-output))

;; Empty rest
(let ()
  (define2 (f x . rest)
    #:fail-case
    (error "empty-rest"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"empty-rest"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f 42))))
      (get-output-string out)))

  (check-regexp-match #rx"\\(f 42\\)" err-output))

;; ==================== Mutated arguments ====================

;; Values are saved before body executes
(let ()
  (define2 (f x)
    #:fail-case
    (set! x (+ x 100))
    (error "mutated!"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"mutated!"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f 5))))
      (get-output-string out)))

  ;; Should print original value 5, not mutated 105
  (check-regexp-match #rx"\\(f 5\\)" err-output))


;; ==================== Non-readable values ====================

;; Structs, fxvectors, etc. — ~v prints them in a readable way when possible
(let ()
  (struct my-struct (a b) #:prefab)

  (define2 (f x #:y y #:z [z '()])
    #:fail-case
    (error "struct!"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"struct!"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f (my-struct (vector 'a (fxvector 2)) (fxvector 1 2))
                        #:y 7
                        #:z '()))))
      (get-output-string out)))

  (check-regexp-match #rx"Reproducible failure case:" err-output)
  (check-regexp-match #rx"#:y 7" err-output)
  ;; The struct should be printed somehow (by ~v)
  (check-regexp-match #rx"my-struct" err-output))

;; Quoted list prints with quote
(let ()
  (define2 (f x)
    #:fail-case
    (error "q!"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"q!"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f '(a b c)))))
      (get-output-string out)))

  (check-regexp-match #rx"'\\(a b c\\)" err-output))


;; ==================== No-args function ====================

(let ()
  (define2 (f)
    #:fail-case
    (error "no-args!"))

  (define err-output
    (let ([out (open-output-string)])
      (check-exn #rx"no-args!"
                 (λ ()
                   (parameterize ([current-error-port out])
                     (f))))
      (get-output-string out)))

  (check-regexp-match #rx"\\(f\\)" err-output))
