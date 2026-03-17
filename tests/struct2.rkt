#lang racket/base

(require "../struct2.rkt"
         "../define.rkt"
         rackunit)

;; -----------------------------------------------------------
;; Group 1: Basic Structs (No Inheritance)
;; -----------------------------------------------------------

(test-case "Basic struct with defaults and keywords"
  (struct2 Point ([x 0] [y 0]) #:transparent)
  
  (define p1 (Point/kw))
  (check-equal? p1 (Point 0 0) "Default values should be used")
  
  (define p2 (Point/kw #:x 10 #:y 20))
  (check-equal? p2 (Point 10 20) "Keywords should override defaults")
  
  (define p3 (Point/kw #:y 5))
  (check-equal? p3 (Point 0 5) "Partial keywords should work"))

(test-case "Mutability checks"
  (struct2 Box ([val #:mutable]) #:transparent)
  
  (define b (Box/kw #:val 10))
  (check-equal? (Box-val b) 10)
  
  (set-Box-val! b 99)
  (check-equal? (Box-val b) 99 "Field should be mutable"))

;; -----------------------------------------------------------
;; Group 2: Inheritance (The "Wrapper" Pattern)
;; -----------------------------------------------------------

(test-case "Inheritance with wrapper pattern"
  (struct2 A (val-a) #:transparent)
  (struct2 B A ([val-b 100]) #:transparent)
  
  (define a-instance (A/kw #:val-a "base"))
  
  ;; Create B using A
  (define b-instance (B/kw a-instance #:val-b 200))
  
  ;; Checks
  (check-true (B? b-instance) "Result should be instance of B")
  (check-true (A? b-instance) "Result should also be instance of A (subtyping)")
  (check-equal? (A-val-a b-instance) "base" "Should preserve parent field")
  (check-equal? (B-val-b b-instance) 200 "Should set new field"))

(test-case "Inheritance using parent default values"
  (struct2 Base ([x 1]) #:transparent)
  (struct2 Sub Base (y) #:transparent)
  
  (define base (Base/kw)) ;; x is 1
  (define sub (Sub/kw base #:y 2))
  
  (check-equal? (Base-x sub) 1 "Inherited value is preserved"))

;; -----------------------------------------------------------
;; Group 3: Fenders (Validation)
;; -----------------------------------------------------------

(test-case "Fenders for validation"
  (struct2 PositivePoint ([x 0] [y 0])
           #:fender (unless (and (>= x 0) (>= y 0))
                      (error "Coordinates must be positive"))
           #:transparent)
  
  ;; Should pass
  (check-not-exn (lambda () (PositivePoint/kw #:x 10 #:y 10)))
  
  ;; Should fail
  (check-exn exn:fail? (lambda () (PositivePoint/kw #:x -1 #:y 5))))

(test-case "Fenders with Inheritance (Accessing Parent via 'super')"
  (struct2 Parent (name) #:transparent)
  (struct2 Child Parent (age)
           ;; Using 'super' to check properties of the parent
           #:fender (when (equal? (Parent-name super) "Forbidden")
                      (error "Cannot inherit from Forbidden parent"))
           #:transparent)
  
  (define p-ok (Parent/kw #:name "Ok"))
  (define p-bad (Parent/kw #:name "Forbidden"))
  
  (check-not-exn (lambda () (Child/kw p-ok #:age 10)))
  (check-exn exn:fail? (lambda () (Child/kw p-bad #:age 10))))

;; -----------------------------------------------------------
;; Group 4: Error Handling / Edge Cases
;; -----------------------------------------------------------

(test-case "Error: Passing wrong parent type"
  (struct2 P1 () #:transparent)
  (struct2 P2 () #:transparent)
  (struct2 C P1 () #:transparent)
  
  (define p2 (P2/kw))
  
  ;; Expect error because we passed P2 instance, but C expects P1
  (check-exn exn:fail:contract? 
             (lambda () (C/kw p2))))

(test-case "Error: Opaque parent (Cannot wrap)"
  ;; P-Opaque is NOT transparent
  (struct2 P-Opaque (x))
  (struct2 C-Wrapper P-Opaque (y) #:transparent)
  
  (define p (P-Opaque/kw #:x 1))
  
  ;; This should fail because `struct->list` returns #f for opaque structs.
  (check-exn exn:fail? 
             (lambda () (C-Wrapper/kw p #:y 2))))

(let ()
  (struct2 A (a b) #:transparent)
  (struct2 B A (c d) #:transparent)
  (define b1 (B 'a 'b 'c 'd))
  ;; Only the A-elements of b1 should be copied:
  (check-equal? (B/kw b1 #:c 'cc #:d 'dd)
                (B 'a 'b 'cc 'dd)))

;; -----------------------------------------------------------
;; Group 5: No fender with inheritance (super not referenced)
;; -----------------------------------------------------------

(test-case "Inheritance without fender"
  (struct2 X (a) #:transparent)
  (struct2 Y X (b) #:transparent)
  
  (define x (X/kw #:a 1))
  (define y (Y/kw x #:b 2))
  (check-equal? (X-a y) 1)
  (check-equal? (Y-b y) 2))

;; -----------------------------------------------------------
;; Group 6: Mutable field with default + inheritance
;; -----------------------------------------------------------

(test-case "Mutable field with default in child"
  (struct2 Base2 (x) #:transparent)
  (struct2 Child2 Base2 ([y 0 #:mutable]) #:transparent)
  
  (define b (Base2/kw #:x 10))
  (define c (Child2/kw b))
  (check-equal? (Child2-y c) 0)
  (set-Child2-y! c 99)
  (check-equal? (Child2-y c) 99))
