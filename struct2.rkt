#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         racket/struct
         racket/list
         ;; Required for static checking (arity/keywords) at call sites
         define2)

(provide struct2)

(begin-for-syntax
  (define-syntax-class struct2-field
    #:attributes (field default mutable define2-spec)
    #:description "a field definition with optional default value and mutability"
    
    ;; Case 1: Simple identifier (e.g., 'x')
    ;; Logic: No default provided -> Mandatory keyword argument (#:!)
    (pattern field:id
      #:attr default #f
      #:attr mutable #f
      #:attr define2-spec #`(#:! field))
    
    ;; Case 2: Mutable with default (e.g., '[x 10 #:mutable]')
    ;; Logic: Default provided -> Optional keyword argument (#:?)
    (pattern [field:id default:expr #:mutable]
      #:attr mutable #'#:mutable
      #:attr define2-spec #`(#:? [field default]))
    
    ;; Case 3: Mutable no default (e.g., '[x #:mutable]')
    ;; Logic: No default provided -> Mandatory keyword argument (#:!)
    (pattern [field:id #:mutable]
      #:attr default #f
      #:attr mutable #'#:mutable
      #:attr define2-spec #`(#:! field))
    
    ;; Case 4: Immutable with default (e.g., '[x 10]')
    ;; Logic: Default provided -> Optional keyword argument (#:?)
    (pattern [field:id default:expr]
      #:attr mutable #f
      #:attr define2-spec #'(#:? [field default]))))

;; ---------------------------------------------------------------------------------------------------
;; Macro: struct2
;; ---------------------------------------------------------------------------------------------------

;; Usage:
;; (struct2 name [parent] (field-spec ...) options ...)
;;
;; Enhances Racket's standard `struct` with:
;; 1. A keyword-based constructor (`<name>/kw`) using `define2` for static checking.
;; 2. Default values for fields.
;; 3. A "Wrapper" style inheritance where a parent instance is passed to the child constructor.
;; 4. A #:fender clause for validation logic before instantiation.
;;    Inside the fender of an inheriting struct, the identifier `super` is bound
;;    to the parent instance.
;;
;; Field Specifications:
;; - id                     : Required keyword argument (checked statically).
;; - [id default]           : Optional keyword argument.
;; - [id #:mutable]         : Required keyword argument (checked statically), mutable field.
;; - [id default #:mutable] : Optional keyword argument, mutable field.
;;
;; Inheritance Note:
;; Because this macro extracts fields from the parent instance using `struct->list`,
;; the parent struct MUST be transparent (or have an inspector that permits reflection).
;;
(define-syntax struct2
  (syntax-parser
    ;; Case 1: Inheritance
    ;; (struct2 Child Parent (fields ...) ...)
    [(_ name:id parent:id (field:struct2-field ...)
        {~optional {~seq #:fender fender-expr}}
        more ...)
     #:with kw-ctor (format-id #'name "~a/kw" #'name #:source #'name)
     #:with parent? (format-id #'parent "~a?" #'parent)
     ;; Introduce `super` into the fender's lexical scope via datum->syntax
     #:with super-id (if (attribute fender-expr)
                       (datum->syntax #'fender-expr 'super)
                       #'unused-super)
     #`(begin
         ;; Define the actual struct, inheriting from parent type.
         (struct name parent ({~? [field.field field.mutable] field.field} ...) more ...)
         
         ;; Define the keyword constructor using define2 syntax.
         ;; usage: (Child/kw parent-instance #:field val ...)
         (define (kw-ctor parent-expr {~@ . field.define2-spec} ...)
           
           ;; 1. Check if the parent argument is actually an instance of the parent struct
           (unless (parent? parent-expr)
             (raise-argument-error 'kw-ctor (symbol->string 'parent) parent-expr))

           ;; 2. Run user-defined validation (Fender)
           ;; `super` is bound via datum->syntax so it's visible in fender-expr.
           {~? (let ([super-id parent-expr])
                 fender-expr)}
           
           ;; 3. Extract fields from the parent instance.
           ;; Note: This relies on the parent being transparent.
           (define vparent (struct->list parent-expr #:on-opaque 'return-false))
           
           ;; Check for opacity
           (unless vparent
             (error 'kw-ctor 
                    "Cannot inherit from opaque struct ~a. The parent must be #:transparent." 
                    'parent))
           (define arity (procedure-arity parent))
           (unless (number? arity)
             (raise-argument-error 'parent
                                   "procedure with flat arity"
                                   arity))

           ;; 4. Construct the new object.
           (apply name
                  (append (take vparent arity)
                          (list field.field ...)))))]
    
    ;; Case 2: Base Case (No Inheritance)
    ;; (struct2 Name (fields ...) ...)
    [(_ name:id (field:struct2-field ...)
        {~optional {~seq #:fender fender-expr}}
        more ...)
     #:with kw-ctor (format-id #'name "~a/kw" #'name #:source #'name)
     #`(begin
         (struct name ({~? [field.field field.mutable] field.field} ...) more ...)
         
         (define (kw-ctor {~@ . field.define2-spec} ...)
           {~? fender-expr}
           (name field.field ...)))]))
