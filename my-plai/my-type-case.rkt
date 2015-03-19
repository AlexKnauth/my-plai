#lang racket/base

(provide define-type
         my-type-case
         )

(require racket/match
         plai/datatype
         "define-syntax-parser.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     syntax/stx
                     racket/struct-info
                     ))



(begin-for-syntax
  (define disappeared-use 'disappeared-use)
  
  (define-syntax-class variant-id
    [pattern variant-id:id
             #:attr v (syntax-local-value #'variant-id (λ () #f))
             #:when (struct-info? (attribute v))
             #:attr info (extract-struct-info (attribute v))
             #:do [(define (info-ref i)
                     (list-ref (attribute info) i))]
             #:attr desc (info-ref 0)
             #:attr constructor (info-ref 1)
             #:attr pred (info-ref 2)
             #:with fields (reverse (info-ref 3))
             ])
  
  (define-syntax-class my-type-case-clause
    #:attributes (norms) #:literals (else)
    [pattern [((~and o (~literal or)) variant-id:variant-id ...) body:expr ...+]
             #:with new-body (syntax-property
                              (syntax/loc this-syntax
                                (let () body ...))
                              disappeared-use (stx-map syntax-local-introduce #'(o variant-id ...)))
             #:with (clause:my-type-case-clause ...) (for/list ([variant-id
                                                                 (syntax->list #'(variant-id ...))])
                                                       (with-syntax ([variant-id variant-id])
                                                         (syntax/loc this-syntax
                                                           [variant-id new-body])))
             #:with ((norm ...) ...) #'(clause.norms ...)
             #:with norms #'(norm ... ...)]
    [pattern [(variant-id:variant-id pat:expr ...) body:expr ...+]
             #:with (field:id ...) (generate-temporaries #'(pat ...))
             #:with new-body (syntax-property
                              (syntax/loc this-syntax
                                (match-let ([pat field] ...)
                                  body ...))
                              disappeared-use (list (syntax-local-introduce #'variant-id)))
             #:with norm (syntax/loc this-syntax
                           [variant-id (field ...) new-body])
             #:with norms #'(norm)]
    [pattern [variant-id:variant-id body:expr ...+]
             #:with (field:id ...) #'variant-id.fields
             #:with new-body (syntax-property
                              (syntax/loc this-syntax
                                (let () body ...))
                              disappeared-use (list (syntax-local-introduce #'variant-id)))
             #:with norm (syntax/loc this-syntax
                           [variant-id (field ...) new-body])
             #:with norms #'(norm)]
    [pattern [(~and el else) body:expr ...+]
             #:with new-body (syntax-property
                              (syntax/loc this-syntax
                                (let () body ...))
                              disappeared-use (list (syntax-local-introduce #'el)))
             #:with norm (syntax/loc this-syntax
                           [else new-body])
             #:with norms #'(norm)]))

(define-syntax-parser my-type-case
  [(my-type-case type:id expr:expr clause:my-type-case-clause ...+)
   #:with ((clause-norm ...) ...) #'(clause.norms ...)
   (syntax-property
    (syntax/loc this-syntax
      (type-case type expr clause-norm ... ...))
    disappeared-use (list (syntax-local-introduce #'type)))])

