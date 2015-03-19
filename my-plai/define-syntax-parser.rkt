#lang racket/base

(provide define-syntax-parser)

(require (for-syntax racket/base
                     syntax/parse
                     racket/stxparam
                     (for-syntax racket/base
                                 )))

(define-syntax define-syntax-parser
  (syntax-parser
    [(define-syntax-parser id:id #:stx stx-arg:id option-or-clause ...)
     #'(define-syntax (id stx-arg)
         (syntax-parameterize ([this-syntax (make-rename-transformer #'stx-arg)])
           (syntax-parse stx-arg option-or-clause ...)))]
    [(define-syntax-parser id:id option-or-clause ...)
     #'(define-syntax-parser id #:stx stx option-or-clause ...)]))

