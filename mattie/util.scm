(library (mattie util)
         (export const define-lazy λ let-when)
         (import (rnrs))

  (define-syntax λ (syntax-rules () ((λ ll x ...) (lambda ll x ...))))

  (define-syntax define-lazy
    (syntax-rules ()
      ((define-lazy name expr)
       (define (name . args)
         (set! name expr)
         (apply name args)))))

  (define-syntax let-when
    (syntax-rules ()
      ((let-when ((name expr) bs ...) body ...)
       (let ((name expr)) (and name (let-when (bs ...) body ...))))
      ((let-when () body ...) (begin body ...))))

  (define (const x) (λ _ x)))
