(library (mattie util)
         (export id const compose define-lazy λ)
         (import (rnrs))

  (define-syntax λ (syntax-rules () ((λ ll x ...) (lambda ll x ...))))

  (define-syntax define-lazy
    (syntax-rules ()
      ((define-lazy name expr)
       (define (name . args)
         (set! name expr)
         (apply name args)))))

  (define (id x) x)
  (define (const x) (λ _ x))
  (define (compose f g) (λ x (f (apply g x)))))
