(library (mattie parser monad)
         (export return bind fmap get-pos set-pos letm fail fail? pass? state val)
         (import (rnrs) (mattie util))

  (define state car)
  (define val cdr)
  (define fail (const #f))
  (define fail? not)
  (define pass? id)

  (define (fmap f m) (letm ((m m)) (return (f m))))

  (define (return v) (λ (x) (cons x v)))

  (define (bind m f)
    (λ (x) (let-when ((m (m x))) ((f (val m)) (state m)))))

  (define (get-pos s) (cons s s))
  (define (set-pos s) (λ _ (cons s #f)))
  (define-syntax letm
    (syntax-rules ()
      ((letm ((name expr) rest ...) body ...)
       (bind expr (λ (name) (letm (rest ...) body ...))))
      ((letm () body ...) (begin body ...)))))
