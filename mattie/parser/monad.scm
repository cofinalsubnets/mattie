(library (mattie parser monad)
         (export return bind join fmap mcomp get-pos set-pos letm fail fail? pass? orm state val)
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

  (define (orm a b) (λ (x) (or (let-when pass? ((r (a x))) r) (b x))))

  (define (join m) (bind m id))
  (define (mcomp a b) (λ (x) (bind (a x) b)))
  (define (get-pos s) (cons s s))
  (define (set-pos s) (λ _ (cons s s)))
  (define-syntax letm
    (syntax-rules ()
      ((letm ((name expr) rest ...) body ...)
       (bind expr (λ (name) (letm (rest ...) body ...))))
      ((letm () body ...) (begin body ...)))))
