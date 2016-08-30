(library (mattie util)
         (export id const compose define-lazy λ let-when string-take string-drop string-take-r string-drop-r fst snd flip)
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
      ((let-when pred ((name expr) bs ...) head body ...)
       (let ((name expr)) (and (pred name) (let-when pred (bs ...) head body ...))))
      ((let-when _ () head body ...) (begin head body ...))
      ((let-when (bs ...) head body ...)
       (let-when id (bs ...) head body ...))))

  (define (string-take n s)
    (substring s 0 (min n (string-length s))))

  (define (string-drop n s)
    (let ((l (string-length s)))
      (substring s (min n l) l)))

  (define (string-take-r n s)
    (string-drop (max 0 (- (string-length s) n)) s))

  (define (string-drop-r n s)
    (let ((l (string-length s)))
      (string-take (max 0 (- l n)) s)))

  (define (flip f) (λ (a b) (f b a)))
  (define (fst a b) a)
  (define snd (flip fst))
  (define (id x) x)
  (define (const x) (λ _ x))
  (define (compose f g) (λ x (f (apply g x)))))
