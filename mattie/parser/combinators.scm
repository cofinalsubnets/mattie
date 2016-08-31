(library (mattie parser combinators)
         (export term comp disj conj conc concs <* *> <*> <_> lang-f lang-t lang-0 lang-1
                 opt rep reps repc alt cat cats cat_ one-of language-contains?  packrat eof)
         (import (rnrs) (mattie util) (mattie parser monad))

  ;; terminal string
  (define (term t)
    (letm ((s get-pos))
      (let ((l (string-length t)))
        (if (string=? t (string-take l s))
          (letm ((_ (set-pos (string-drop l s)))) (return t))
          fail))))

  ;; parser negation corresponding language complement
  ;; rewrite this so it doesn't break the monad abstraction
  (define (comp l) (λ (s) (if (l s) (fail s) (cons "" s))))

  ;; parser alternation corresponding to language union
  (define disj orm)

  ;; parser predication corresponding (approximately) to language intersection
  (define (conj a b) (letm ((s get-pos) (_ a) (_ (set-pos s))) b))

  ;; parser catenation corresponding to { xy : x ∈ a, y ∈ b } for languages a b
  (define (conc f a b) (letm ((a a) (b b)) (return (f a b))))
  (define (concs a b) (conc string-append a b))
  (define (<* a b) (conc fst a b))
  (define (*> a b) (conc snd a b))
  (define (<_> a b c) (<*> (<* a b) c))
  (define (<*> a b) (conc cons a b))

  ;; parse 1 or 0 times 
  (define (opt l) (disj l lang-0))

  ;; parse 0 or more times. this is one of the main "looping" constructs so
  ;; it's important that it not break tco!
  (define (rep f i l) (disj (fmap (const i) (conj (comp l) lang-0))
                            (letm ((r l)) (λ (x) ((rep f (f r i) l) x)))))
  (define (reps l) (rep (flip string-append) "" l))
  (define (repc l) (rep cons '() l))

  ;; convenience fns for disj/conj that automatically turn strings into terms
  (define (alt . ls) (fold-right disj lang-f (map terminate ls)))
  (define (cat f i . ls) (fold-right (λ (a b) (conc f a b)) (fmap (const i) lang-0) (map terminate ls)))
  (define (cats . ls) (apply cat (append (list string-append "") ls)))
  (define (cat_ . ls) (apply cat (append (list (const #f) #f) ls)))
  (define (terminate x) (if (string? x) (term x) x))

  ;; convenience fn to make a parser that accepts any char of a given string
  (define (one-of cs)
    (let ((cs (string->list cs)))
      (letm ((c lang-1)) (if (memq (string-ref c 0) cs) (return c) fail))))

  ;; check whether an entire string is in a parser's language
  (define (language-contains? l s)
    (let ((r (l s))) (and (pass? r) (string=? "" (state r)))))

  ;; make a packrat (memoized) parser. in addition to being  way faster this
  ;; lets you handle left-recursive production rules (in principle, we don't
  ;; do that yet)
  (define (packrat p)
    (let ((ht (make-hashtable equal-hash equal?)))
      (λ xs (if (hashtable-contains? ht xs)
              (hashtable-ref ht xs #f)
              (let ((r (apply p xs))) (hashtable-set! ht xs r) r)))))

  ;; empty language containing no strings
  (define lang-f fail)

  ;; language containing all strings
  (define lang-t (comp lang-f))

  ;; singleton language containing the empty string
  (define lang-0 (term ""))

  ;; language containing all 1-character strings
  (define lang-1
    (letm ((s get-pos))
      (if (> (string-length s) 0)
        (letm ((_ (set-pos (string-drop 1 s))))
          (return (string-take 1 s)))
        fail)))

  (define eof (comp lang-1)))
