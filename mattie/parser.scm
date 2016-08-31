(library (mattie parser)
         (export parse-language)
         (import (rnrs) (mattie util) (mattie parser combinators) (mattie parser monad))

  (define (tag t l) (fmap (λ (r) (cons t r)) l))
  (define (tagt t l) (fmap (const t) l))

  (define english-letter
    (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (define decimal-digit (one-of "0123456789"))
  (define ws* (reps (one-of " \n\t\r\f")))

  (define (notp a b) (conj (comp a) b))

  (define word-base
    (let* ((word-start (disj english-letter (term "_")))
           (word-cont (alt word-start decimal-digit "-")))
      (concs word-start (reps word-cont))))

  (define word (tag 'atom (fmap string->symbol word-base)))

  (define term-base
    (let* ((term-part (disj (fmap (const "\"") (term "\\\""))
                            (notp (term "\"") lang-1)))
           (term-base-a (<* (*> (term "\"") (reps term-part)) (term "\"")))
           (qtc (notp (one-of " \n\t\r\f()\"") lang-1))
           (term-base-b (*> (term "'") (concs qtc (reps qtc)))))
      (disj term-base-a term-base-b)))

  (define lterm (tag 'lterm term-base))
  (define rterm (tag 'rterm term-base))

  (define any (tagt 'dot (term ".")))
  (define eof- (tagt 'eof (term "$")))
  (define rep- (tagt 'rep (term "*")))
  (define opt- (tagt 'opt (term "?")))
  (define neg- (tagt 'neg (term "~")))
  (define out  (tagt 'out (term "%")))
  (define suff (repc (alt rep- opt- neg- out)))
  (define call (tag 'call (fmap string->symbol word-base)))

  ;; thunk'd so packrat hashtables can be gc'd
  (define (make-language-parser)
    (define-lazy prec0 (packrat (alt par lterm eof- any (notp defn word))))
    (define prec1
      (packrat (conc (λ (p ss) (fold-left (flip cons) p ss)) prec0 suff)))
    (define-lazy prec2 (packrat (disj cat- prec1)))
    (define-lazy prec3 (packrat (disj and- prec2)))
    (define-lazy prec4 (packrat (disj map- prec3)))
    (define-lazy prec5 (packrat (disj alt- prec4)))
    (define-lazy map-rhs-1 (alt rterm (notp defn call)))
    (define-lazy map-rhs-2
      (tag 'rcat (<_> map-rhs-1 ws* (disj map-rhs-2 map-rhs-1))))
    (define map-rhs (disj map-rhs-2 map-rhs-1))

    (define par (_*_ (cat_ "(" ws*) prec5 (cat_ ws* ")")))
    (define cat- (tag 'lcat (<_> prec1 ws* prec2)))
    (define and- (tag 'and (<_> prec2 (cat_ ws* (term "&") ws*) prec3)))
    (define alt- (tag 'alt (<_> prec4 (cat_ ws* (term "|") ws*) prec5)))
    (define map- (tag 'map (<_> prec3 (cat_ ws* (term "->") ws*) map-rhs)))
    (define defn (tag 'def (<_> (*> ws* word) (cat_ ws* (term "<-") ws*) prec5)))

    (<* (<*> defn (repc defn)) ws*))
  
  (define (parse-language s)
    (let ((r ((make-language-parser) s)))
      (and (pass? r) (string=? "" (state r)) (val r)))))
