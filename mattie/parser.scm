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

  (define atom-base
    (let* ((atom-start (disj english-letter (term "_")))
           (atom-cont (alt atom-start decimal-digit "-")))
      (fmap string->symbol (concs atom-start (reps atom-cont)))))

  (define atom (tag 'atom atom-base))
  (define call (tag 'call atom-base))

  (define term-base
    (let* ((term-part (disj (fmap (const "\"") (term "\\\""))
                            (notp (term "\"") lang-1)))
           (term-base-a (_*_ (term "\"") (reps term-part) (term "\"")))
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
  (define out  (tagt 'out (term "!")))
  (define suff (repc (alt rep- opt- neg- out)))

  ;; thunk'd so packrat hashtables can be gc'd
  (define (make-language-parser)
    (define-lazy prec0 (packrat (alt par lterm eof- any (notp def atom))))
    (define prec1
      (packrat (conc (λ (p ss) (fold-left (flip cons) p ss)) prec0 suff)))
    (define-lazy prec2 (packrat (disj cat- prec1)))
    (define-lazy prec3 (packrat (disj and- prec2)))

    (define-lazy map-rhs-1 (alt rterm (notp def call)))
    (define-lazy map-rhs-2
      (tag 'rcat (<_> map-rhs-1 ws* (disj map-rhs-2 map-rhs-1))))
    (define map-rhs (disj map-rhs-2 map-rhs-1))
    (define map- (tag 'map (<_> prec3 (cat_ ws* (term "->") ws*) map-rhs)))

    (define prec4 (packrat (disj map- prec3)))
    (define-lazy prec5 (packrat (disj alt- prec4)))

    (define par (_*_ (cat_ "(" ws*) prec5 (cat_ ws* ")")))
    (define cat- (tag 'lcat (<_> prec1 ws* prec2)))
    (define and- (tag 'and (<_> prec2 (cat_ ws* (term "&") ws*) prec3)))
    (define alt- (tag 'alt (<_> prec4 (cat_ ws* (term "|") ws*) prec5)))
    (define def (tag 'def (<_> (*> ws* atom) (cat_ ws* (term "<-") ws*) prec5)))

    (<* (<*> def (repc def)) ws*))
  
  (define (parse-language s)
    (let ((r ((make-language-parser) s)))
      (and (pass? r) (string=? "" (state r)) (val r)))))
