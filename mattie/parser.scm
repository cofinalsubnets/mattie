(library (mattie parser)
         (export parse-language)
         (import (rnrs) (mattie util) (mattie parser combinators))

  (define (tag-terminal t l)
    (lmap (λ (s st) (cons (cons t s) st)) l))
  (define (tag-unary t l)
    (lmap (λ (_ st) (cons (cons t (car st)) (cdr st))) l))
  (define (tag-nullary t l)
    (lmap (λ (_ st) (cons (list t) st)) l))
  (define (tag-binary t l)
    (lmap (λ (_ st) (cons (cons t (cons (cadr st) (car st))) (cddr st))) l))

  (define english-letter
    (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (define decimal-digit (one-of "0123456789"))
  (define ws* (rep (one-of " \n\t\r\f")))

  (define (notp a b) (conj (comp a) b))

  (define word-base
    (let* ((word-start (disj english-letter (one-of "_")))
           (word-cont (alt word-start decimal-digit (term "-"))))
      (conc word-start (rep word-cont))))

  (define word (tag-terminal 'atom word-base))

  (define term-base
    (let* ((term-part (disj (term "\\\"") (notp (term "\"") lang-1)))
           (term-base-a (cat (term "\"") (rep term-part) (term "\"")))
           (qtc (notp (one-of " \n\t\r\f()") lang-1))
           (term-base-b (conc (term "'") (conc qtc (rep qtc)))))
      (disj term-base-a term-base-b)))

  (define lterm (tag-terminal 'lterm term-base))
  (define rterm (tag-terminal 'rterm term-base))

  (define dot (tag-nullary 'dot (term ".")))
  (define $ (tag-nullary 'state (term "$")))
  (define rep- (tag-unary 'rep (term "*")))
  (define opt- (tag-unary 'opt (term "?")))
  (define neg- (tag-unary 'neg (term "~")))

  ;; thunk'd so packrat hashtables can be gc'd
  (define (make-language-parser)
    (define-lazy prec0 (packrat (alt paren lterm dot (notp defn word))))
    (define      prec1 (packrat (cat prec0 (rep (alt rep- opt- neg-)))))
    (define-lazy prec2 (packrat (disj cat- prec1)))
    (define-lazy prec3 (packrat (disj and- prec2)))
    (define-lazy prec4 (packrat (disj alt- prec3)))

    (define-lazy map-rhs-unit (alt $ rterm (notp defn word)))
    (define-lazy map-rhs-cat
      (tag-binary 'rcat (cat map-rhs-unit ws* (disj map-rhs-cat map-rhs-unit))))
    (define map-rhs (disj map-rhs-cat map-rhs-unit))
    (define map- (tag-binary 'map (cat prec4 ws* (term "->") ws* map-rhs)))

    (define prec5 (packrat (disj map- prec4)))

    (define paren (cat (term "(") ws* prec5 ws* (term ")")))
    (define cat- (tag-binary 'lcat (cat prec1 ws*                prec2)))
    (define and- (tag-binary 'and (cat prec2 ws* (term "&") ws* prec3)))
    (define alt- (tag-binary 'alt (cat prec3 ws* (term "|") ws* prec4)))
    (define defn (tag-binary 'def (cat ws* word ws* (term "<-") ws* prec5)))
    (define lang (cat defn (rep defn) ws*))
    (λ (s) (lang s '())))
  
  (define (parse-language s) ((make-language-parser) s)))
