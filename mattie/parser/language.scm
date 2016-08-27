(library (mattie parser language)
         (export make-language-parser)
         (import (rnrs) (mattie parser stateful))

  (define (tag-terminal t l)
    (lmap (lambda (s st) (cons (cons t s) st)) l))
  (define (tag-unary t l)
    (lmap (lambda (s st) (cons (cons t (car st)) (cdr st))) l))
  (define (tag-nullary t l)
    (lmap (lambda (s st) (cons (list t) st)) l))
  (define (tag-binary t l)
    (lmap (lambda (s st) (cons (cons t (cons (cadr st) (car st)))
                               (cddr st))) l))

  (define english-letter
    (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (define decimal-digit
    (one-of "0123456789"))
  (define ws* (rep (one-of " \n\t\r\f")))

  (define word
    (let* ((word-start (disj english-letter (one-of "_")))
           (word-cont (alt word-start decimal-digit (term "-"))))
      (tag-terminal 'atom (conc word-start (rep word-cont)))))

  (define term-
    (let ((term-part (disj (term "\\\"") (conj (comp (term "\"")) lang-1))))
      (tag-terminal 'term (cat (term "\"") (rep term-part) (term "\"")))))

  (define (memoize p)
    (let ((ht (make-hashtable equal-hash equal?)))
      (lambda args
        (let ((v (hashtable-ref ht args '())))
          (if (null? v)
            (let ((r (apply p args))) (hashtable-set! ht args r) r) v)))))

  (define dot (tag-nullary 'dot (term ".")))
  (define rep- (tag-unary 'rep (term "*")))
  (define opt- (tag-unary 'opt (term "?")))
  (define neg- (tag-unary 'neg (term "~")))

  ;; this is a thunk so packrat memo tables can be gc'd
  (define (make-language-parser)
    (define prec0 (memoize
      (lambda (s st) ((alt paren term- dot (conj (comp defn) word)) s st))))
    (define prec1 (memoize (cat prec0 (rep (alt rep- opt- neg-)))))
    (define prec2 (memoize (lambda (s st) ((disj cat- prec1) s st))))
    (define prec3 (memoize (lambda (s st) ((disj and- prec2) s st))))
    (define prec4 (memoize (lambda (s st) ((disj alt- prec3) s st))))
    (define prec5 (memoize (lambda (s st) ((disj map- prec4) s st))))
    (define (paren s st) ((cat (term "(") ws* prec5 ws* (term ")")) s st))
    (define cat- (tag-binary 'cat (cat prec1 ws*                prec2)))
    (define and- (tag-binary 'and (cat prec2 ws* (term "&") ws* prec3)))
    (define alt- (tag-binary 'alt (cat prec3 ws* (term "|") ws* prec4)))
    (define map- (tag-binary 'map (cat prec4 ws* (term "->") ws* word)))
    (define defn (tag-binary 'def (cat ws* word ws* (term "<-") ws* prec5)))
    (cat defn (rep defn) ws*)))
