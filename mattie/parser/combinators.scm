(library (mattie parser combinators)
         (export term comp disj conj conc lmap lang-f lang-t lang-0 lang-1
                 opt rep alt cat one-of language-contains? run-stateless
                 packrat)
         (import (rnrs) (mattie util))

  ;; a parser is a procedure that accepts a string (the input) and a state & 
  ;; returns:
  ;;   - on success, a pair comprising a string (the "unparsed" remainder; a
  ;;     right-anchored substring of the input) and an updated state
  ;;   - on failure, #f

  ;; terminal string
  (define (term t)
      (λ (s st)
        (let ((lt (string-length t))
              (ls (string-length s)))
          (and (>= ls lt)
               (string=? t (substring s 0 lt))
               (cons (substring s lt ls) st)))))

  ;; parser negation corresponding language complement
  (define (comp l) (λ (s st) (and (not (l s st)) (cons "" st))))

  ;; parser alternation corresponding to language union
  (define (disj a b) (λ (s st) (or (a s st) (b s st))))

  ;; parser predication corresponding (approximately) to language intersection
  (define (conj a b) (λ (s st) (let-when ((ar (a s st))) (b s (cdr ar)))))

  ;; parser catenation corresponding to { xy : x ∈ a, y ∈ b } for languages a b
  (define (conc a b)
    (λ (s st) (let-when ((ar (a s st))) (b (car ar) (cdr ar)))))

  ;; parse 1 or 0 times 
  (define (opt l) (disj l lang-0))

  ;; parse 0 or more times. implementing w/ opt/conc breaks tco :(
  (define (rep l) (λ (s st)
    (let ((r (l s st)))
      (if r ((rep l) (car r) (cdr r)) (cons s st)))))

  ;; convenience fns for disj/conj that automatically turn strings into terms
  (define (alt . ls) (fold-right disj lang-f (map terminate ls)))
  (define (cat . ls) (fold-right conc lang-0 (map terminate ls)))
  (define (terminate x) (if (string? x) (term x) x))

  ;; convenience fn to make a parser that accepts any char of a given string
  (define (one-of cs)
      (let ((cs (string->list cs)))
        (λ (s st) (and (> (string-length s) 0)
                       (memq (string-ref s 0) cs)
                       (cons (substring s 1 (string-length s)) st)))))

  ;; map a fn over a parser to be applied to its output state
  (define (lmap f l)
    (λ (s st)
      (if (eq? st no-st)
        (l s st)
        (let-when ((r (l s st)))
          (cons (car r) (f (substring s 0 (- (string-length s)
                                             (string-length (car r))))
                           (cdr r)))))))

  ;; hack to make lmap a noop when running statelessly. it would be nice be
  ;; able to use gensym for this but i guess that's not portable?
  (define no-st (list '()))

  ;; run the parser without propagating any state (so it can't really parse,
  ;; just accept/reject a language)
  (define (run-stateless p s) (let-when ((r (p s no-st))) (car r)))

  ;; check whether an entire string is in a parser's language
  (define (language-contains? l s) (equal? "" (run-stateless l s)))

  ;; make a packrat (memoized) parser. in addition to being potentiallt way
  ;; faster this also lets you handle left-recursive production rules (which
  ;; we don't do yet)
  (define (packrat p)
    (let ((ht (make-hashtable equal-hash equal?)))
      (λ xs (let ((v (hashtable-ref ht xs '())))
              (if (null? v) (let ((r (apply p xs))) (hashtable-set! ht xs r) r)
                            v)))))

  ;; empty language containing no strings
  (define lang-f (const #f))

  ;; language containing all strings
  (define lang-t (comp lang-f))

  ;; singleton language containing the empty string
  (define lang-0 (term ""))

  ;; language containing all 1-character strings
  (define (lang-1 s st) (and (> (string-length s) 0)
                             (cons (substring s 1 (string-length s)) st))))
