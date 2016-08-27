(library (mattie parser combinators)
         (export term comp disj conj conc lmap
                 lang-f lang-t lang-0 lang-1
                 opt rep alt cat one-of parses
                 run-stateless)
         (import (rnrs) (mattie util))

  (define (term t)
      (λ (s st)
        (let ((lt (string-length t))
              (ls (string-length s)))
          (and (>= ls lt)
               (string=? t (substring s 0 lt))
               (cons (substring s lt ls) st)))))

  (define (comp l) (λ (s st) (and (not (l s st)) (cons "" st))))
  (define (disj a b) (λ (s st) (or (a s st) (b s st))))

  (define (conj a b)
    (λ (s st) (let ((ar (a s st))) (and ar (b s (cdr ar))))))

  (define (conc a b)
    (λ (s st) (let ((ar (a s st))) (and ar (b (car ar) (cdr ar))))))

  ;; hack to make sure state is ignored lmapped fns. it would be nice be able
  ;; to use gensym for this.
  (define signal-stateless (list '()))

  (define (lmap f l)
    (λ (s st)
      (let ((r (l s st)))
        (if (eq? st signal-stateless) r
          (and r (cons (car r)
                       (f (substring s 0 (- (string-length s)
                                            (string-length (car r))))
                          (cdr r))))))))

  (define (run-stateless p s) (let ((r (p s signal-stateless)))
                                (and r (car r))))

  (define (parses l s) (equal? "" (run-stateless l s)))

  (define (lang-f s st) #f)
  (define lang-t (comp lang-f))
  (define lang-0 (term ""))
  (define (lang-1 s st) (and (> (string-length s) 0)
                             (cons (substring s 1 (string-length s)) st)))

  (define (terminate x) (if (string? x) (term x) x))
  (define (opt l) (disj l lang-0))
  (define (rep l) (opt (conc l (λ (s st) ((rep l) s st)))))
  (define (alt . ls) (fold-right disj lang-f (map terminate ls)))
  (define (cat . ls) (fold-right conc lang-0 (map terminate ls)))
  (define (one-of cs)
    (let ((cs (string->list cs)))
      (λ (s st) (and (> (string-length s) 0)
                     (memq (string-ref s 0) cs)
                     (cons (substring s 1 (string-length s)) st))))))
