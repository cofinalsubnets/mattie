(library (mattie parsers stateful)
         (export term comp disj conj conc lmap
                 lang-f lang-t lang-0 lang-1
                 opt rep alt cat one-of
                 ws ws* ws+ decimal-digit
                 english-letter contains)
         (import (rnrs))

  (define (term t)
      (lambda (s st)
        (let ((lt (string-length t))
              (ls (string-length s)))
          (and (>= ls lt)
               (string=? t (substring s 0 lt))
               (cons (substring s lt ls) st)))))

  (define (comp l) (lambda (s st) (and (not (l s st)) (cons "" st))))
  (define (disj a b) (lambda (s st) (or (a s st) (b s st))))

  (define (conj a b)
    (lambda (s st) (let ((ar (a s st))) (and ar (b s (cdr ar))))))

  (define (conc a b)
    (lambda (s st)
      (let ((ar (a s st)))
        (and ar (b (car ar) (cdr ar))))))

  (define (lmap f l)
    (lambda (s st)
      (let ((r (l s st)))
        (and r (cons (car r)
                     (f (substring s 0 (- (string-length s)
                                          (string-length (car r)))) 
                        (cdr r)))))))

  (define (contains l s) (let ((r (l s '()))) (and r (string=? "" (car r)))))

  (define (lang-f s st) #f)
  (define lang-t (comp lang-f))
  (define lang-0 (term ""))
  (define (lang-1 s st) (and (> (string-length s) 0)
                             (cons (substring s 1 (string-length s)) st)))

  (define (term~ x) (if (string? x) (term x) x))
  (define (opt l) (disj l lang-0))
  (define (rep l) (opt (conc l (lambda (s st) ((rep l) s st)))))
  (define (alt . ls) (fold-right disj lang-f (map term~ ls)))
  (define (cat . ls) (fold-right conc lang-0 (map term~ ls)))
  (define (one-of cs)
    (let ((cs (string->list cs)))
      (lambda (s st)
        (let ((ls (string-length s)))
          (and (> ls 0)
               (let* ((c (string-ref s 0))
                      (m (find (lambda (x) (char=? x c)) cs)))
                 (and m (cons (substring s 1 ls) st))))))))

  (define ws (one-of " \n\t\r"))
  (define ws* (rep ws))
  (define ws+ (conc ws ws*))
  (define decimal-digit
    (one-of "0123456789"))
  (define english-letter
    (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
