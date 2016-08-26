(library (px stateful-parsers)
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
               (list t (substring s lt ls) st)))))

  (define (comp l) (lambda (s st) (and (not (l s st)) (list s "" st))))
  (define (disj a b) (lambda (s st) (or (a s st) (b s st))))

  (define (conj a b)
    (lambda (s st) (let ((ar (a s st))) (and ar (b s (caddr ar))))))
  (define (conc a b)
    (lambda (s st)
      (let ((ar (a s st)))
        (and ar (let ((br (b (cadr ar) (caddr ar))))
                  (and br (cons (string-append (car ar) (car br)) (cdr br))))))))
  (define (lmap f l)
    (lambda (s st)
      (let ((r (l s st)))
        (and r (list (car r) (cadr r) (f (car r) (caddr r)))))))

  (define (contains l s) (let ((r (l s '()))) (and r (string=? "" (cadr r)))))

  (define (lang-f s st) #f)
  (define lang-t (comp lang-f))
  (define lang-0 (term ""))
  (define (lang-1 s st) (and (> (string-length s) 0)
                             (list (string (string-ref s 0))
                                   (substring s 1 (string-length s))
                                   st)))

  (define (opt l) (disj l lang-0))
  (define (rep l) (opt (conc l (lambda (s st) ((rep l) s st)))))
  (define (alt . ls) (fold-right disj lang-f ls))
  (define (cat . ls) (fold-right conc lang-0 ls))
  (define (one-of cs)
    (let ((cs (string->list cs)))
      (lambda (s st)
        (let ((ls (string-length s)))
          (and (> ls 0)
               (let* ((c (string-ref s 0))
                      (m (find (lambda (x) (char=? x c)) cs)))
                 (and m (list (string m) (substring s 1 ls) st))))))))

  (define ws (one-of " \n\t\r"))
  (define ws* (rep ws))
  (define ws+ (conc ws ws*))
  (define decimal-digit
    (one-of "0123456789"))
  (define english-letter
    (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(library (px lang)
         (export px-lang)
         (import (rnrs) (px stateful-parsers))

  (define (tag-terminal t l)
    (lmap (lambda (s st) (cons (cons t s) st)) l))
  (define (tag-nullary t l)
    (lmap (lambda (s st) (cons (list t) st)) l))
  (define (tag-unary t l)
    (lmap (lambda (s st) (cons (cons t (car st)) (cdr st))) l))
  (define (tag-binary t l)
    (lmap (lambda (s st) (cons (cons t (cons (cadr st) (car st))) (cddr st))) l))

  (define word
    (let* ((word-start (disj english-letter (one-of "_")))
           (word-cont (alt word-start decimal-digit (term "-"))))
      (tag-terminal 'atom (conc word-start (rep word-cont)))))

  (define term-
    (let ((term-part (disj (conj (comp (term "\"")) lang-1) (term "\\\""))))
      (tag-terminal 'term (cat (term "\"") (rep term-part) (term "\"")))))

  (define (paren s st) ((cat (term "(") ws* expr ws* (term ")")) s st))
  (define dot (tag-nullary 'dot (term ".")))
  (define uni-base (alt paren term- word dot))
  (define rep- (tag-unary 'rep (cat uni-base ws* (term "*"))))
  (define opt- (tag-unary 'opt (cat uni-base ws* (term "?"))))
  (define neg- (tag-unary 'neg (cat uni-base ws* (term "~"))))
  (define uni (alt rep- opt- neg- uni-base))
  (define (expr s st)
    ((conc ws* (alt alt- and- cat- (conj (comp defn) uni))) s st))
  (define alt- (tag-binary 'alt (cat uni ws* (term "|") ws* expr)))
  (define cat- (tag-binary 'cat (cat uni ws* expr)))
  (define and- (tag-binary 'and (cat uni ws* (term "&") ws* expr)))
  (define defn (tag-binary 'def (cat ws* word ws* (term "<-") ws* expr)))
  (define lang (cat defn (rep defn) ws*))

  (define (px-lang s)
    (let ((r (lang s '())))
      (and r (string=? (cadr r) "")
           (let ((defs (caddr r)))
             (validate-defs defs)
             (make-lang defs)))))

  (define arities
      `((cat . 2)
        (alt . 2)
        (and . 2)
        (rep . 1)
        (opt . 1)
        (neg . 1)
        (atom . 0)
        (term . 0)
        (dot . 0)))

  (define (get-syms d)
    (if (eq? (car d) 'atom) (list (cdr d))
      (case (car d)
        ((0) '())
        ((1) (get-syms (cdr d)))
        ((2) (append (get-syms (cadr d)) (get-syms (cddr d)))))))

  (define (validate-defs ds)
    (let ((rules (map cdadr ds)))
      (assert (member "main" rules))
      (let* ((rs (fold-left append '()
                            (map (lambda (d) (get-syms (cddr d))) ds)))
             (undefined-rules (filter (lambda (s) (not (member s rules))) rs)))
        (assert (null? undefined-rules)))))

  (define (make-lang defs)

    (define handlers
      `((cat . ,conc)
        (alt . ,disj)
        (and . ,conj)
        (term . ,term)
        (opt . ,opt)
        (neg . ,comp)
        (dot . ,(lambda _ lang-1))
        (rep . ,(lambda (l) (if (eq? l lang-1) lang-t (rep l))))
        (atom . ,(lambda (a)
                   (define sym (string->symbol a))
                   (define (f s st)
                     (set! f (cdr (assq sym tbl)))
                     (f s st))
                   (lambda (s st) (f s st ))))))

    (define (linguify b)
        (apply (cdr (assq (car b) handlers))
               (case (cdr (assq (car b) arities))
                 ((0) (list (cdr b)))
                 ((1) (list (linguify (cdr b))))
                 ((2) (list (linguify (cadr b)) (linguify (cddr b)))))))

    (define (r t d)
      (let-values (((name body) (values (cdadr d) (cddr d))))
        (cons (cons (string->symbol name) (linguify body)) t)))

    (define tbl (fold-left r '() defs))

    (cdr (assq 'main tbl))))

