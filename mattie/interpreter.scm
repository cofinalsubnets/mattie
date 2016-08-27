(library (mattie interpreter)
         (export make-interpreter)
         (import (rnrs)
                 (mattie parsers stateful)
                 (mattie parsers language))

  (define (make-interpreter src entry-point)
    (let ((r ((make-language-parser) src '())))
      (and r (string=? (car r) "")
           (let ((defs (cdr r)))
             (validate-defs defs entry-point)
             (make-lang defs entry-point)))))

  (define arities
      `((cat . 2)
        (alt . 2)
        (and . 2)
        (map . 2)
        (rep . 1)
        (opt . 1)
        (neg . 1)
        (atom . 0)
        (term . 0)
        (dot . 0)))

  (define (get-syms d)
    (if (eq? (car d) 'atom) (list (cdr d))
      (case (cdr (assq (car d) arities))
        ((0) '())
        ((1) (get-syms (cdr d)))
        ((2) (append (get-syms (cadr d)) (get-syms (cddr d)))))))

  (define (validate-defs ds entry-point)
    (let ((rules (map cdadr ds)))
      (assert (member entry-point rules))
      (let* ((ss (map (lambda (d) (get-syms (cddr d))) ds))
             (rs (fold-left append '() ss))
             (undefined-rules (filter (lambda (s) (not (member s rules))) rs)))
        (assert (null? undefined-rules)))))

  (define (unescape-term t)
    (let ((inner (substring t 1 (- (string-length t) 1))))
      (list->string
        (let loop ((cs (string->list inner)))
          (cond ((null? cs) cs)
                ((char=? (car cs) #\\)
                 (assert (not (null? (cdr cs)))) ;; grammar should ensure this
                 (loop (cons (cadr cs) (cddr cs))))
                (else (cons (car cs) (loop (cdr cs)))))))))

  (define (make-lang defs entry-point)

    (define handlers
      `((cat . ,conc)
        (alt . ,disj)
        (and . ,conj)
        (map . ,(lambda (a _) a)) ;; noop for now
        (term . ,(lambda (t) (term (unescape-term t))))
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

    (cdr (assq (string->symbol entry-point) tbl))))