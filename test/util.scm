(library (test util)
         (export test tests run-test passed? failed? run-test-suite)
         (import (chezscheme))

  (define-syntax test
    (syntax-rules ()
      ((test msg body ...)
       (lambda () (guard (x (#t (list msg #f x))) body ... (list msg #t))))))

  (define-syntax tests
    (syntax-rules ()
      ((tests (msg body ...) ...) (list (test msg body ...) ...))))

  (define (run-test t)
    (let ((r (t))) (display (if (cadr r) "." "X")) r))

  (define passed? cadr)
  (define (failed? tr) (not (passed? tr)))
  (define (show . xs) (for-each display xs))

  (define (map- f l) (if (null? l) '() (cons (f (car l)) (map- f (cdr l)))))

  (define (run-test-suite name ts)
    (show "Running " name " ")
    (let* ((rs (map- run-test ts))
           (fs (filter failed? rs)))
      (if (null? fs)
        (show " :D ok!\n")
        (begin
          (show " D: oh no\n" (length fs) " failures:\n")
          (let loop ((n 1) (f fs))
            (if (null? f) '()
              (begin
                (show n ". " (caar f) "\n   ")
                (display-condition (caddar f))
                (newline)
                (loop (+ n 1) (cdr f))))))))))
