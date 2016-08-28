(library (test util)
         (export test tests)
         (import (chezscheme))

  (define-syntax test
    (syntax-rules ()
      ((test msg body ...)
       (lambda () (guard (x (#t (list msg #f x))) body ... (list msg #t))))))

  (define-syntax tests
    (syntax-rules ()
      ((tests (msg body ...) ...) (list (test msg body ...) ...)))))
