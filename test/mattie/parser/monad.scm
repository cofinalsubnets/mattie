(library (test mattie parser monad)
         (export parser-monad-tests)
         (import (rnrs) (mattie util) (test util) (mattie parser monad))

  (define (check m v s)
    (assert (pass? m))
    (assert (equal? (state m) s))
    (assert (equal? (val m) v)))

  (define parser-monad-tests (tests
    ("return"
     (check ((return 1) 'state) 1 'state))
    ("bind"
     (let* ((a (return 1))
            (f (λ (x) (return (+ x 1))))
            (r ((bind a f) #f)))
       (check ((bind a f) #f) 2 #f)))
    ("fmap"
     (let* ((a (return "hi"))
            (b (fmap (λ (s) (string-append s " pal")) a)))
       (check (b '()) "hi pal" '())))
    ("get-pos" (check (get-pos "x") "x" "x"))
    ("set-pos" (check ((set-pos "x") #t) #f "x"))
    ("letm" (let ((m (letm ((x (return 1))
                            (y get-pos)
                            (_ (set-pos 3)))
                       (return (+ x y)))))
              (check (m 2) 3 3))))))
