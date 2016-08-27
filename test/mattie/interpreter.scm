(library (test mattie interpreter)
         (export interpreter-tests)
         (import (rnrs)
                 (test util)
                 (mattie interpreter)
                 (mattie parser stateful))

  (define interpreter-tests (tests
    ("reject programs with undefined symbols"
     (assert (guard (_ (#t #t)) (make-interpreter "a <- b" "a") #f)))
    ("reject programs with a missing entry point"
     (assert (guard (_ (#t #t)) (make-interpreter "a <- b" "c") #f)))
    ("match parentheses"
      (define matched-parens
        (make-interpreter "a <- \"(\" a \")\" | \"\"" "a"))
     (assert (parses matched-parens "((()))"))
     (assert (parses matched-parens ""))
     (assert (not (parses matched-parens "(()")))
     (assert (not (parses matched-parens "( )"))))
    ("multiple rules"
     (define prog (make-interpreter "a <- b b <- (c | d)* c <- \"a\" d <- \"bc\"" "a"))
     (assert (parses prog "aaabcaaaabcbca"))))))
