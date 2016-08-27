(library (test mattie program-builder)
         (export program-builder-tests)
         (import (rnrs) (test util) (mattie program-builder) (rename (mattie parsers stateful) (contains parses)))
  (define program-builder-tests (tests
    ("reject programs with undefined symbols"
     (assert (guard (_ (#t #t)) (make-program "a <- b" "a") #f)))
    ("reject programs with a missing entry point"
     (assert (guard (_ (#t #t)) (make-program "a <- b" "c") #f)))
    ("match parentheses"
      (define matched-parens
        (make-program "a <- \"(\" a \")\" | \"\"" "a"))
     (assert (parses matched-parens "((()))"))
     (assert (parses matched-parens ""))
     (assert (not (parses matched-parens "(()")))
     (assert (not (parses matched-parens "( )")))))))
