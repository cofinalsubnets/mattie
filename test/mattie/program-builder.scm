(library (test mattie program-builder)
         (export program-builder-tests)
         (import (rnrs) (test util) (mattie program-builder) (rename (mattie parsers stateful) (contains parses)))
  (define matched-parens
    (make-program "a <- \"(\" a \")\" | \"\"" "a"))
  (define program-builder-tests (tests
    ("matching parentheses"
     (assert (parses matched-parens "((()))"))
     (assert (parses matched-parens ""))
     (assert (not (parses matched-parens "(()")))
     (assert (not (parses matched-parens "( )")))))))
