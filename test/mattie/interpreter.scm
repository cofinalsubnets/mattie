(library (test mattie interpreter)
         (export interpreter-tests)
         (import (rnrs)
                 (test util)
                 (mattie interpreter)
                 (mattie parser combinators))

  (define interpreter-tests (tests
    ("reject programs with undefined symbols"
     (assert (guard (_ (#t #t)) (make-interpreter "a <- b" "a") #f)))
    ("reject programs with a missing entry point"
     (assert (guard (_ (#t #t)) (make-interpreter "a <- b" "c") #f)))
    ("matched parentheses"
      (define matched-parens
        (make-interpreter "a <- \"(\" a \")\" | \"\"" "a"))
     (assert (parses matched-parens "((()))"))
     (assert (parses matched-parens ""))
     (assert (not (parses matched-parens "(()")))
     (assert (not (parses matched-parens "( )"))))
    ("multiple rules"
     (define prog (make-interpreter "a <- b b <- (c | d)* c <- \"a\" d <- \"bc\"" "a"))
     (assert (parses prog "aaabcaaaabcbca"))
     (assert (not (parses prog "baaaac"))))
    ("conc conj disj comp"
     (define src
       "digit <- \"0\" | \"1\" | \"2\" | \"3\"
        odd <- \"1\" | \"3\"
        even <- odd~ & digit
        main <- even odd even")
     (define prog (make-interpreter src "main"))
     (assert (parses prog "032"))
     (assert (not (parses prog "201"))))
    ("map matches to terminals"
     (define prog (make-interpreter "a <- \"x\" -> \"hello \" \"world\"" "a"))
     (assert (equal? (prog "x" "") (cons "" "hello world"))))
       
       )))
