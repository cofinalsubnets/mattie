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
        (make-interpreter "a <- \"(\" a \")\" | \"\"" 'a))
     (assert (language-contains? matched-parens "((()))"))
     (assert (language-contains? matched-parens ""))
     (assert (not (language-contains? matched-parens "(()")))
     (assert (not (language-contains? matched-parens "( )"))))
    ("multiple rules"
     (define prog (make-interpreter "a <- b b <- (c | d)* c <- \"a\" d <- \"bc\"" 'a))
     (assert (language-contains? prog "aaabcaaaabcbca"))
     (assert (not (language-contains? prog "baaaac"))))
    ("conc conj disj comp"
     (define src
       "digit <- \"0\" | \"1\" | \"2\" | \"3\"
        odd <- \"1\" | \"3\"
        even <- odd~ & digit
        main <- even odd even")
     (define prog (make-interpreter src 'main))
     (assert (language-contains? prog "032"))
     (assert (not (language-contains? prog "201"))))
    ("map matches to terminals"
     (define prog (make-interpreter "a <- \"x\" -> \"hello \" \"world\"" 'a))
     (assert (equal? (prog "x" "") (cons "" "hello world"))))
    ("map matches to current state"
     (define count-xs (make-interpreter "x <- \"x\" -> <> \".\" y <- \"y\" b <- (x|y)*" 'b))
     (assert (equal? (count-xs "xyyxyxxy" "") (cons "" "....")))))))
