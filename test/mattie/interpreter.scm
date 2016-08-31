(library (test mattie interpreter)
         (export interpreter-tests)
         (import (chezscheme)
                 (test util)
                 (mattie interpreter)
                 (mattie parser combinators))

  (define interpreter-tests (tests
    ("reject programs with undefined symbols"
     (assert (not (interpret-mattie "a <- b" "a"))))
    ("reject programs with a missing entry point"
     (assert (not (interpret-mattie "a <- b" "c"))))
    ("matched parentheses"
      (define matched-parens
        (interpret-mattie "a <- \"(\" a \")\" | \"\"" 'a))
     (assert (language-contains? matched-parens "((()))"))
     (assert (language-contains? matched-parens ""))
     (assert (not (language-contains? matched-parens "(()")))
     (assert (not (language-contains? matched-parens "( )"))))
    ("multiple rules"
     (define prog (interpret-mattie "a <- b b <- (c | d)* c <- \"a\" d <- \"bc\"" 'a))
     (assert (language-contains? prog "aaabcaaaabcbca"))
     (assert (not (language-contains? prog "baaaac"))))
    ("conc conj disj comp"
     (define src
       "digit <- \"0\" | \"1\" | \"2\" | \"3\"
        odd <- \"1\" | \"3\"
        even <- odd~ & digit
        main <- even odd even")
     (define prog (interpret-mattie src 'main))
     (assert (language-contains? prog "032"))
     (assert (not (language-contains? prog "201"))))
    ("eof"
     (define prog (interpret-mattie "a <- 'x . 'x $~ | 'xxx $" 'a))
     (assert (language-contains? prog "xxx"))
     (assert (not (language-contains? prog "xyx"))))
    ("subparsers only succeed if they consume their entire input"
     (define prog (interpret-mattie "a <- 'bbb -> b b <- 'bb" 'a))
     (assert (not (language-contains? prog "bbb"))))
    ("map matches to terminals"
     (define prog (interpret-mattie "a <- \"x\" -> \"hello \" \"world\"" 'a))
     (assert (equal? (prog "x") (cons "" "hello world"))))
    ("map matches to subparser calls"
     (define prog (interpret-mattie "a <- 'qqq -> b b <- .* -> 'bbb" 'a))
     (assert (equal? (prog "qqq") (cons "" "bbb"))))
    ("prints designated matches"
     (define prog (interpret-mattie "a <- ('b -> 'xxx)!" 'a))
     (define s (with-output-to-string (lambda () (prog "b"))))
     (assert (string=? s "xxx")))
    ("rule 110"
     (define src "
       rule110 <- '111 -> '0
                | '110 -> '1
                | '101 -> '1
                | '100 -> '0
                | '011 -> '1
                | '010 -> '1
                | '001 -> '1
                | '000 -> '0
       loop <- ... & main | .* -> \"\"
       main <- (.(..)< -> rule110)*! ((.* -> \"\n\")! -> \"\") -> loop")
      (define prog (interpret-mattie src 'main))
      (assert (language-contains? prog "000"))
      (let  ((output (with-output-to-string (lambda () (prog "00101")))))
        (assert (string=? output "111\n0\n"))))
    ("map matches to current state"
     (define count-xs (interpret-mattie "x <- \"x\" -> \".\" y <- \"y\" -> \"\" b <- (x|y)*" 'b))
     (assert (equal? (count-xs "xyyxyxxy") (cons "" "....")))))))
