(library (test mattie parser)
         (export parser-tests parser-tests-for)
         (import (rnrs)
                 (mattie parser)
                 (test util))

  (define (language-contains? p s) (let ((r (p s))) (and r (string=? (car r) ""))))

  (define (parser-tests-for program)
    (tests
      ("one simple rule"
       (assert (language-contains? program "a <- b")))
      ("multiple rules"
       (assert (language-contains? program "a <- b c <- d q <- p")))
      ("concatenation"
       (assert (language-contains? program "a <- b c")))
      ("disjunction"
       (assert (language-contains? program "a <- b | c")))
      ("conjunction"
       (assert (language-contains? program "a <- b & c")))
      ("complement"
       (assert (language-contains? program "a <- b~")))
      ("repetition"
       (assert (language-contains? program "a <- b*")))
      ("option"
       (assert (language-contains? program "a <- b?")))
      ("dot"
       (assert (language-contains? program "a <- .")))
      ("\"-delimited terminals"
       (assert (language-contains? program "a <- \"bc\" \"q\"")))
      ("'-initiated terminals"
       (assert (language-contains? program "main <- 'b@c (. -> $ '.)*")))
      ("terminals containing escaped quotes"
       (assert (language-contains? program "a <- \"\\\"\"")))
      ("match application on an atom"
       (assert (language-contains? program "a <- \"b\" -> q")))
      ("match application on a terminal"
       (assert (language-contains? program "a <- \"b\" -> \"q\"")))
      ("match application on a sequence of atoms and terminals"
       (assert (language-contains? program "a <- \"b\" -> q \"qwer\" x")))
      ("match application followed by definition"
       (assert (language-contains? program "a <- \"b\" -> q x <- y")))
      ("match application state reference"
       (assert (language-contains? program "a <- \"b\" -> $ \".\"")))
      ("concatenation precedes disjunction"
       (let ((r (program "a <- b c | d")))
         (assert (equal? (caddr (cadr r)) 'alt))))
      ("concatenation precedes conjunction"
       (let ((r (program "a <- b c & d")))
         (assert (equal? (caddr (cadr r)) 'and))))
      ("conjunction precedes disjunction"
       (let ((r (program "a <- b & c | d")))
         (assert (equal? (caddr (cadr r)) 'alt))))
      ("parentheses"
       (let ((r (program "a <- b (c | d)")))
         (assert (equal? (caddr (cadr r)) 'lcat))))
      ("nested parentheses"
       (assert (language-contains? program "a <- ((b))")))
      ("multiple suffixes"
       (let ((r (program "a <- b~*?")))
         (assert (equal? (caddr  (cadr r)) 'opt))
         (assert (equal? (cadddr (cadr r)) 'rep))
         (assert (equal? (cadddr (cdadr r)) 'neg))))
      ("leading & trailing whitespace"
       (assert (language-contains? program "  a <- \nb \t")))))
    
  (define parser-tests (parser-tests-for parse-language)))
