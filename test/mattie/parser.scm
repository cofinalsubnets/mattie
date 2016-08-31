(library (test mattie parser)
         (export parser-tests parser-tests-for)
         (import (rnrs)
                 (mattie parser)
                 (test util))

  (define (language-contains? p s) (p s))

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
       (assert (language-contains? program "main <- 'b@c (. -> '.)*")))
      ("'-terms don't include control chars"
       (define r (program "a <- 'b*!"))
       (assert (eq? (caddar r) 'out)))
      ("whitespace can occur btwn an expr & its suffix"
       (assert (language-contains? program "a <- 'b *")))
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
      ("eof"
       (assert (language-contains? program "a <- \"b\" $")))
      ("symbols are interned"
       (define r (program "a <- 'b"))
       (assert (symbol? (cdadar r))))
      ("concatenation precedes disjunction"
       (let ((r (program "a <- b c | d")))
         (assert (equal? (caddar r) 'alt))))
      ("concatenation precedes conjunction"
       (let ((r (program "a <- b c & d")))
         (assert (equal? (caddar r) 'and))))
      ("conjunction precedes disjunction"
       (let ((r (program "a <- b & c | d")))
         (assert (equal? (caddar r) 'alt))))
      ("parentheses"
       (let ((r (program "a <- b (c | d)")))
         (assert (equal? (caddar r) 'lcat))))
      ("nested parentheses"
       (assert (language-contains? program "a <- ((b))")))
      ("multiple suffixes"
       (let ((r (program "a <- b~*?")))
         (assert (equal? (caddar  r) 'opt))
         (assert (equal? (car (cdddar r)) 'rep))
         (assert (equal? (cadr (cdddar r)) 'neg))))
      ("leading & trailing whitespace"
       (assert (language-contains? program "  a <- \nb \t")))))
    
  (define parser-tests (parser-tests-for parse-mattie)))
