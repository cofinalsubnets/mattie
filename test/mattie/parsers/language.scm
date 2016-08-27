(library (test mattie parsers language)
         (export language-parser-tests language-tests-for)
         (import (rnrs)
                 (rename (mattie parsers stateful) (contains parses))
                 (mattie parsers language)
                 (test util))
  (define (language-tests-for program)
    (tests
      ("one simple rule"
       (assert (parses program "a <- b")))
      ("multiple rules"
       (assert (parses program "a <- b c <- d q <- p")))
      ("concatenation"
       (assert (parses program "a <- b c")))
      ("disjunction"
       (assert (parses program "a <- b | c")))
      ("conjunction"
       (assert (parses program "a <- b & c")))
      ("complement"
       (assert (parses program "a <- b~")))
      ("repetition"
       (assert (parses program "a <- b*")))
      ("option"
       (assert (parses program "a <- b?")))
      ("dot"
       (assert (parses program "a <- .")))
      ("terminals"
       (assert (parses program "a <- \"bc\" \"q\"")))
      ("terminals containing escaped quotes"
       (assert (parses program "a <- \"\\\"\"")))
      ("concatenation precedes disjunction"
       (let ((r (program "a <- b c | d" "")))
         (assert (equal? (caddr (cadr r)) 'alt))))
      ("concatenation precedes conjunction"
       (let ((r (program "a <- b c & d" "")))
         (assert (equal? (caddr (cadr r)) 'and))))
      ("conjunction precedes disjunction"
       (let ((r (program "a <- b & c | d" "")))
         (assert (equal? (caddr (cadr r)) 'alt))))
      ("parentheses"
       (let ((r (program "a <- b (c | d)" "")))
         (assert (equal? (caddr (cadr r)) 'cat))))
      ))
    
  (define language-parser-tests (language-tests-for program)))
