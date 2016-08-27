(library (test mattie parser combinators)
         (export parser-combinator-tests)
         (import (rnrs)
                 (mattie parser combinators)
                 (test util))

  (define digit
    (alt "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

  (define (matched-parens s st)
    ((opt (cat (term "(") matched-parens (term ")"))) s st))

  (define (final-state p s i)
    (let ((r (p s i))) (and r (string=? "" (car r)) (cdr r))))

  (define parser-combinator-tests (tests

    ("terminals"
     (assert (parses (term "asdf") "asdf"))
     (assert (not (parses (term "asdf") "qwer")))
     (assert (parses (term "") ""))
     (assert (not (parses (term "") "nonempty"))))

    ("disjunction"
     (assert (for-all (lambda (d) (parses digit d))
                      (list "1" "2" "3" "4" "5" "6" "7" "8" "9"))))

    ("conjunction"
     (assert (parses (conj lang-1 digit) "3"))
     (assert (not (parses (conj lang-f digit) "4"))))

    ("complement and conjunction"
     (define even-digit (alt "0" "2" "4" "6" "8"))
     (define odd-digit (conj (comp even-digit) digit))
     (assert (parses odd-digit "9"))
     (assert (not (parses odd-digit "4"))))

    ("concatenation disjunction repetition"
     (define hex-digit (alt digit "a" "b" "c" "d" "e" "f"))
     (define hex-number (cat (term "0x") hex-digit (rep hex-digit)))
     (assert (parses hex-number "0x33dead55")))

    ("matching parentheses"
     (assert (parses matched-parens "((()))"))
     (assert (parses matched-parens ""))
     (assert (not (parses matched-parens "(()")))
     (assert (not (parses matched-parens "( )"))))

    ("full/empty languages"
      (assert (parses lang-t "\t6f85be70-a463-441d-b496-627bb0a0f66f:
                              nobody understands me :("))
      (assert (not (parses lang-f ""))))

    ("zero/one languages"
     (assert (parses lang-0 ""))
     (assert (not (parses lang-0 ".")))
     (assert (parses lang-1 "."))
     (assert (not (parses lang-1 ""))))


    ("stateful parsing"
     (define count-ones
       (rep (lmap (lambda (s st)
                    (if (string=? s "1")
                      (string-append "." st)
                      st))
                  (alt "0" "1"))))
     (assert (string=? (final-state count-ones "00010100110" "") "...."))))
    ))