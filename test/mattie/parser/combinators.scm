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
     (assert (language-contains? (term "asdf") "asdf"))
     (assert (not (language-contains? (term "asdf") "qwer")))
     (assert (language-contains? (term "") ""))
     (assert (not (language-contains? (term "") "nonempty"))))

    ("disjunction"
     (assert (for-all (lambda (d) (language-contains? digit d))
                      (list "1" "2" "3" "4" "5" "6" "7" "8" "9"))))

    ("conjunction"
     (assert (language-contains? (conj lang-1 digit) "3"))
     (assert (not (language-contains? (conj lang-f digit) "4"))))

    ("complement and conjunction"
     (define even-digit (alt "0" "2" "4" "6" "8"))
     (define odd-digit (conj (comp even-digit) digit))
     (assert (language-contains? odd-digit "9"))
     (assert (not (language-contains? odd-digit "4"))))

    ("concatenation disjunction repetition"
     (define hex-digit (alt digit "a" "b" "c" "d" "e" "f"))
     (define hex-number (cat (term "0x") hex-digit (rep hex-digit)))
     (assert (language-contains? hex-number "0x33dead55")))

    ("matching parentheses"
     (assert (language-contains? matched-parens "((()))"))
     (assert (language-contains? matched-parens ""))
     (assert (not (language-contains? matched-parens "(()")))
     (assert (not (language-contains? matched-parens "( )"))))

    ("full/empty languages"
      (assert (language-contains? lang-t
                "\t6f85be70-a463-441d-b496-627bb0a0f66f:
                              nobody understands me :("))
      (assert (not (language-contains? lang-f ""))))

    ("zero/one languages"
     (assert (language-contains? lang-0 ""))
     (assert (not (language-contains? lang-0 ".")))
     (assert (language-contains? lang-1 "."))
     (assert (not (language-contains? lang-1 ""))))

    ("state threading w/ lmap"
     (define count-ones
       (rep (lmap (lambda (s st)
                    (if (string=? s "1")
                      (string-append "." st)
                      st))
                  (alt "0" "1"))))
     (assert (string=? (final-state count-ones "00010100110" "") "....")))

    ("lmapped fns not called when running stateless"
     (define ran-stateless #t)
     (define p (lmap (lambda (s _)
                       (set! ran-stateless #f)
                       (string->number s))
                     (cat digit (rep digit))))
     (assert (string=? "" (run-stateless p "12345")))
     (assert ran-stateless))

    )))
