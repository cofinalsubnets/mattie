(library (test mattie parser combinators)
         (export parser-combinator-tests)
         (import (rnrs)
                 (mattie util)
                 (mattie parser monad)
                 (mattie parser combinators)
                 (test util))

  (define digit (one-of "0123456789"))

  (define-lazy matched-parens (opt (cats "(" matched-parens ")")))

  (define parser-combinator-tests (tests

    ("terminals"
     (assert (language-contains? (term "asdf") "asdf"))
     (assert (not (language-contains? (term "asdf") "qwer")))
     (assert (language-contains? (term "") ""))
     (assert (not (language-contains? (term "") "nonempty"))))

    ("disjunction"
     (assert (for-all (lambda (d) (language-contains? digit d))
                      (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))))

    ("conjunction"
     (assert (language-contains? (conj lang-1 digit) "3"))
     (assert (not (language-contains? (conj lang-f digit) "4"))))

    ("complement and conjunction"
     (define even-digit (alt "0" "2" "4" "6" "8"))
     (define odd-digit (conj (comp even-digit) digit))
     (assert (language-contains? odd-digit "9"))
     (assert (not (language-contains? odd-digit "4"))))

    ("concatenation disjunction repetition"
     (define hex-digit (alt digit (one-of "abcdef")))
     (define hex-number (cats (term "0x") hex-digit (reps hex-digit)))
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

    ("counting"
     (define count-ones
       (reps (fmap (λ (s) (if (string=? s "1") "." ""))
                   (alt "0" "1"))))
     (assert (string=? (val (count-ones "00010100110")) "....")))

    ("packrat parsers only parse a given string once"
     (define count 0)
     (define p (packrat (fmap (λ (x) (set! count (+ 1 count)) x)
                              (term "."))))
     (assert (equal? (p ".") (p ".")))
     (assert (= count 1))))))
