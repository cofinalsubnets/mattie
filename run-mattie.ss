#!/usr/bin/env scheme-script

(import (rnrs) (mattie interpreter))

(define (with-arguments f) (apply f (cdr (command-line))))

(with-arguments (lambda (src in . _)
  (let ((r ((interpret-mattie src 'main) in "")))
    (display r) (newline))))
