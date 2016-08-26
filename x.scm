#!/usr/bin/env scheme-script
#!r6rs

(import  (rnrs) (mattie lang))

(define (textual port) (transcoded-port port (make-transcoder (utf-8-codec))))
(define stdout (textual (standard-output-port)))
(define stderr (textual (standard-error-port)))
(define status 0)

(define (err . msgs)
  (set! status 1)
  (for-each (lambda (m) (display m stderr)) msgs))

(define (send-port i o)
  (let ((v (get-bytevector-some i)))
    (or (eof-object? v)
        (begin (put-bytevector o v)
               (send-port i o)))))

(define (safe-open-file-input-port f)
  (guard (x (#t (err "Error opening `" f "'\n") #f))
    (open-file-input-port f)))

(define (cat fname)
  (let ((p (safe-open-file-input-port fname)))
    (if p (begin (send-port p (standard-output-port))
                 (close-port p)))))

(for-each cat (cdr (command-line)))

(exit status)
