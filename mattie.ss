(import (rnrs) (mattie interpreter))

(define (read-file path)
  (let* ((p (open-file-input-port path
                                  (file-options)
                                  'block
                                  (make-transcoder (utf-8-codec))))
         (s (get-string-all p)))
    (close-port p)
    s))

(define source-files (cdr (command-line)))
(define source (fold-right string-append "" (map read-file source-files)))

(define interpret (make-interpreter source "main"))

(display (interpret (get-string-all (current-input-port)) ""))
(newline)
(exit 0)
