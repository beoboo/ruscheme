(define (last-pair l)
  (let ((rest (cdr l)))
    (if (null? rest)
      l
      (last-pair rest))))

(display (last-pair (list 23 72 149 34)))
(newline)
