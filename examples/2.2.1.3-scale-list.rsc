(define nil (list))

(define (scale-list items factor)
  (if (null? items)
    nil
    (cons (* (car items) factor)
      (scale-list (cdr items) factor))))

(display (scale-list (list 1 2 3 4 5) 10))
(newline)