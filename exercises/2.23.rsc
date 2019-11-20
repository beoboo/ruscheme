(define nil (list))
(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
      (map proc (cdr items)))))

(define (for-each proc items) (map proc items) true)
(for-each (lambda (x) (newline) (display x))
  (list 57 321 88))

(display "Result: ")
(display res)
(newline)