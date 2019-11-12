(define x (cons (list 1 2) (list 3 4)))

(display (length (list x x)))
(newline)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(display (count-leaves (list x x)))
(newline)

