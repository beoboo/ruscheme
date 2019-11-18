(define a 1)

(define b 2)

(display (list a b))
(newline)

(display (list 'a 'b))
(newline)

(display (list 'a b))
(newline)

(display (list 'a 'b 'c))
(newline)

(display (list (list 'george)))
(newline)

(display (cdr '((x1 x2) (y1 y2))))
(newline)

(display (cadr '((x1 x2) (y1 y2))))
(newline)

(display (pair? (car '(a short list))))
(newline)
