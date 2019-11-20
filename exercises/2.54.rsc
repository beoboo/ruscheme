(define (equal? x y)
  (if (and (pair? x) (pair? y))
    (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))
    (eq? x y)))

(display (equal? 1 2))
(newline)

(display (equal? 1 1))
(newline)

(display (equal? (list 1 2 3) (list 1 2 3)))
(newline)

(display (equal? '(1 2 3) '(1 (2) 3)))
(newline)