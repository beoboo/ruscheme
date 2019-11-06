(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x) (* x x))

((average-damp square) 10)
