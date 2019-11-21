(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (cond ((< x 0) (- x))
    (else x)))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
    1 .0))

(display (sqrt 9))

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1 .0))

(display (golden-ratio))