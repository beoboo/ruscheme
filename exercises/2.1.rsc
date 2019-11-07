(define (neg? x) (< x 0))

(define (abs x) (if (neg? x) (- x) x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (signed-n (if (neg? d) (- n) n))
        (signed-d (abs d)))
    (cons (/ signed-n g) (/ signed-d g))))

(display (make-rat -1 2))
(display (make-rat 1 -2))
(display (make-rat -1 -2))