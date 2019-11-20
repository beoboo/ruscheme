(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(display (sum-integers 0 10))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(display (sum-cubes 0 10))


(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1 .0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(display (pi-sum 1 1000))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
      (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(display (sum-cubes 0 10))


(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(display (sum-integers 0 10))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1 .0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(display (* 8 (pi-sum 1 1000)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2 .0)) add-dx b)
    dx))

(display (integral cube 0 1 0 .01))
(display (integral cube 0 1 0 .001))
