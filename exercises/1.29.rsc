(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
      (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (term k) (
                     (define y (f (+ a (* k h))))
                     (cond ((or (zero? k)
                              (= k n)) y)
                       ((even? k) (* 2 y))
                       (else (* 4 y)))))
  (* (/ h 3)
    (sum term 0 inc n)))

(simpson-integral cube 0 1 10)