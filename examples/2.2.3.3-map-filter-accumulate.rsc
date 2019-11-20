(define nil (list))

(define (odd? x) (= 1 (remainder x 2)))

;(define (append list1 list2)
;  (if (null? list1)
;      list2
;      (cons (car list1) (append (cdr list1) list2))))
;
(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
      (map proc (cdr items)))))

(display (map square (list 1 2 3 4 5)))
(newline)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
    ((predicate (car sequence))
      (cons (car sequence)
        (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

(display (filter odd? (list 1 2 3 4 5)))
(newline)

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(display (accumulate + 0 (list 1 2 3 4 5)))
(newline)

(display (accumulate * 1 (list 1 2 3 4 5)))
(newline)

(display (accumulate cons nil (list 1 2 3 4 5)))
(newline)

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))
(display (enumerate-interval 2 7))
(newline)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
            (enumerate-tree (cdr tree))))))
(display (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))
(newline)
