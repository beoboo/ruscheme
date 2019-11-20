(define (equal? a b)
  (or
    (eq? a b)
    (and
      (pair? a)
      (pair? b)
      (equal? (car a) (car b))
      (equal? (cdr a) (cdr b)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (union-set set1 set2)
  (if (null? set1)
    set2
    (let ((e (car set1)))
      (union-set
        (cdr set1)
        (if (element-of-set? e set2) set2 (cons e set2))))))

(define evens (list 0 2 4 6))
(define odds (list 1 3 5 7))

(display (union-set '() '()))
(newline)
(display (union-set '() evens))
(newline)
(display (union-set evens evens))
(newline)
(display (union-set evens odds))
(newline)
(display (union-set odds evens))
(newline)
