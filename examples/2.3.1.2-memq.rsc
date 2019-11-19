(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
;
;(display (eq? 1 1))
;(newline)
;
;(display (eq? 1 2))
;(newline)
;
;(display (eq? 'apple 'banana))
;(newline)
;
;(display (eq? 'apple (car '(apple))))
;(newline)
;
;(display (memq (car (list 1)) (list 1)))
;(newline)
;
;(display (memq 'apple '(pear banana prune)))
;(newline)

;(display (memq 'apple '(x (apple sauce) y apple pear)))
;(newline)
;
(display (memq 'red '((red shoes) (blue socks))))
(newline)

;(display (memq 'red '(red shoes blue socks)))
;(newline)
;