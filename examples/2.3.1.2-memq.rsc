(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))



(display (memq 'apple '(pear banana prune)))
(newline)

(display (memq 'apple '(x (apple sauce) y apple pear)))
(newline)

(display (memq 'red '((red shoes) (blue socks))))
(newline)

(display (memq 'red '(red shoes blue socks)))
(newline)
