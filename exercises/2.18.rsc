(define (reverse l)
    (define (iter items res)
        (if (null? items)
            res
            (iter (cdr items) (cons (car items) res))))
    (iter l ()))


(display (reverse (list 23 72 149 34)))
(newline)
