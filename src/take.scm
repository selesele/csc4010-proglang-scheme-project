; Takes the first num elements of the given list.
(define (take list num)
  (cond ((zero? num) '()) ; If we've taken our amount, stop.
        ((null? list) '()) ; Similar case if we've run out of things to take.
        (else (cons (car list) (take (cdr list) (- num 1)))))) ; Otherwise, recurse.
