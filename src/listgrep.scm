; Takes the first num elements of the given list.
; Returns the empty list if
(define (take xs num)
  (cond ((zero? num) '()) ; If we've taken our amount, stop.
        ((null? xs) '()) ; Similar case if we've run out of things to take.
        (else (cons (car xs) (take (cdr xs) (- num 1)))))) ; Otherwise, recurse.

; FIXME does not handle null lists properly. Issue appears to be with take.
; Checks if the second list provided is a sublist of the first,
(define (sublist? list1 list2)
  (cond ((null? list1) (null? list2)) ; Handle the nulls.
        ((equal? (take list1 (length list2)) list2) #t) ; Compare the first elements.
        (else (sublist? (cdr list1) list2)))) ; Otherwise we recurse on the rest.

