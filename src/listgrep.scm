; TODO comments
(define (sublist? list1 list2)
  (cond ((null? list1) #f) ; Initial list checks
        ((null? list2) #f)
        ((sublist-prime list1 list2) #t) ; Our helper function confirmed a sublist exists.
        (else (sublist? list1 (cdr list2)))))

; TODO comments
(define (sublist-prime list1 list2)
  (cond ((null? list1) #t) ; We have confirmed the full length of list1 inside list2.
        ((null? list2) #f) ; We have exhausted list2.
        ((equal? (car list1) (car list2))
         (sublist-prime (cdr list1) (cdr list2))) ; We continue comparing.
        (else #f))) ; We have a problem.


; TODO comments
(define (lgrep list1 list2)
  (cond ((null? list1) '())
        ((null? list2) '())
        (else (lgrep-prime list1 list2 ))))

; TODO comments
(define (lgrep-prime list1 list2)
  (cond ((null? list2) '())
        ((sublist? list1 (car list2)) (cons (car list2) (lgrep-prime list1 (cdr list2))))
        (else (lgrep-prime list1 (cdr list2)))))

; TODO add tests, cover each case