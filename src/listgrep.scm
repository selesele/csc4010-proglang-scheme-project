;; Sublist finder and lgrep functions for CSC4010 2017 HW4
;; By Caleb Currie & Will Nesbitt

; Verifies if list1 is present somewhere in list2 at least once
; Returns true if list1 is found within list2
; Returns false if not
(define (sublist? list1 list2)
  (cond ((null? list1) #f) ; Initial list checks
        ((null? list2) #f)
        ((sublist-prime list1 list2) #t) ; Our helper function confirmed a sublist exists.
        (else (sublist? list1 (cdr list2)))))

; Helper recursive function for the sublist? function
; Searches for list1 in list2, which is a sublist of list2 from sublist?, based on the current position in list2
; Returns true if found, false if not
(define (sublist-prime list1 list2)
  (cond ((null? list1) #t) ; We have confirmed the full length of list1 inside list2.
        ((null? list2) #f) ; We have exhausted list2.
        ((equal? (car list1) (car list2))
         (sublist-prime (cdr list1) (cdr list2))) ; We continue comparing.
        (else #f))) ; We have a problem.


; List grep tool to search for list1 within the lists of list2
; list1 is the list to search for
; list2 is a list of lists, where each internal list is searched using sublist?
; Returns a list of lists, where each internal list is an internal list from list2 containing list1
(define (lgrep list1 list2)
  (cond ((null? list1) '()) ; Initial list checks
        ((null? list2) '())
        (else (lgrep-prime list1 list2 )))) ; Begin searching in lgrep-prime

; Helper recursive function for lgrep function
; Returns a list of lists, as passed up through lgrep
(define (lgrep-prime list1 list2)
  (cond ((null? list2) '()) ; We are at the end of list2, send the empty list
        ((sublist? list1 (car list2)) (cons (car list2) (lgrep-prime list1 (cdr list2)))) ; Use sublist? to check if list1 is in the current sublist of list2.  If so, recurse
        (else (lgrep-prime list1 (cdr list2))))) ; Otherwise, we still need to keep checking the rest of list2 recursively


"Testing for sublist?"
(sublist? '(1 2 3) '(4 5 9 2 1 2 1 2 3 2 1 3 2 1 2 3 5 6)) ; Should return true
"should be"
#t

(newline)

(sublist? '(5 4 3) '(2 5 5 4 3 6 7 5 4 3)) ; Should return true
"should be"
#t

(newline)

(sublist? '(2 2) '(6 8 1 4 8)) ; Should return false
"should be"
#f

(newline)

(sublist? '(1 2) '(a 4 6 * 5 3 g b 1 2 s)) ; Should return true
"should be"
#t

(newline)


"Testing for lgrep"
(lgrep '(c d e) '((a b c d e f g)(c d c d e)(a b c d)(h i c d e k)(x y z))) ; Should return a lists containing lists 1, 2 and 4
"should be"
'((a b c d e f g) (c d c d e) (h i c d e k))

(newline)

(lgrep '(l i s t) '((l i l i s i)(1 2 l i 3 4)(5 6 l i s l i s t)(0 l i s t t))) ; Should return a list containing lists 3 and 4
"should be"
'((5 6 l i s l i s t) (0 l i s t t))

(newline)

(lgrep '(n 0) '((a b c)(1 2 3)(0 n 1 2)(3 2 1))) ; Should return an empty list
"should be"
'()
