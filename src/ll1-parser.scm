#lang racket

;;
;; Here is our good old friend the calculator language,
;; in the form you are required to accept as input.
;; You may want to create some additional grammars to test with.
;; We will certainly create additional ones to grade with :-)
;;

(define calc-gram
  '(("P"  ("SL" "$$"))
    ("SL" ("S" "SL") ())
    ("S"  ("id" ":=" "E") ("read" "id") ("write" "E"))
    ("E"  ("T" "TT"))
    ("T"  ("F" "FT"))
    ("TT" ("ao" "T" "TT") ())
    ("FT" ("mo" "F" "FT") ())
    ("ao" ("+") ("-"))
    ("mo" ("*") ("/"))
    ("F"  ("id") ("num") ("(" "E" ")"))
    ))




; Helper Functions

; Takes in two lists of lists and pairs (into an association list)
   ; each corresponding element
; Pairs up until one list runs out - the remainder of the longer list is dropped
(define (pair lst1 lst2)
  (cond
    ((null? lst1) '())
    ((null? lst2) '())
    (else
     (cons (list (car lst1) (car lst2))
           (pair (cdr lst1) (cdr lst2))))))

; KOSA/SCOTT HELPER FUNCTIONS
(define sort
  (lambda (L)
    ; Use string comparison to quicksort list.
    (letrec ((partition
              (lambda (e L A B)
                (if (null? L) (cons A B)
                    (let ((c (car L)))
                      (if (string<? c e)
                          (partition e (cdr L) (cons c A) B)
                          (partition e (cdr L) A (cons c B))))))))
      (cond
        ((null? L) L)
        ((null? (cdr L)) L)
        (else (let* ((AB (partition (car L) (cdr L) '() '()))
                     (A (car AB))
                     (B (cdr AB)))
                (append (sort A)
                        (list (car L))
                        (sort B))))))))

(define unique
  (lambda (L)
    ; Return list in which adjacent equal elements have been combined.
    (cond
      ((null? L) L)
      ((null? (cdr L)) L)
      ((equal? (car L) (cadr L)) (unique (cdr L)))
      (else (cons (car L) (unique (cdr L)))))))

(define unique-sort
  (lambda (L)
    ; Sort (using string-ified elements) and remove duplicates.
    (unique (sort L))))

(define flatten
  (lambda (L)
    ; Return left-to-right fringe of tree as list.
    (cond
      ((null? L) L)
      ((list? (car L)) (append (flatten (car L)) (flatten (cdr L))))
      (else (cons (car L) (flatten (cdr L)))))))

(define start-symbol
  (lambda (grammar)
    (caar grammar)))

(define last
  (lambda (L)
    ; Return last element of list.
    (cond
      ((null? L) '())
      ((null? (cdr L)) (car L))
      (else (last (cdr L))))))

(define end-marker
  (lambda (grammar)
    (last (cadar grammar))))

(define non-terminals
  (lambda (grammar)
    ; Return list of all non-terminals in grammar.
    (map car grammar)))

(define gsymbols
  (lambda (grammar)
    ; Return list of all symbols in grammar (no duplicates).
    (unique-sort (flatten grammar))))

(define terminals
  (lambda (grammar)
    ; Return list of all terminals in grammar.
    (apply append
           (map (lambda (x) (if (non-terminal? x grammar) '() (list x)))
                (gsymbols grammar)))))

(define productions
  (lambda (grammar)
    ; Return list of all productions in grammar.
    ; Each is represented as a (lhs rhs) pair, where rhs is a list.
    (apply append
           (map (lambda (prods)
                  (map (lambda (rhs)
                         (list (car prods) rhs))
                       (cdr prods)))
                grammar))))

; SHOULD THIS ALSO CHECK IF MEMBER OF GSYMBOLS?
(define non-terminal?
  (lambda (x grammar)
    ; Is x a non-terminal?
    (not (not (member x (non-terminals grammar))))))
    ; 'not not' makes return type a boolean, not a list

(define gsymbol?
  (lambda (x grammar)
    ; is x a symbol in grammar?
    ; (note that name symbol? is taken by Scheme)
    (not (not (member x (gsymbols grammar))))))
    ; 'not not' makes return type a boolean, not a list

(define terminal?
  (lambda (x grammar)
    ; Is x a terminal in grammar?
    (and (member x (gsymbols grammar))
         (not (member x (non-terminals grammar))))))

(define union
  (lambda sets
    (unique-sort (apply append sets))))


;-----------------------OUR FUNCTIONS---------------------------
; change non-terminals to get-non-terminals

(define (get-productions nt grammar)
   ;(filter (lambda (lst)
    ;        (not (null? lst)))
   (map (lambda (prod)
         (if (eq? nt (car prod)) (cdr prod) '()))
       grammar))



; Generates the FIRST set for the given non terminal using the given grammar
; TODO confirm unique not needed here
(define (gen-first-set nt grammar)
  (flatten (map gen-first-set-prime (get-productions nt grammar))))

(define (gen-first-set-prime x grammar)
  (cond
    ((null? x) '())
    ((list? x) (if (non-terminal? (car x)) (car x)
                   '()))))



;(define first-sets
 ; (pair non-terminals (map gen-first-set non-terminals)))





(define parse-table
  (lambda (grammar)
    ; Return parse table for grammar.
    ; Table looks like the grammar, except that each RHS is replaced with a
    ; (predict-set RHS) pair.
    ; My version uses the get-knowledge routine above.

      ;;; your code here
      '() ;; added by MJK
    ))


; PARSING TESTING FUNCTIONS

(define lookup
  (lambda (nt t parse-tab)
    ; Double-index to find prediction for non-terminal nt and terminal t.
    ; Return #f if not found.
    (letrec ((helper
              (lambda (L)
                (cond
                  ((null? L) #f)
                  ((member t (caar L)) (cadar L))
                  (else (helper (cdr L)))))))
      (helper (cdr (assoc nt parse-tab))))))

(define display-list
  (lambda (L)
    ; Print list to standard output.
    ; Yes, this is imperative.
    (if (not (null? L))
        (begin (display (string-append " " (car L))) (display-list (cdr L)))
        (display '()) ;; added by MJK
                 )))

(define parse
  (lambda (grammar input)
    ; Parse input according to grammar.
    ; Print predictions and matches (imperatively) along the way.
    ; You can also print the stack and remaining input at each step,
    ; if you want: simply uncomment the ';;;' line below
    ; Return #t if the input is in the language; #f if it's not.
    (letrec
        ((die (lambda (s) (begin (display "syntax error: ") (display s) (newline) #f)))
         (parse-tab (parse-table grammar))
         (helper
          (lambda (stack input)
            (begin
;;;           (display stack) (display input) (newline)
              (cond
                ((null? stack)
                 (or (null? input) (die "extra input beyond end of program")))
                ((terminal? (car stack) grammar)
                 (if (equal? (car stack) (car input))
                     (begin
                       (display (string-append "   match " (car input)))
                       (newline)
                       (helper (cdr stack) (cdr input)))
                     (die (string-append "expected " (car stack) "; saw " (car input)))))
                (else ; non-terminal
                 (let ((rhs (lookup (car stack) (car input) parse-tab)))
                   (if rhs
                       (begin
                         (display (string-append "   predict " (car stack) " ->"))
                         (display-list rhs)
                         (newline)
                         (helper (append rhs (cdr stack)) input))
                       (die (string-append "no prediction for " (car stack)
                                           " when seeing " (car input)))))))))))
      (helper (list (start-symbol grammar)) input))))
