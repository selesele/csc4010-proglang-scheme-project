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

(define test-gram
  '(("P"  ("SL" "$$"))
    ("SL" ("S" "SL") ())
    ("S"  ("id" ":=" "E") ("read" "id") ("write" "E"))
    ("E"  ("T" "TT"))
    ("T"  ("F" "FT"))
    ("TT" ("ao" "T" "TT") ())
    ("FT" ("mo" "F" "FT") ())
    ("ao" ("+") ("-") ("SL" "TT"))
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


(define non-terminal?
  (lambda (x grammar)
    (not (not (member x (non-terminals grammar))))))
 
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
; redo the defines to have lambda
; They recompute sets on the fly, and so shall we

; We interestingly need to define logical OR and AND as functions to be used as first-class values
; Particularly, we ran into this case when trying to foldr with or
(define or-fn
    (lambda (b1 b2)
      (or b1 b2)))
(define and-fn
    (lambda (b1 b2)
      (and b1 b2)))


; Nests list items each within a list
(define nest
  (lambda (lst)
    (if (null? lst) '()
        (cons (list (car lst)) (nest (cdr lst))))))

; Removes one layer of nesting in a list
(define un-nest
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (car lst)) (un-nest (cdr lst))))))


; Epsilon-producing set
; Our assumption is that if a nt is in eps, lambda is in FIRST(nt)
; EPS is defined as any non-terminal that has an epsilon production OR
;  any non-terminal that has a production containing only non-terminals that are all in EPS
(define eps
  (lambda (grammar)
    (union
     ; all non-terminals with an epsilon production
      (filter (lambda (nt)
                (has-epsilon-production? nt grammar))
              (non-terminals grammar))
     ; unioned with all non-terminals that have a production of all non-terminals that each produce epsilon
      (un-nest
       (filter (lambda (nt)
                (produces-epsilon? nt grammar))
              (nest
               (filter (lambda (nt)
                         (has-non-terminal-only-production? nt grammar))
                       (non-terminals grammar))))))))

; Take a non-terminal
; does it have a nt only prod?
; if so, then check if at least one nt-only prod has only nt's that 



; Takes in an x and sees if it will ever produce epsilon given the grammar
(define produces-epsilon?
  (lambda (w grammar)
    ; all non-terminals in the function must
    (foldr and-fn #t
           (map (lambda (nt)
                  (cond
                   ((has-epsilon-production? nt grammar) #t)
                   ((has-non-terminal-only-production? nt grammar)
                    (foldr or-fn #f
                           (map (lambda (prod)
                                  (produces-epsilon? prod grammar))
                                (get-non-terminal-only-productions nt grammar))))
                   (else #f)))
                w))))




; Determines if a given w is comprised of only non-terminals
; The special case is to deal with the lack of symmetrical definitons in the grammar
; i.e. the production containing epsilon is only the empty list not the list containing the empty list
(define only-non-terminals?
  (lambda (w grammar)
    (if (null? w) #f
        (not (member #f
                     (map (lambda (w)
                            (non-terminal? w grammar))
                          w))))))

; Determines if a given non-terminal has at least one non-terminal only production
(define has-non-terminal-only-production?
  (lambda (nt grammar)
    ; if any production has a non-terminal-only production, we return true
    (not (not (member #t
                      (map (lambda (prod)
                             (only-non-terminals? prod grammar))
                           (get-productions nt grammar)))))))


; Checks if the given non-terminal has an epsilon production
(define has-epsilon-production?
  (lambda (nt grammar)
    (not (not (member #t
                      (map null? (get-productions nt grammar)))))))

; Removes all occurence of obj in lst
(define (remove obj lst)
    (cond
      ((null? lst) '())
      ((equal? (car lst) obj) (remove obj (cdr lst)))
      (else
       (cons (car lst) (remove obj (cdr lst))))))

; Returns set1 - set2
; N.B. if removing epsilon, you need to pass in the set containing epsilon
(define (set-difference set1 set2)
  (cond
    ; can't remove something from nothing
    ((null? set1) '())
    ; if our current element of set 1 is not in set 2, we can return it and continue
    ((not (member (car set1) set2))
     (cons (car set1) (set-difference (cdr set1) set2)))
    (else
    ; otherwise, we need to remove that element (by ignoring it)
     (set-difference (cdr set1) set2))))

; Lookup to see if the given non-terminal is in the EPS set
(define (in-eps? nt grammar)
  ; member's return value is forced int #t/#f
  (not (not (member nt (eps grammar)))))


; Returns the productions with a LHS of nt
; Assummes nt is a LHS and grammar is in the form of our calculator grammar
;  That is, an association list with all productions on the RHS for a given non-terminal
(define (get-productions nt grammar)
   (cdr
    (assoc nt grammar)))

(define get-non-terminal-only-productions
  (lambda (nt grammar)
    (filter (lambda (w)
              (only-non-terminals? w grammar))
            (get-productions nt grammar))))



; Generates the FIRST set for the given non terminal using the given grammar
; TODO confirm unique not needed here
(define (gen-nt-first-set nt grammar)
  ; Rule 3 is applied first
  (flatten
   ; TODO fit this map to have the grammar and eps too
   (map (lambda (nt)
          (gen-x-first-set nt grammar (eps grammar)))
        (get-productions nt grammar))))

; Returns the FIRST set for a given x using the given grammar
(define (gen-x-first-set x grammar epsilon-producing-set)
  (cond
    ; Rule 1 - FIRST(Epsilon) = {Epsilon}
    ((null? x) '())
    ((list? x)
     ; Rule 2 - FIRST(aw) = FIRST(a) = {a}
     (if (terminal? (car x) grammar) (list (car x))
         ; Rule 4 - we have a w with two cases
         ; FIRST(Aw) = FIRST(A)
         (if (not (in-eps? (car x) grammar)) (gen-x-first-set x grammar (eps grammar))
             ; FIRST(Aw) = (FIRST(A) - {Epsilon}) U FIRST(w)
             (union
              (set-difference (gen-x-first-set (car x) grammar epsilon-producing-set) '(()))
              (gen-x-first-set (cdr x) grammar epsilon-producing-set)))))
    ; It is just a terminal - FIRST(x) = {x}
    ((terminal? x grammar) ('(x)))
    (else
     ; It is just a non-terminal, and so we apply rule 3 first
     (gen-nt-first-set x grammar))))


; association list with the first sets for each non-terminal
(define first-sets
  (lambda (grammar)
    (pair non-terminals
          (map (lambda (nt)
                 (gen-nt-first-set nt grammar))
               (non-terminals grammar)))))





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






