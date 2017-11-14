#lang racket
;; LL1 Parsing & Related Functions
;; By Caleb Currie & Will Nesbitt

;;---------------------BEGIN GIVEN HELPER CODE-------------------------
; Small modifications may be present, and it does not include all
;  code from the skeleton file.
;
; N.B. as per our talk with Dr. Kosa, the grammar structure could be more
;  consistent if the epsilon production was not just the empty list, but
;  rather the list containing the empty list

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

;;---------------------END GIVEN HELPER CODE---------------------------


;;--------------------------OUR FUNCTIONS------------------------------

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

; We interestingly need to define logical OR and AND as functions to be
;  used as first-class values
; Particularly, we ran into this case when trying to foldl with or
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

; Removes all occurence of obj in lst
(define remove
  (lambda (obj lst)
    (cond
      ((null? lst) '())
      ((equal? (car lst) obj) (remove obj (cdr lst)))
      (else
       (cons (car lst) (remove obj (cdr lst)))))))

; Returns set1 - set2
; N.B. if removing epsilon, you need to pass in the set containing epsilon
(define set-difference
  (lambda (set1 set2)
    (cond
      ; can't remove something from nothing
      ((null? set1) '())
      ; if our current element of set 1 is not in set 2, we can return it and continue
      ((not (member (car set1) set2))
       (cons (car set1) (set-difference (cdr set1) set2)))
      (else
       ; otherwise, we need to remove that element (by ignoring it)
       (set-difference (cdr set1) set2)))))

; Checks if the given list contains the specified object
(define contains?
  (lambda (obj lst)
    (not (not (member obj lst)))))

; Epsilon-producing set
; Our assumption is that if a nt is in eps, lambda is in FIRST(nt)
; EPS is defined as any non-terminal that has an epsilon production OR
;  any non-terminal that has a production containing only non-terminals that are all in EPS
; TODO XXX Should we be able to produce the EPS of some given string? e.g. if the given value produces lambda
; I think so, given the bototm of page 88
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

; Lookup to see if the given non-terminal is in the EPS set
(define in-eps?
  (lambda (nt grammar)
    ; member's return value is forced int #t/#f
    (not (not (member nt (eps grammar))))))

; Takes in an x and sees if it will ever produce epsilon given the grammar
(define produces-epsilon?
  (lambda (w grammar)
    ; all non-terminals in the function must
    (foldl and-fn #t
           (map (lambda (nt)
                  (cond
                   ((has-epsilon-production? nt grammar) #t)
                   ((has-non-terminal-only-production? nt grammar)
                    (foldl or-fn #f
                           (map (lambda (prod)
                                  (produces-epsilon? prod grammar))
                                (get-non-terminal-only-rhs-set nt grammar))))
                   (else #f)))
                w))))

; Determines if a given list is comprised of only non-terminals
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
                           (get-rhs-set nt grammar)))))))

; Checks if the given non-terminal has an epsilon production
(define has-epsilon-production?
  (lambda (nt grammar)
    (not (not (member #t
                      (map null? (get-rhs-set nt grammar)))))))

; Returns the set of Right Hand Sides of productions with an LHS of nt
; Assummes nt is a LHS and grammar is in the form of our calculator grammar
;  That is, an association list with all productions on the RHS for a given non-terminal
(define get-rhs-set
  (lambda (nt grammar)
    (if (not (assoc nt grammar))
        '()
        (cdr (assoc nt grammar)))))

; As above, but only the RHSes comprised solely of non-terminals
(define get-non-terminal-only-rhs-set
  (lambda (nt grammar)
    (filter (lambda (w)
              (only-non-terminals? w grammar))
            (get-rhs-set nt grammar))))

; Returns a list of full productions that each have nt as their lhs
(define get-productions
  (lambda (nt grammar)
    (filter (lambda (w)
              (equal? nt (car w)))
            (productions grammar))))

; Returns a list of full productions that each contain nt in their rhs
(define get-productions-with-nt-in-rhs
  (lambda (nt grammar)
    (filter (lambda (w)
              (contains? nt (flatten (cdr w))))
            (productions grammar))))

; Returns the FIRST set for a given x using the given grammar
(define gen-first-set
  (lambda (x grammar epsilon-producing-set)
    (cond
      ; Rule 1 - FIRST(Epsilon) = {Epsilon}
      ((null? x) '(()))
      ; Is it just a terminal? FIRST(x) = {x}
      ((terminal? x grammar) ('(x)))
      ; It is just a non-terminal, and so we apply Rule 3 first
      ((non-terminal? x grammar)
       (flatten
        (map (lambda (prod)
               (gen-first-set prod grammar epsilon-producing-set))
             (get-rhs-set x grammar))))
      (else
       ; Rule 2 - FIRST(aw) = FIRST(a) = {a}
       (if (terminal? (car x) grammar) (list (car x))
           ; Rule 4 - we have a w with two cases
           ; We also assume that lambda is not an element of FIRST(A) if A is in the EPS
           ; FIRST(Aw) = FIRST(A)
           (if (not (in-eps? (car x) grammar)) (gen-first-set (car x) grammar epsilon-producing-set)
               ; FIRST(Aw) = (FIRST(A) - {Epsilon}) U FIRST(w)
               (append ;TODO XXX should be union but having issues with epsilon appearing when taking union
                (set-difference (gen-first-set (car x) grammar epsilon-producing-set) '(()))
                (gen-first-set (cdr x) grammar epsilon-producing-set))))))))

; association list with the first sets for each non-terminal
(define first-sets
  (lambda (grammar)
    (pair (non-terminals grammar)
          (map (lambda (nt)
                 (gen-first-set nt grammar (eps grammar)))
               (non-terminals grammar)))))

; adds $$ into the start symbol's FOLLOW set
(define insert-end-marker
  (lambda (follow-sets)
    (cons (list (caar follow-sets) (append (cadar follow-sets) '("$$")))
          (letrec ((helper (lambda (the-rest)
                             (if (null? (cdr the-rest))
                                 '()
                                 (cons (car the-rest) (cdr the-rest))))))
            (helper (cdr follow-sets))))))

; Gets the last element of a list
(define last-elem
  (lambda (lst)
    (if (null? (cdr lst)) (car lst)
        (last-elem (cdr lst)))))

; Checks if the given element is the rightmost in a list
(define is-rightmost-element?
  (lambda (e lst)
    (equal? (last lst) e)))

; Returns everything after a specified element in a given list
; The special case is once again due to epsilon productions being un-wraped
(define everything-after
  (lambda (obj lst)
    (cond
      ((null? lst) '())
      ((equal? obj (car lst)) (cdr lst))
      ((everything-after obj (cdr lst))))))

; As our rules define how to build follow sets for our non-terminal in the RHS, we build them for it
;TODO add start symbol check
(define gen-follow-set
  (lambda (nt grammar start-symbol)
    ; We have a kludge to solve our duplicate returns
    ; We had issues with epsilon when doing unions, so we use append instead
    (unique
     (sort
      (remove '()
              (flatten
               (letrec ([helper (lambda (nt finished-set)
                                  (map (lambda (prod)
                                         (cond
                                           ((null? prod) '()) ; Epsilon production are not considered
                                           ((contains? nt finished-set) '()) ; We've already looked at FOLLOW(nt)
                                           ; Rule 2
                                           ((is-rightmost-element? nt (flatten (cdr prod)))
                                            (if (equal? nt (car prod)) '()
                                                (helper (car prod) (cons nt finished-set))))
                                           ; Rule 3 and 4
                                           (else
                                            (append ;union TODO
                                             (remove '()
                                                     (gen-first-set
                                                      (everything-after nt (flatten (cdr prod))) grammar (eps grammar)))
                                             (if (contains? '() (gen-first-set
                                                                 (everything-after nt (flatten (cdr prod))) grammar (eps grammar)))
                                                 (if (equal? nt (car prod)) '()
                                                     (helper (car prod) (cons nt finished-set)))
                                                 '())))))
                                       (get-productions-with-nt-in-rhs nt grammar)))])
                 (helper nt '()))))))))

; N.B. should we just compute them on the fly instead of automatically doing this?
; Can also have check in gen-follow-set for if nt is start symbol, insert $$
; XXX TODO SEE ABOVE
; association list with the follow sets for each non-terminal
(define follow-sets
   (lambda (grammar)
     ; To be inserted after we generate everything else
     ;(insert-end-marker
      (pair (non-terminals grammar)
            ; see about expanding this and doing it for just the one line
            (map (lambda (nt)
                   (gen-follow-set nt grammar (start-symbol grammar)))
                 (non-terminals grammar)))))

; returns the PREDICT set for a given production
(define gen-predict-set
  (lambda (prod grammar)
    (sort
     (remove '()
            
             ; FIRST(alpha)
             ; Again, our kludge is present due to union's limitations
             (append
              (gen-first-set (flatten (cdr prod)) grammar (eps grammar))
              ; if EPS(alpha) then FOLLOW(A) else null
              (if (produces-epsilon? (flatten (cdr prod)) grammar)
                  (gen-follow-set (car prod) grammar (start-symbol grammar))
                  '()))))))

; Returns an association list of all productions and their corresponding predict sets
(define predict-sets-over-grammar
  (lambda (grammar)
    (pair (productions grammar)
          (map (lambda (prod)
                 (gen-predict-set prod grammar))
               (productions grammar)))))

; Returns the predict sets for a given non terminal's productions
(define predict-sets
  (lambda (nt grammar)
    (map (lambda (prod)
           (gen-predict-set prod grammar))
         (get-productions nt grammar))))

; Returns an association list of each predict set and rhs it was
;  generated over, given the appropriate structures
(define pair-pred-rhs
  (lambda (predict-sets rhs-sets)
    (if (null? rhs-sets)
        '()
        (cons (pair (car predict-sets) (car rhs-sets))
              (pair-pred-rhs (cdr predict-sets) (cdr rhs-sets))))))

; Generates the parse-table for the given grammar
(define parse-table
  (lambda (grammar)
    (map (lambda (nt)
           (apply list nt (pair (predict-sets nt grammar)
                          (get-rhs-set nt grammar))))
         (non-terminals grammar))))

;;---------------------BEGIN GIVEN PARSING FUNCTIONS----------------------
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
;;-----------------------END GIVEN PARSING FUNCTIONS----------------------


;;----------------------------BEGIN TESTS---------------------------------
"Testing for EPS"
(newline)
(eps calc-gram)
"should be"
'("FT" "SL" "TT")
(newline)
"Testing for FIRST"
(newline)
(gen-first-set "T" calc-gram (eps calc-gram))
"should be"
'("id" "num" "(")
(newline)
(gen-first-set "FT" calc-gram (eps calc-gram))
"should be"
'("*" "/")
(newline)
"Testing for FOLLOW"
(newline)
(gen-follow-set "T" calc-gram (start-symbol calc-gram))
"should be"
'("$$" ")" "+" "-" "id" "read" "write")
(newline)
(gen-follow-set "FT" calc-gram (start-symbol calc-gram))
"should be"
'("$$" ")" "+" "-" "id" "read" "write")
;;----------------------------END TESTS------------------------------------