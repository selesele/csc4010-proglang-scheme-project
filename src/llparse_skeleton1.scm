;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LL parser in Scheme.
;; 
;; Written by Michael L. Scott for CSC 254, September 2004
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; This first section of the file contains utility routines
;; that you may find helpful.  I recommend you read all the
;; routines carefully.  Understanding them (really, deeply
;; understanding them) will help you establish the mindset
;; you need to write the rest of the code.
;;
#lang racket

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

(define right-context
  (lambda (B grammar)
    ; Return a list of pairs.
    ; Each pair consists of a symbol A and a list of symbols beta
    ; such that for some alpha, A -> alpha B beta.
    (apply append
           (map
            (lambda (prod)
              (letrec
                  ((helper
                    (lambda (subtotal rhs)
                      (let ((suffix (member B rhs)))
                        (if suffix
                            (helper (cons (cons (car prod)
                                                (list (cdr suffix))) 
                                          subtotal)
                                    (cdr suffix))
                            subtotal)))))
                (helper '() (cadr prod))))
            (productions grammar)))))

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

;;
;; Much of the following employs a "knowledge" structure.
;; It's a list of 4-tuples, one for each non-terminal,
;;    in the same order those non-terminals appear in the grammar
;;    (the order is important).
;; You are not required to organize things this way, but it's a
;; reasonable approach if you don't have other ideas.
;;
;; The fields of the 4-tuple are:
;;   car     non-terminal A [not needed computationally,
;;           but included for readability of output]
;;   cadr    Boolean: do we currently think A -->* epsilon
;;   caddr   (current guess at) FIRST(A) - {epsilon}
;;   cadddr  (current guess at) FOLLOW(A) - {epsilon}
;;

(define initial-knowledge
  (lambda (grammar)
    ; Return knowledge structure with empty FIRST and FOLLOW sets
    ; and false gen-epsilon estimate for all symbols.
    (map (lambda (A) (list A #f '() '()))
         (non-terminals grammar))))

(define symbol-knowledge
  (lambda (A knowledge)
    ; Return knowledge vector for A.
    (assoc A knowledge)))

(define generates-epsilon?
  (lambda (w knowledge grammar)
    ; Can w generate epsilon based on current estimates?
    ; if w is a terminal, no
    ; if w is a non-terminal, look it up
    ; if w is an empty list, yes
    ; if w is a non-empty list, "iterate" over elements

      ;;; your code here
    '() ;; added by MJK

    ))

(define first
  (lambda (w knowledge grammar)
    ; Return FIRST(w) - {epsilon}, based on current estimates.
    ; if w is a terminal, return (w)
    ; if w is a non-terminal, look it up
    ; if w is an empty list, return ()  [empty set]
    ; if w is a non-empty list, "iterate" over elements

      ;;; your code here
    '() ;; added by MJK
    ))

(define follow
  (lambda (A knowledge)
    ; Return FOLLOW(A) - {epsilon}, based on current estimates.
    ; Simply look it up.
    (cadddr (symbol-knowledge A knowledge))))

(define get-knowledge
  (lambda (grammar)
    ; Return knowledge structure for grammar.
    ; Start with (initial-knowledge grammar) and "iterate", until
    ; the structure doesn't change.
    ; Uses (right-context B grammar), for all non-terminals B,
    ; to help compute follow sets.

      ;;; your code here
     '() ;; added by MJK
    ))

(define parse-table
  (lambda (grammar)
    ; Return parse table for grammar.
    ; Table looks like the grammar, except that each RHS is replaced with a
    ; (predict-set RHS) pair.
    ; My version uses the get-knowledge routine above.

      ;;; your code here
      '() ;; added by MJK
    ))

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
