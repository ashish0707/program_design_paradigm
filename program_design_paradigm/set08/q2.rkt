;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 is-null-derivable?
 make-pos
 make-neg
 make-clause)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

;; A Literal is one of
;; -- (make-pos Variable)  Interp: a literal containing the variable
;; -- (make-neg Variable)  Interp: a literal containing the negation of
;;                                 the variable
(define-struct pos (name))
(define-struct neg (name))

;; TEMPLATE
;; pos-fn : Pos -> ??
;; (define (pos-fn v)
;;  (...
;;       (pos-name v)))

;; neg-fn : Neg -> ??
;; (define (neg-fn v)
;;  (...
;;       (neg-name v)))

;; A Clause is a SetOfLiteral
;; A SetOfLiteral is a list of Literals (LOL) without duplicates.
;; A SetOfLiteral is either
;; - Literal
;; - (cons Node LOL)

;; A ListOfLiteral (LOL) is:
;; -- empty
;; -- (cons Literal LOL)

;; TEMPLATE
;; lol-fn: LOL -> ??
;; (define (lol-fn lol)
;;   (cond
;;     [(empty? lol) ...]
;;     [else (...
;;           (literal-fn (first lol))
;;           (lon-fn (rest lol)))]))


;; A Node is a Clause.
(define-struct edge (from to))
;; An Edge is a (make-edge Node Node)
;; INTERPRETATION:
;; from and to represents the two clauses that have a connection.

;; TEMPLATE
;; edge-fn : Edge -> ??
;; (define (edge-fn e)
;;    (...
;;       (edge-from e)
;;       (edge-to e)))

;; A Graph is ListOfEdge (LOE)
;; A LOE is:
;; -- empty
;; -- (cons Edge LOE)

;; TEMPLATE
;; loe-fn: LOE -> ??
;; (define (loe-fn loe)
;;   (cond
;;     [(empty? loe) ...]
;;     [else (...
;;           (edge-fn (first loe))
;;           (loe-fn (rest loe)))]))

;; INFORMATION ANALYSIS
;; Resolution Rule : a formula form is unsatisfiable if and only if the empty
;;                   clause is derivable from the formula.
;; List Of Compliment : is a list of literal where each literal and its
;;                      negation is present in first and second clause resp.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS


(define clause1 (list (make-pos 'a)
                      (make-neg 'b)
                      (make-pos 'c)))

(define clause2 (list (make-pos 'd)
                      (make-pos 'b)))

(define clause3 (list (make-neg 'a)
                      (make-pos 'c)))

(define clause4 (list (make-pos 'b)))

(define clause5 (list (make-neg 'c)))

(define sample-list (list clause1 clause2 clause3 clause4 clause5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DEFINITIONS

;; make-clause : ListOfLiteral -> Clause
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a clause containing set of literals
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF foldr on LOL - list_of_literal.
(define (make-clause list_of_literal)
  (foldr
   ;; Literal, SetOfLiteral -> SetOfLiteral
   ;; RETUNRS : set of literal appending literal from given list iff
   ;; not already present in list.
   (lambda(x base)
     (set-cons x base)) empty list_of_literal))

;;TEST 
(begin-for-test
  (check-equal? (make-clause (list (make-pos 'a) (make-pos 'a) (make-pos 'b)))
                 (list (make-pos 'a) (make-pos 'b))
                 "Make clause might have duplicated the literal"))
                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-null-derivable? : ListOfClause -> Boolean
;; GIVEN    : a list of clauses
;; RETURNS  : true iff either the any clause in given listOfClause is empty or
;;            emptyclause is derivable from the given clauses.
;; EXAMPLES : see test cases.
;; STRATEGY : Check if the ListOfClause or any clause within is empty.
;;            If not, Call a function to resolve ListOfClause.

(define (is-null-derivable? loc)
  
  (cond
    [(empty? loc) false]
    [(any-clause-empty? loc) true]
    [else
     (is-resolvable? loc empty)]))

;;TEST 
(begin-for-test
  (check-equal? (is-null-derivable? sample-list) true
                 "is-null-derivable? returned incorrect result for given list
                 check resolution of clauses for error")
  (check-equal? (is-null-derivable? (list (make-clause empty))) true
                 "is-null-derivable? returned incorrect result for given list
                 where a clause is empty")
  (check-equal? (is-null-derivable? empty) false
                 "is-null-derivable? returned incorrect result for empty list"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-resolvable? : ListOfClause Graph -> Boolean
;; GIVEN    : a list of clauses and a graph.
;; RETURNS  : true iff the empty clause is derivable from the given
;;            clauses using the rule of resolution as given above.
;; WHERE    : the graph contains the bi-directional edges between the
;;            resolved clauses in the previous iteration.
;;            Value of the graph for first iteration is empty.
;; EXAMPLES : see test cases.
;; STRATEGY : Divide into cases on list of resolved clauses
;;            for the given ListOfClause.
;; HALTING MEASURE : The function halts when 'empty' is a member of list of 
;;                   resolved clauses derived for given ListOfClause or when
;;                   list of resolved clauses is subset of the given
;;                   ListOfClause.
(define (is-resolvable? loc graph)
  
  (local
    ((define new-resolutions  
       (get-resolved-clauses-for-list loc graph)))
    (cond
      [(my-member? empty new-resolutions) true]
      [(subset? new-resolutions loc) false]
      [else (is-resolvable?
             (set-union new-resolutions loc) graph)])))

;;TEST 
(begin-for-test
  (check-equal? (is-resolvable? sample-list empty) true
                 "resolve-clauses returned incorrect result for given list"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any-clause-empty? : ListOfClause -> Boolean
;; GIVEN    : a list of clause.
;; RETURNS  : true iff any clause is empty
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF ormap on ListOfClause - loc.
(define (any-clause-empty? loc)
  (ormap
   ;; Clause -> Boolean
   ;; Returns : true iff the clause if empty
   (lambda(x)
     (empty? x)) loc))

;;TEST 
(begin-for-test
  (check-equal? (any-clause-empty? (list (list (make-pos 'a)) empty)) true
                 "any-clause-empty? returned incorrect results for empty
                 clause"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-resolved-clauses-for-list : ListOfClause Graph -> ListOfClause
;; GIVEN    : a list of clauses and a graph.
;; RETURNS  : a list of clause resulting from resolution of the given
;;            clauses
;; WHERE    : graph gets updated with bi-direction edge between clause
;;            being resolved and LOC given, in every subsequent call.
;; EXAMPLES : see test cases.
;; STRATEGY : Divide into cases on length of ListOfClause - loc.
;; HALTING MEASURE : Length of the ListOfClause - loc.

(define (get-resolved-clauses-for-list loc graph)
  (cond
    [(empty? loc) empty]
    [else
     (append
      (get-resolved-clauses-for-clause (first loc) loc graph)
      (get-resolved-clauses-for-list (rest loc)
                               (add-edges-to-graph (first loc)
                                                   loc
                                                   graph)))]))

;;TEST 
(begin-for-test
  (check-equal? (get-resolved-clauses-for-list (list clause1 clause2) empty)
                (list (list (make-pos 'a) (make-pos 'c) (make-pos 'd)))
                "get-list-of-resolutions returned incorrect clause list "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-edges-to-graph : Clause ListOfClause Graph -> Graph
;; GIVEN    : a clause, a list of clauses and a graph.
;; RETURNS  : same graph as given but appending an edge from given
;;            clause to all clauses in given list.
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF on length of ListOfClause - loc.

(define (add-edges-to-graph clause ListOfClause graph)
  (foldr
   ;; Clause Graph -> Graph
   ;; Returns : same graph as given but appending an bi-directional edge
   ;; between given clause and a clause from ListOfClauses 
   (lambda(c base)
     (cons (make-edge c clause)
           (cons (make-edge clause c) base))) graph ListOfClause))

;;TEST 
(begin-for-test
  (check-equal? (add-edges-to-graph clause1 (list clause2 clause3) empty)
                (list
                 (make-edge
                  (list (make-pos 'd) (make-pos 'b))
                  (list (make-pos 'a) (make-neg 'b) (make-pos 'c)))
                 (make-edge
                  (list (make-pos 'a) (make-neg 'b) (make-pos 'c))
                  (list (make-pos 'd) (make-pos 'b)))
                 (make-edge
                  (list (make-neg 'a) (make-pos 'c))
                  (list (make-pos 'a) (make-neg 'b) (make-pos 'c)))
                 (make-edge
                  (list (make-pos 'a) (make-neg 'b) (make-pos 'c))
                  (list (make-neg 'a) (make-pos 'c))))
                "add-edges-to-graph added incorrect edges to graph"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-resolved-clauses-for-clause : Clause ListOfClause Graph -> ListOfClause
;; GIVEN    : a clause, a list of clauses and a graph.
;; RETURNS  : the list of resolution of given clause with the clauses
;;            in the given list. Resolution of clauses is skipped if an
;;            edge is present in the graph.
;; WHERE    : the graph contains the bi-directional edges between the
;;            previous resolved clauses and given list of clauses.
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF Foldr on ListOfClause - loc.
(define (get-resolved-clauses-for-clause clause loc graph)
  (foldr
   ;; Clause ListOfClause -> ListOfClause
   ;; Returns : the list of resolved clauses for the given clause
   ;;           and a clause from LOC.
   (lambda(c base)
     (if (or (equal? c clause) (my-member? (make-edge clause c) graph))
         base
         (append (resolution clause c) base)))
   
   empty loc))

;;TEST 
(begin-for-test
  (check-equal? (get-resolved-clauses-for-clause clause1 (list clause2 clause3)
                                                 empty)
                (list
                 (list (make-pos 'a) (make-pos 'c) (make-pos 'd))
                 (list (make-neg 'b) (make-pos 'c)))
                "get-resolved-clauses-for-clause returned incorrect list of
                 clauses"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; resolution : Clause Clause -> ListOfClauses
;; GIVEN    : two clause to be reduced if it contains compliments of literals.
;; RETURNS  : list of clauses created applying resolution rule.
;; EXAMPLES : see test cases.
;; STRATEGY : Divide on cases for length of ComplementList - complement-list.
(define (resolution clause1 clause2)
  
  (local
    ((define complement-list 
       (find-complements clause1 clause2)))
    
    (if (> (length complement-list) 0)
        (filter-clauses-having-compliments-within
         (create-new-clauses complement-list clause1 clause2))
        (list clause1 clause2))))

;;TEST 
(begin-for-test
  (check-equal?  (resolution clause1 clause2)
                 (list (list (make-pos 'a) (make-pos 'c) (make-pos 'd)))
                 "incorrect resolution of two clauses generated"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; filter-clauses-having-compliments-within : ListOfClause -> ListOfClauses
;; GIVEN    : a list of clause
;; RETURNS  : a list of clause eliminating the clauses that contains
;;            a literal and its negation within itself.
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF filter on ListOfClause - loc
(define (filter-clauses-having-compliments-within loc)

  (filter
         ;; Clause -> Boolean
         ;; Returns : true iff the clause contains no complimentary pair
         ;;           pair of literals.
         (lambda(c)
           (not (clause-contains-compliment? c)))loc))

;;TEST 
(begin-for-test
  (check-equal? (filter-clauses-having-compliments-within
                 (list
                  (list (make-pos 'a) (make-neg 'a))
                  (list (make-neg 'b) (make-pos 'c))))
                (list (list (make-neg 'b) (make-pos 'c)))
                "incorrect clause filteration done by
                 filter-clauses-having-compliments-within"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;; clause-contains-compliment? : Clause -> Boolean
;; GIVEN    : a Clause
;; RETURNS  : true iff the clauses contains
;;            a literal and its negation within itself.
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF ormap on Clause - clause
(define (clause-contains-compliment? clause)
  (ormap
         ;; Literal -> Boolean
         ;; Returns : true iff the compliment of the given literal is
         ;;           present in the clause.
         (lambda(x)
           (is-compliment-present? x clause)) clause))

;;TEST 
(begin-for-test
  (check-equal? (clause-contains-compliment? (list (make-pos 'a) (make-neg 'a)))
                true
                "clause-contains-compliment? returned incorrect result"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;; create-new-clauses : ListOfLiteral Clause Clause -> ListOfClause
;; GIVEN    : a list of literal and two Clauses.
;; RETURNS  : a list of clause by removing the compliments from clauses one at
;;            a time and then appending them.
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF foldr on complement-list
(define (create-new-clauses complement-list clause1 clause2)
  (foldr
   ;; Literal LOC -> LOC
   ;; RETURNS  : a clause by removing the literal and its negation
   ;;             from clause1 and clause2 resp. and then appending them.
   (lambda(literal base)
     (cons
      (set-union (get-clause-without-compliment literal clause1)
                          (get-clause-without-compliment (negate literal)
                                                         clause2))
      base))
   
   empty complement-list))

;;TEST 
(begin-for-test
  (check-equal? (create-new-clauses (list (make-pos 'a)) clause1 clause2)
                (list (list (make-neg 'b) (make-pos 'c) (make-pos 'd)
                            (make-pos 'b)))
                "create-new-clauses created incorrect clauses."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; negate   : Literal -> Literal
;; GIVEN    : A Literal
;; RETURNS  : The negation of given literal
;; EXAMPLES : see test cases.
;; STRATEGY : Use template on literal.
(define (negate literal)
  (if (pos? literal)
      (make-neg (pos-name literal))
      (make-pos (neg-name literal))))

;;TEST 
(begin-for-test
  (check-equal? (negate (make-neg 'b)) (make-pos 'b)
                "negate performed incorrect negation of given literal"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-clause-without-compliment   : Literal Clause -> Clause
;; GIVEN    : A Literal and a clause.
;; RETURNS  : The clause without the given literal
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF filter on clause.
(define (get-clause-without-compliment lit clause)
  (filter
   ;; Literal -> Boolean
   ;; Returns : true iff the given literal does not match
   ;;           the literal from the clause.
   (lambda(x)
          (not (equal? x lit))) clause))

;;TEST 
(begin-for-test
  (check-equal? (get-clause-without-compliment (make-pos 'a) clause1)
                (list (make-neg 'b) (make-pos 'c))
                "get-clause-without-compliment"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find-complements   : Clause Clause -> ListOfLiteral
;; GIVEN    : two clauses
;; RETURNS  : the list of complimentary literal in two clauses.
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF Foldr on first given clause.
(define (find-complements clause1 clause2)  
  (foldr
   ;; Literal LOL -> LOL
   ;; Returns : a list of complimentary literal in two clauses.
   (lambda(l base)
     (if (is-compliment-present? l clause2)         
         (cons l base)
         base))
   
   empty clause1))

;;TEST 
(begin-for-test
  (check-equal? (find-complements clause1 clause2)  (list (make-neg 'b))
                "find-complements returned incorrect results"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-compliment-present?   : Literal Clause -> Boolean
;; GIVEN    : a Clause and a Literal.
;; RETURNS  : true iff the compliment of literal is present in clause.
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF ormap on first clause.          
(define  (is-compliment-present? l clause)   
  (ormap
   ;; Literal -> Boolean
   ;; Returns : true iff the literal is complement of given literal.
   (lambda (l1)
     (is-complement? l l1)) clause))

;;TEST 
(begin-for-test
  (check-equal? (is-compliment-present? (make-pos 'a) clause1) false
                "is-compliment-present? returned incorrect result"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-complement?   : Literal Literal -> Boolean
;; GIVEN    : two Literal.
;; RETURNS  : true iff the two literals are complement of each other.
;; EXAMPLES : see test cases.
;; STRATEGY : Divide on cases on type of template         
(define (is-complement? c x)
  (cond
    [(pos? c) (and (neg? x)
                       (equal? (pos-name c) (neg-name x)))]
    [(neg? c) (and (pos? x)
                       (equal? (neg-name c) (pos-name x)))]))

;;TEST 
(begin-for-test
  (check-equal? (is-complement? (make-pos 'a) (make-neg 'a)) true
                "is-complement? returned incorrect result for given set
                 of literals"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS

(define sample-list2
  (list (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
        (make-clause (list (make-neg 'a) (make-neg 'd)))
        (make-clause (list (make-pos 'd)))
        (make-clause (list (make-neg 'd) (make-pos 'c)))
        (make-clause (list (make-neg 'b)))))

(define sample-list3
  (list (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
        (make-clause (list (make-neg 'a) (make-neg 'b) (make-pos 'c)))
        (make-clause (list (make-neg 'a) (make-neg 'b) (make-pos 'c)))))

(begin-for-test
  (check-equal?
   (is-null-derivable? sample-list2) false
    "is-null-derivable? returned incorrect boolean value for the given list")
 
  (check-equal?
   (is-null-derivable? sample-list3) false
     "is-null-derivable? returned incorrect boolean value for the given list"))






