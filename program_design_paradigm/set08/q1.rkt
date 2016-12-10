;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "08" "q1.rkt")

(provide
 make-def
 make-varexp
 make-appexp
 any-loops?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

;; TEMPLATE
;; def-fn : Def -> ??
;; (define (def-fn d)
;;    (...
;;       (def-name d)
;;       (def-args d)
;;       (def-body d)))

(define-struct varexp (name))
(define-struct appexp (fn args))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; TEMPLATE
;; varexp-fn : Varexp -> ??
;; (define (varexp-fn v)
;;  (...
;;       (varexp-name v)))

;; appexp-fn : Appexp -> ??
;; (define (appexp-fn a)
;;  (...
;;    (appexp-fn a)
;;    (apppexp-args a)))


;; A ListOfDefinition (LOD) is:
;; -- empty
;; -- (cons Definition LOD)

;; TEMPLATE
;; lod-fn: LOD -> ??
;; (define (lod-fn lod)
;;   (cond
;;     [(empty? lod) ...]
;;     [else (...
;;           (def-fn (first lod))
;;           (lod-fn (rest lod)))]))


;; A Node is a Symbol.
(define-struct edge (from to))
;; An Edge is a (make-edge Node Node)
;; INTERPRETATION:
;; from and to are two nodes where "from" is the parent
;; and "to" is its child or successor.

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

;; A SetOfNode is a list of Node without duplicates.
;; A SetOfNode is either
;; - Node
;; - (cons Node LON)

;; A ListOfNode (LON) is:
;; -- empty
;; -- (cons Node LON)

;; TEMPLATE
;; lon-fn: LON -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon) ...]
;;     [else (...
;;           (symbol-fn (first lon))
;;           (lon-fn (rest lon)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
(define (node=? n1 n2) (symbol=? n1 n2))

(define sample-program-no-loops
  (list
   (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
   (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
   
   (make-def 'no-loop (list 'x) (make-varexp 'x))))


(define some-loops
  (list
   (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
   (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'u)
             (make-appexp 'f1 (list (make-appexp 'f4
                                                 (list (make-varexp 'u)
                                                       (make-varexp 'w)))
                                    (make-varexp 'z))))
   (make-def 'f4 (list 'x 'y)
             (make-appexp 'f5
                          (list (make-varexp 'y)
                                (make-varexp 'u))))
   (make-def 'f5 (list 'u)
             (make-appexp 'f2
                          (list (make-appexp 'f3 empty))))
   (make-def 'no-loop (list 'x) (make-varexp 'x))))

(define some-loops1
  (list
   (make-def 'f1 (list 'x) (make-appexp 'f1 '()))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FUNCTION DEFINITIONS

;; successors : Node Graph -> ListofNode
;; GIVEN    : a node and a graph
;; RETURNS  : the set of its immediate successors in the graph
;; EXAMPLES : see test cases.
;; STRATEGY : use HOF map on nodes
(define (successors n1 loe)
  (map 
   edge-to  
   (filter
    ;; Edge -> Boolean
    ;; Returns : true iff there is an edge originating from given node.
    (lambda (e) (node=? (edge-from e) n1))
    loe)))

;;TEST 
(begin-for-test
  (check set-equal?
         (successors 'a (list (make-edge 'a 'b) (make-edge 'a 'c)))
         (list 'b 'c)
         "successors func provided incorrect successors for given node"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all-successors : SetOfNode Graph -> SetOfNode
;; GIVEN    : A set of nodes and a graph
;; RETURNS  : the set of all immediate successors of the given node
;;            from the given graph
;; EXAMPLES : see test cases.
;; STRATEGY : use HOF foldr on nodes
(define (all-successors nodes graph)
  (foldr
   ;; Node ListOfNode -> ListOfNode
   (lambda (node s)
     (set-union
      (successors node graph)
      s))
   empty
   nodes))

;;TEST
(begin-for-test
  (check set-equal?
         (all-successors (list 'f1 'f2) (create-graph some-loops))
         (list 'no-loop 'f1)
         "all-successors returned incorrect successors for the given nodes"))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reachable-from? : SetOfNode SetOfNode Graph
;; GIVEN    : two sets of nodes, 'newest' and 'reached'
;; WHERE    : newest is a subset of nodes.
;; AND      : newest is the most recently added set of nodes
;; RETURNS  : the set of nodes reachable from 'reached'.
;; EXAMPLES : see test cases.
;; STRATEGY : recur on successors of newest that are not already in
;;            recent; halt when no more successors 
;; HALTING MEASURE : the number of graph nodes _not_ in 'reached'

(define (reachable-from? newest nodes graph)
  (local
    ((define candidates (set-diff 
                         (all-successors newest graph)
                         nodes)))
    (cond
      [(empty? candidates) nodes]
      [else (reachable-from?
             candidates
             (append candidates nodes)
             graph)])))

;;TEST
(begin-for-test
  (check set-equal? (reachable-from?
                     (all-successors (list 'f3)
                                     (create-graph some-loops))
                     empty (create-graph some-loops))
                    (list 'f4 'f1 'f3 'f2 'f5 'no-loop)
                    "reachable-from? returned incorrect list of nodes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create-graph : Program -> Graph
;; GIVEN    : A list of Definitions or a Program
;; RETURNS  : The graph consisting of the edges where each edge
;;            denotes a call from parent funtion in the body of def.
;;            to its children function of the same def. body.
;; EXAMPLES : see test cases.
;; STRATEGY : use HOF foldr on nodes
(define (create-graph program)
  (foldr
   ;; Definition Graph -> Graph
   ;; Returns : a graph adding the edges from given definitions body.
   (lambda(d base)
     (if(appexp? (def-body d))
        (append (get-edges-for-def (def-name d) (def-body d)) base)
        base))
   empty program))

;;TEST
(begin-for-test
  (check set-equal? (create-graph sample-program-no-loops)
                    (list (make-edge 'f1 'no-loop)
                          (make-edge 'f2 'f1))
                    "create-graph created incorrect graph"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-edges-for-def : Symbol Expression -> ListOfEdge
;; GIVEN    : A symbol which denotes a function and an expression.
;; RETURNS  : The List of edge where each edge denotes a call from 
;;            parent funtion of the def. to the children function
;;            of the same definition's body. Symbol denotes the parent funtion 
;;            name and Expression has the child funtions.
;; EXAMPLES : see test cases.
;; STRATEGY : use HOF foldr on nodes
;; HALTING MEASURE : Lenght of the expression argument + Length of arguments of
;;                   its child Appexp. The function will terminate when all the
;;                   appexp in given expressions's argument list and all the 
;;                   nested appexp in argument list for appexp in given exp list
;;                   are processed recursively.
(define (get-edges-for-def sym exp)
  (foldr
   ;; Definition Expression -> LOE
   ;; Returns : a list of edge adding the edges from given symbol to
   ;;           functions within the expression's arguments.
   (lambda(x base)
     (if (appexp? x)
         (append (get-edges-for-def sym x) base)
         base))
   
   (cons (make-edge sym (appexp-fn exp))
         empty)
   
   (appexp-args exp)))

;;TEST
(begin-for-test
  (check set-equal? (get-edges-for-def 'f1(make-appexp 'no-loop
                                                       (list (make-varexp 'x))))
                     (list (make-edge 'f1 'no-loop))
                     "get-edges-for-def returned incorrect set of edge"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-loop-present? : Program Graph -> Boolean
;; GIVEN    : a valid SGS program p and a graph.
;; RETURNS  : true iff there is some function f in Program that is called
;;            directly within itself or indirectly from its child functions.
;; EXAMPLES : see test cases.
;; STRATEGY : Use HOF ormap on Program - program.
(define (is-loop-present? program graph)
  (ormap
   ;; Definition -> Boolean
   ;; Returns : true iff the given definition contains a loop
   (lambda(d)
     (has-loop? (def-name d) graph))  
   program))

;;TEST
(begin-for-test
  (check-equal? (is-loop-present? some-loops (create-graph some-loops))
                    true
                    "is-loop-present? returned incorrect results."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; has-loop? : Node Graph -> Boolean
;; GIVEN    : a node denoting some function and a graph.
;; RETURNS  : true iff the given node is a member of
;;            the set of nodes reachable from all successors of given 'node'.
;; EXAMPLES : see test cases.
;; STRATEGY : Combine simpler functions.
;(define (has-loop? n graph)
;  (my-member? n (reachable-from? (all-successors (list n) graph)
;                                 empty graph)))

(define (has-loop? n graph)
  (my-member? n (reachable-from? (successors n graph)
                                 empty graph)))

;;TEST
(begin-for-test
  (check-equal? (has-loop? 'f3 (create-graph some-loops))
                    true
                    "has-loop? returned incorrect result for the given node"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any-loops? : Program -> Boolean
;; GIVEN    : a valid SGS program p (that is, a GS program that obeys the
;;            restrictions listed above).
;; RETURNS  : true iff there is some function f in p that calls itself
;;            either directly or indirectly.
;; EAMPLES  : see test cases.
;; STRATEGY : Call a more general function.
(define  (any-loops? program)
  (is-loop-present? program (create-graph program))) 
 
;;TEST
(begin-for-test
  (check-equal? (any-loops? some-loops) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;END