;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname draft-06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; dimensions of the canvas
(define CANVAS-WIDTH 500)
(define INITIAL-VEL 0)
(define INITIAL-MOUSE-X -1)
(define INITIAL-MOUSE-Y -1)
(define CANVAS-HEIGHT 400)
(define TEXT_HEIGHT 10)
(define CIRCLE-RADIUS 20)
(define SIMLPE-CIRCLE (circle CIRCLE-RADIUS "outline" "green"))
(define SELECTED-CIRCLE (circle CIRCLE-RADIUS "solid" "green"))
(define SQUARE-RADIUS 20)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define SIMPLE-SQUARE (square SQUARE-RADIUS "outline" "green"))
(define SELECTED-SQUARE (square SQUARE-RADIUS "solid" "green"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-struct leaf  (shape position selected? parent))
;; A leaf is a (make-leaf Boolean Posn Boolean Tree)

(define-struct node (shape position selected? lson rson parent))
;; A leaf is a (make-node Boolean Posn Boolean Tree Tree Tree)

;; A Tree is either
;; -- empty
;; -- (make-node Boolean Posn Boolean Tree Tree Tree) 

;; Template: ******************************************TBD**********
;; tree-fn : Tree -> ???
;; (define (tree-fn t)
;;   (cond
;;     [(leaf? t) (... (leaf-datum t))]
;;     [else (...
;;             (tree-fn (node-lson t))
;;             (tree-fn (node-rson t)))]))

(define-struct world (listOfTree))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFINITIONS FOR KEY EVENTS
(define delete-node-event "d")
(define new-cicle-key-event "c")
(define new-square-key-event "s")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;initial-world : Any -> World
;GIVEN: any value
;RETURNS: an initial world.  The given value is ignored.
(define (initial-world any)
  (make-world empty))

;run :  Any -> World
;GIVEN: any value
;EFFECT: runs a copy of an initial world
;RETURNS: the final state of the world.  The given value is ignored.
;


;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN    : a world and a description of a mouse event
;; RETURNS  : the world that should follow the given mouse event
;; STRATEGY : use template for World on w
#;(define (world-after-mouse-event w x y event)
  (make-world
   (trees-after-mouse-event (world-listOfTree w) x y event)))

;; trees-after-mouse-event : LOT Integer Integer MouseEvent -> LOT
;; GIVEN: a circle list and a description of a mouse event
;; RETURNS: the list of trees that should follow the given mouse event
;; examples:  See test cases below
;; strategy:  Cases on KeyEvent mev
#;(define (circles-after-mouse-event lst mx my mev)
  (cond
    [(mouse=? mev "button-down") (trees-after-button-down lst mx my)]
    ;[(mouse=? mev "drag") (trees-after-drag lst mx my)]
    ;[(mouse=? mev "button-up") (trees-after-button-up lst mx my)]))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; trees-after-button-down : LOT Integer Integer -> LOT
;; RETURNS: the list of trees following a button-down at the given location.
;; STRATEGY: use HOF Map on LOT - lst
#;(define (trees-after-button-down lst x y)
 (map
  (lambda (t)
    (tree-after-button-down t x y)) lst))


;; circle-after-button-down : circle Integer Integer -> circle
;; RETURNS: the cat following a button-down at the given location.
;; STRATEGY: combine simpler functions.
#;(define (tree-after-button-down c x y)
  (if (in-circle? c x y)
      (make-mycircle (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
                     (- (circ-x c) x) (- (circ-y c) y)
                     true (mycircle-center-trail c) (circle-pen-down? c)) c))

;; in-node? : Node Integer Integer -> Boolean
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given node.
;; EXAMPLES: see tests below
;; STRATEGY: Use template for Node on n
(define (in-node? n x y)
  (if (node-shape n)
      (in-circle?) (in-square?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-circle? : Node Integer Integer -> Boolean
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given circle.
;; EXAMPLES: see tests below
;; STRATEGY: Use template for Node on n
(define (in-circle? n x y)
  (<=
   (sqrt
    (+ (sqr (- (posn-x (node-position n)) x))
       (sqr (- (posn-y (node-position n)) y)))) CIRCLE-RADIUS))

;;; tests:
(begin-for-test
  (check-equal? (in-circle? node1 30 50) true
   "in-circle returned incorrect results"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-square? : Node Integer Integer -> Boolean
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given square.
;; EXAMPLES: see tests below
;; STRATEGY: Use template for Node on n
(define (in-square? n x y)
 ( = (* (* 2 SQUARE-RADIUS)  (* 2 SQUARE-RADIUS))
     (+
      (area-of-triangle1 n x y)
      (area-of-triangle2 n x y)
      (area-of-triangle3 n x y)
      (area-of-triangle4 n x y))))

;;; tests:
(begin-for-test
  (check-equal? (in-square? node1 29 50) false
   "in-square? returned incorrect results"))



;; area-of-triangle1 : Node Posn Posn -> Number
;; GIVEN    : a Node and x and y co-ordinate of a point
;; RETURNS  : the area of triangle
;; EXAMPLES : see test cases
;; STRATEGY : combine simpler functions
(define (area-of-triangle1 n x y)
  (area-of-triangle
                    ; Point A
                    (make-posn (- (posn-x (node-position n)) SQUARE-RADIUS)
                    (- (posn-y (node-position n)) SQUARE-RADIUS))
                    ; Point B
                    (make-posn (+ (posn-x (node-position n))  SQUARE-RADIUS)
                    (- (posn-y (node-position n)) SQUARE-RADIUS))
                    ; Point P
                    (make-posn x y)))

;; area-of-triangle2 : Node Posn Posn -> Number
;; GIVEN    : a Node and x and y co-ordinate of a point
;; RETURNS  : the area of triangle
;; EXAMPLES : see test cases
;; STRATEGY : combine simpler functions
(define (area-of-triangle2 n x y)
  (area-of-triangle
                    ; Point B
                    (make-posn (+ (posn-x (node-position n))  SQUARE-RADIUS)
                    (- (posn-y (node-position n)) SQUARE-RADIUS))
                    ; Point C
                    (make-posn (+ (posn-x (node-position n))  SQUARE-RADIUS)
                    (+ (posn-y (node-position n)) SQUARE-RADIUS))
                    ; Point P
                    (make-posn x y)))

;; area-of-triangle3 : Node Posn Posn -> Number
;; GIVEN    : a Node and x and y co-ordinate of a point
;; RETURNS  : the area of triangle
;; EXMAPLES : see test cases
;; STRATEGY : combine simpler functions
(define (area-of-triangle3 n x y)
  (area-of-triangle
                   ; Point C
                    (make-posn (+ (posn-x (node-position n))  SQUARE-RADIUS)
                    (+ (posn-y (node-position n)) SQUARE-RADIUS))
                   ; Point D
                    (make-posn (- (posn-x (node-position n))  SQUARE-RADIUS)
                    (+ (posn-y (node-position n)) SQUARE-RADIUS))
                    ; Point P
                    (make-posn x y)))

;; area-of-triangle4 : Node Posn Posn -> Number
;; GIVEN    : a Node and x and y co-ordinate of a point
;; RETURNS  : the area of triangle
;; EXMAPLES : see test cases
;; STRATEGY : combine simpler functions
(define (area-of-triangle4 n x y)
  (abs (area-of-triangle
                    ; Point A
                    (make-posn (- (posn-x (node-position n)) SQUARE-RADIUS)
                    (- (posn-y (node-position n)) SQUARE-RADIUS))
                    ; Point D
                    (make-posn (- (posn-x (node-position n))  SQUARE-RADIUS)
                    (+ (posn-y (node-position n)) SQUARE-RADIUS))
                    ; Point P
                    (make-posn x y))))

;; area-of-triangle : Posn Posn Posn -> Number
;; GIVEN    : three points of a triangle
;; RETURNS  : the area of triangle
;; EXMAPLES : see test cases
;; STRATEGY : combine simpler functions
(define (area-of-triangle pointA pointB pointC)
 (/
  (+
   (* (posn-x pointA)
      (- (posn-y pointB) (posn-y pointC)))
   (* (posn-x pointB)
      (- (posn-y pointC) (posn-y pointA)))
   (* (posn-x pointC)
      (- (posn-y pointA) (posn-y pointB)))) 2))

(begin-for-test
  (check-equal?
   (area-of-triangle
    (make-posn 30 30) (make-posn 70 30) (make-posn 50 50)) 400
   "area-of-triangle returned incorrect area"))

       
;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(new-cicle-key-event kev)
     (world-with-new-circle-added w)]

     [(new-cicle-key-event kev)
     (world-with-new-square-added w)]

     [else w]))
   
    

;; world-with-new-circle-added : World -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one, but with a new circle added
;; at the top center of the canvas
;; STRATEGY: use template for World on w
(define (world-with-new-circle-added w)
  (make-world
   (cons
    (make-node true
     (make-posn (/ CANVAS-WIDTH 2) 20) false empty empty empty)
    (world-listOfTree w))))

;; world-with-new-square-added : World -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one, but with a new square added
;; at the top center of the canvas
;; STRATEGY: use template for World on w
(define (world-with-new-square-added w)
  (make-world
   (cons
    (make-node false
     (make-posn (/ CANVAS-WIDTH 2) 20) false empty empty empty)
    (world-listOfTree w))))


;world-to-trees : World -> ListOfTree
;GIVEN: a World
;RETURNS: a list of all the trees in the given world.
(define (world-to-trees w)
  (world-listOfTree w))

;tree-to-root : Tree -> Node
;GIVEN: a tree
;RETURNS: the node at the root of the tree
;EXAMPLE: Consider the tree represented as follows:
;
;                 A
;                 |
;       +---+-----+-----+
;       |   |     |     |
;       B   C     D     E
;           |           |
;         +---+      +-----+
;         |   |      |     |
;         F   G      H     I
;
;If tree-to-root is given the subtree rooted at C, it should return the
;data structure associated with node C. This data structure may or may
;not include data associated with rest of the tree, depending on
;whether you have chosen to represent nodes differently from trees.
;
;
;tree-to-sons : Tree -> ListOfTree
;GIVEN: a tree
;RETURNS: the data associated with the immediate subtrees of the given
;tree. 
;EXAMPLE: In the situation above, if tree-to-sons is given the subtree
;rooted at C, it should return a list consisting of the subtree rooted
;at F and the subtree rooted at G.
;
;[Note how these examples are expressed.  They are not just tests, but
;are constructed to illuminate possible ambiguities or
;misunderstandings in the purpose statement.  This is what a good
;example does.]
;
;
;node-to-center : Node -> Posn
;RETURNS: the center of the given node as it is to be displayed on the
;scene.
;Note: this function returns a Posn (an ISL builtin).  This is for the
;convenience of the testing framework, and you may or may not wish to
;represent the center of the node in this way.
;
;node-to-selected? : Node -> Boolean
;RETURNS: true iff the given node is selected.


(define node1 (make-node false (make-posn 50 50) false empty empty empty))