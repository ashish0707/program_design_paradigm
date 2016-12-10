;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "06" "q1.rkt")

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define SHAPE-CIRCLE "circle")
(define SHAPE-SQUARE "square")
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define CIRCLE 20)
(define LINE "blue")
(define SQUARE (* CIRCLE 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define SIMPLE-CIRCLE (circle CIRCLE "outline" "green"))
(define SELECTED-CIRCLE (circle CIRCLE "solid" "green"))
(define SIMPLE-SQUARE (square SQUARE "outline" "green"))
(define SELECTED-SQUARE (square SQUARE "solid" "green"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct node (is-shape-circle? position dx dy selected? children parent))

;; A leaf is a (make-node Boolean Posn Int Int Boolean LON NODE)
;; INTERP:
;; is-shape-circle? is the boolean to decide between circle or square
;; position is the location of the center of the shape
;; dx and dy are the distances between center of the shape and mouse pointer
;; selected? is whether the shape is selected or not
;; children is the LON
;; parent is the root of the tree

;; TEMPLATE:
;; node-fn : Node -> ??
;; (define (node-fn n)
;;   (... (node-is-shape-circle? n)
;;        (node-position n)
;;        (node-dx n)
;;        (node-dy n)
;;        (node-selected? n)
;;        (node-children n)
;;        (node-parent n)))

;; INTERP:
;; A Tree is a list of Nodes

;; A ListOfNode is:
;; -- empty
;; -- (cons Node ListOfNode)

;; TEMPLATE
;; LON-fn: LON->??
;;  (define (LON-fn LON)
;;     (cond
;;       [(empty? LON) ...]
;;       [else (...
;;           (node-fn (first LON))
;;           (LON-fn (rest LON)))]))

(define-struct world (trees))

;; A world is (make-world LON)
;; INTERP:
;; tree is the list of nodes

;; Template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (...
;;       (world-trees w)) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples for testing

(define node1
  (make-node false (make-posn 50 50) -1 -1 false empty empty))

(define node2
  (make-node true (make-posn 30 30) -1 -1 true empty
                    (make-node false
                               (make-posn 70 70) -1 -1 true empty empty)))

(define node3
  (make-node false (make-posn 30 30) -1 -1 true empty
                    (make-node false
                               (make-posn 70 70) -1 -1 true empty empty)))

(define list-of-tree
  (list (make-node true (make-posn 30 30) -1 -1 false
                   (list (make-node true (make-posn 30 (+ CIRCLE 53))
                                    -1 -1 false empty empty)) '())
        (make-node false (make-posn 70 70) -1 -1 true
                   (list (make-node true (make-posn 70 (+ SQUARE 3 70))
                                    -1 -1 false empty empty)) '())))

(define list-of-tree2
  (list (make-node true (make-posn 30 30) -1 -1 false empty empty)
        (make-node false (make-posn 70 70) -1 -1 false empty empty)))

(define list-of-tree3
  (list (make-node true (make-posn 30 30) -1 -1 false
                   (list (make-node true (make-posn 30 (+ CIRCLE 53))
                                    -1 -1 false empty empty)) empty)))

(define list-of-tree4
  (list (make-node true (make-posn 30 30) -1 -1 true empty empty)
        (make-node false (make-posn 70 70) -1 -1 true empty empty)))


(define world1
  (make-world empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION

;; run     :  Any -> World
;; GIVEN   : any value
;; EFFECT  : runs a copy of an initial world
;; RETURNS : the final state of the world.  The given value is ignored.

(define (run any)
  (big-bang (initial-world any)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN    : any value
;; EXAMPLE  : (initial-world 0) = (make-world empty)
;; RETURNS  : an initial world.  The given value is ignored.
;; STRATEGY : combine simpler functions

(define (initial-world pos)
  (make-world empty))

;; TEST
(begin-for-test
  (check-equal? (initial-world 0) (make-world empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD TO SCENE FUNCTIONS

;; world-to-scene : World -> Scene
;; GIVEN    : a World
;; RETURNS  : a Scene that portrays the given world.
;; EXAMPLE  : (world-to-scene world1) = EMPTY-CANVAS
;; STRATEGY : combine simpler functions
(define (world-to-scene w)
  (place-shapes (world-trees w) EMPTY-CANVAS)) 


;; place-shapes : LON Canvas -> Scene
;; GIVEN    : a list of node and a canvas
;; RETURNS  : a Scene that has the a new shape
;; EXAMPLE  : (place-shapes list-of-tree3 EMPTY-CANVAS) = 
;;            (place-image SIMPLE-CIRCLE 30 30
;;                       (place-image SIMPLE-CIRCLE 30 73 EMPTY-CANVAS))
;; STRATEGY : Use HOF foldr on LON - lst
;; HALTING MEASURE : Length of LON - lst. The function will halt when
;; all the nodes in the list along with their children printed on the canvas.
(define (place-shapes lst canvas)
  ;; (Node Canvas -> Scene) Scene LON -> SCENE
  (foldr
   ;; Node Canvas -> Scene
   ;; RETURNS:  a canvas with new shape
   (lambda (n canvas)
     (if (empty? (node-children n))
         (place-shape n canvas)
         (place-shape n (place-shapes (node-children n) canvas))))
   canvas lst))

  
;; place-shape : Node Scene -> Scene
;; GIVEN    : a node and a Scene
;; RETURNS  : a circle or a square is created on the canvas
;; EXAMPLE  : (place-shape node1 EMPTY-CANVAS) = 
;;            (place-image SIMPLE-CIRCLE 30 30 EMPTY-CANVAS))
;; STRATEGY : Use template on Node for n

(define (place-shape n s)
  (if (node-is-shape-circle? n)
      (if (empty? (node-parent n))
          (place-circle n s)
          (place-line n (node-parent n) (place-circle n s)))
      (if (empty? (node-parent n))
          (place-square n s)
          (place-line n (node-parent n) (place-square n s)))))

;; place-line : Node Node Scene -> Scene
;; GIVEN    : a node and its parent and a scene to place a line.
;; RETURNS  : a Scene with line placed between center of parent and child node.
;; EXAMPLE  :
;; (place-line node1 node1 EMPTY-CANVAS) = EMPTY-CANVAS
;; STRATEGY : combine simpler functions

(define (place-line n parentNode canvas)
 (scene+line canvas
             (posn-x (node-position n))
              (posn-y (node-position n))
               (posn-x (node-position parentNode))
               (posn-y (node-position parentNode))
                LINE)) 


;; place-circle : Node Canvas -> Canvas
;; GIVEN    : a node and a canvas
;; RETURNS  : a circle created on canvas
;; EXAMPLE  : (place-circle node1 EMPTY-CANVAS) =
;;            (place-image SIMPLE-CIRCLE 50 50 EMPTY-CANVAS)
;; STRATEGY : cases on node n

(define (place-circle n s)
  (if (node-to-selected? n)
      (draw-on-canvas SELECTED-CIRCLE n s)
      (draw-on-canvas SIMPLE-CIRCLE n s)))

;; place-square : Node Canvas -> Image
;; GIVEN    : a node and a canvas
;; RETURNS  : a square created on canvas
;; EXAMPLE  : (place-square node1 EMPTY-CANVAS) =
;;            (place-image SIMPLE-SQUARE 50 50 EMPTY-CANVAS)
;; STRATEGY : cases on node n

(define (place-square n s)
  (if (node-to-selected? n)
      (draw-on-canvas SELECTED-SQUARE n s)
      (draw-on-canvas SIMPLE-SQUARE n s)))

;; draw-on-canvas : Shape Node Canvas -> Canvas
;; GIVEN    : a shape, a node and a canvas
;; RETURNS  : a square or circle created on canvas
;; EXAMPLE  : (draw-on-canvas SIMPLE-SQUARE node1 EMPTY-CANVAS) =
;;            (place-image SIMPLE-SQUARE 50 50 EMPTY-CANVAS)
;; STRATEGY : combine simpler functions
(define (draw-on-canvas img n s)
  (place-image  img
                (posn-x (node-position n))
                (posn-y (node-position n)) s))

;; TESTS
(begin-for-test
  (check-equal? (world-to-scene world1) EMPTY-CANVAS)
  (check-equal? (world-to-scene (make-world list-of-tree2))
                (place-image SIMPLE-CIRCLE 30 30
                             (place-image SIMPLE-SQUARE 70 70 EMPTY-CANVAS)))
  (check-equal? (world-to-scene (make-world list-of-tree4))
                (place-image SELECTED-CIRCLE 30 30
                             (place-image SELECTED-SQUARE 70 70 EMPTY-CANVAS)))
  (check-equal? (place-shapes list-of-tree3 EMPTY-CANVAS)
                (place-image SIMPLE-CIRCLE 30 30
                             (place-image SIMPLE-CIRCLE 30 73 EMPTY-CANVAS)))
  (check-equal? (place-shape node2 EMPTY-CANVAS)
                (scene+line
                 (place-image (circle 20 "solid" "green")
                              30 30 EMPTY-CANVAS) 30 30 70 70 "blue"))
  (check-equal? (place-shape node3 EMPTY-CANVAS)
                (scene+line
                 (place-image (square 40 "solid" "green")
                              30 30 EMPTY-CANVAS) 30 30 70 70 "blue")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD AFTER MOUSE EVENT FUNCTIONS

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN    : a world and a description of a mouse event
;; RETURNS  : the world that should follow the given mouse event
;; EXAMPLE  :
;; (world-after-mouse-event (make-world list-of-tree3) 100 500 "button-up")
;;           (make-world (list (make-node #true (make-posn 30 30)  -1  -1 false
;;                (list (make-node #true (make-posn 30 73) -1 -1 false
;;                                     '() '())) false)))
;; STRATEGY : combine simpler functions

(define (world-after-mouse-event w x y event)
  (make-world (trees-after-mouse-event (world-trees w) x y event)))

;; trees-after-mouse-event : LON Integer Integer MouseEvent -> LON
;; GIVEN: a circle list and a description of a mouse event
;; RETURNS: the list of nodes that should follow the given mouse event
;; examples:
;; (trees-after-mouse-event list-of-tree3 100 500 "button-up")
;;           (make-world (list (make-node #true (make-posn 30 30)  -1  -1 false
;;                (list (make-node #true (make-posn 30 73) -1 -1 false
;;                                     '() '())) false)))
;; strategy:  Cases on KeyEvent mev

(define (trees-after-mouse-event lst mx my mev)
  (cond
    [(mouse=? mev "button-down") (nodes-after-button-down lst mx my)]
    [(mouse=? mev "drag") (nodes-after-drag lst mx my)]
    [(mouse=? mev "button-up") (nodes-after-button-up lst mx my)]
    [else lst]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUTTON-DOWN FUNCTION

;; nodes-after-button-down : LON Integer Integer -> Node
;; GIVEN    : List of Nodes (LON), x and y coordinates of mouse pointer.
;; RETURNS  : the Node following a button-down at the given location.
;; EXAMPLES :
;; (trees-after-mouse-event list-of-tree3 100 500 "button-down")
;;           (make-world (list (make-node #true (make-posn 30 30)  -1  -1 false
;;                (list (make-node #true (make-posn 30 73) -1 -1 false
;;                                     '() '())) false)))
;; STRATEGY : Use HOF map on lst
;; HALTING MEASURE : Length of LON - lst. The function will halt when
;; all the nodes in the list along with their children are updated with
;; relative distance between mouse and node center.
(define (nodes-after-button-down lst x y)
  (map
   ;; Node -> Node
   ;; RETURNS : Similar node but making it selected if mouse pointer 
   ;; coordinates are within the node
   (lambda (n)
     (if (in-node? n x y)
         (inside-shape n x y)
         (outside-shape n x y)))      
     lst))

;; inside-shape : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and coordinates of mouse pointer
;; RETURNS: a new node after being selected.
;; It stores the relative distance from its center to mouse for itself
;; and its center.
;; EXAMPLE:
;; (inside-shape node1 50 50) = (make-node false (make-posn 50 50)
;;                                 0 0 true empty empty))
;; STRATEGY: use template for Node on n

(define (inside-shape n x y)
  (make-node (node-is-shape-circle? n) (node-position n)
             (- (posn-x (node-position n)) x)
             (- (posn-y (node-position n)) y) true
             (nodes-after-button-down
              (store-rel-position (node-children n) x y) x y)
             (node-parent n)))
 
;; outside-shape : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and coordinates of mouse pointer
;; RETURNS: a similar node as given but checks if any of its children
;; is selected.
;; EXAMPLE:
;; (outside-shape node1 50 50) = (make-node false (make-posn 50 50)
;;                                 -1 -1 false empty empty))
;; STRATEGY: use template for Node on n

(define (outside-shape n x y)
  (make-node (node-is-shape-circle? n) (node-position n)
             (node-dx n)
             (node-dy n) false
             (nodes-after-button-down (node-children n) x y)
             (node-parent n)))

;; store-rel-position : LON Integer Integer -> LON
;; GIVEN    : List of Nodes (LON), x and y coordinates of mouse pointer.
;; RETURNS  : the Node following a button-down at the given location.
;; EXAMPLES :
;; (store-rel-position list-of-tree2 30 30)=
;; (list (make-node #true (make-posn 30 30) 0 0 false '() false)
;;      (make-node false (make-posn 70 70) 40 40 false '() false))
;; STRATEGY : Use HOF map on lst

(define (store-rel-position lst x y)
  (map
   ;; NODE -> NODE
   ;; RETURNS : Similar node but with new relative distance which is
   ;; x,y coordinates of node - x,y coordinates of mouse resp. 
   (lambda (n)
     (make-node (node-is-shape-circle? n) (node-position n)
                (- (posn-x (node-position n)) x)
                (- (posn-y (node-position n)) y) false
             (store-rel-position (node-children n) x y) (node-parent n))) lst))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUTTON-UP FUNCTION

;; nodes-after-button-up : LON Integer Integer -> Node
;; GIVEN    : List of Nodes (LON), x and y coordinates of mouse pointer.
;; RETURNS  : the Node following a button-down at the given location.
;; EXAMPLES :
;; (nodes-after-button-up list-of-tree2 30 30) = (list
;;                   (make-node false (make-posn 30 30)
;;                              -1 -1 false empty false)
;;                   (make-node  false (make-posn 70 70)
;;                              -1 -1 false '() false)))
;; STRATEGY : Use HOF map on lst

(define (nodes-after-button-up lst x y)
  (map
   ;; NODE -> NODE
   ;; RETURNS : Similar node but with deselecting it and its children
   (lambda (n)
     (make-node (node-is-shape-circle? n) (node-position n)
                (node-dx n)
                (node-dy n) false
                (nodes-after-button-up (node-children n) x y) (node-parent n)))
   lst))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DRAG FUNCTION

;; nodes-after-drag : LON Integer Integer -> LON
;; GIVEN    : List of Nodes (LON), x and y coordinates of mouse pointer.
;; RETURNS  : the Node following a button-down at the given location.
;; EXAMPLES :
;; 
;; STRATEGY : Use HOF map on lst
;; HALTING MEASURE : Length of LON - lst. The function will halt when
;; all the nodes in the list along with their children are updated according
;; to new mouse pointer values.
(define (nodes-after-drag lst x y)
  (map
   ;; NODE -> NODE
   ;; RETURNS : Similar node but with new x,y coordinates to follow drag
   ;; mouse event aLONg with updating its children
   (lambda (n) 
     (if (node-selected? n)
         (drag-selected n x y)
         (drag-unselected n x y)))
   lst))

;; drag-selected : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and mouse pointer location
;; RETURNS: a node after being dragged
;; EXAMPLE:
;; (drag-selcted node1 50 50) =  (make-node false (make-posn 49 49)
;;                                  -1 -1 false empty empty))
;; STRATEGY: us template of node on n

(define (drag-selected n x y)
  (make-node (node-is-shape-circle? n)
                    (make-posn (+ x (node-dx n))
                               (+ y (node-dy n)))
                    (node-dx n)
                    (node-dy n)
                    (node-selected? n)
                    (nodes-after-drag
                     (drag-children (node-children n) x y n) x y)
                    (node-parent n)))

;; drag-unselected : Node NonNegInt NonNegInt -> Node
;; GIVEN: a node and mouse pointer location
;; RETURNS: a node after being dragged
;; EXAMPLE:
;; (drag-unselcted node1 50 50) =  (make-node false (make-posn 50 50)
;;                                  -1 -1 false empty empty))
;; STRATEGY: us template of node on n

(define (drag-unselected n x y)
  (make-node (node-is-shape-circle? n)
                    (node-position n)
                    (node-dx n)
                    (node-dy n)
                    (node-selected? n)
                    (nodes-after-drag (node-children n) x y)
                    (node-parent n)))


;; drag-children : LON Integer Integer -> LON
;; GIVEN    : List of Nodes (LON), x and y coordinates of mouse pointer.
;; RETURNS  : the Node following a button-down at the given location.
;; EXAMPLES : see test cases
;; STRATEGY : Use HOF map on lst
;; HALTING MEASURE : Length of LON - lst. The function will halt when
;; all the nodes in the list along with their children are updated according to
;; new mouse co-ordinates
(define (drag-children lst x y parentNode)
  (map
   ;; NODE -> NODE
   ;; RETURNS : Similar node but with new  x,y coordinates of node to follow
   ;; drag event.
   (lambda (n)
     (make-node (node-is-shape-circle? n)
                (make-posn (+ x (node-dx n))
                           (+ y (node-dy n))) 
                (node-dx n)
                (node-dy n)
                (node-selected? n)
                (drag-children (node-children n) x y n)
                parentNode)) lst))


;; in-node? : Node Integer Integer -> Boolean
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given node.
;; EXAMPLES:
;; (in-node? node1 90 70) = false
;; STRATEGY: cases on node n

(define (in-node? n x y)
  (if (node-is-shape-circle? n)
      (in-circle? n x y)
      (in-square? n x y)))


;; in-circle? : Node Integer Integer -> Boolean
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given circle.
;; EXAMPLES:
;; (in-circle? node1 90 70) = false
;; STRATEGY: combine simpler functions

(define (in-circle? n x y)
  (<=
   (sqrt
    (+ (sqr (- (posn-x (node-position n)) x))
       (sqr (- (posn-y (node-position n)) y)))) CIRCLE))


;; in-square? : Node Integer Integer -> Boolean
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given square.
;; EXAMPLES:
;; (in-square? node1 90 70)
;; STRATEGY: combine simpler functions

(define (in-square? n x y)
  (and (<= (- (posn-x (node-position n)) (/ SQUARE 2))
           x
           (+ (posn-x (node-position n)) (/ SQUARE 2)))
       (<= (- (posn-y (node-position n)) (/ SQUARE 2))
           y
           (+ (posn-y (node-position n)) (/ SQUARE 2)))))

;; TESTS

(begin-for-test
  (check-equal? (in-square? node1 29 50) false
                "in-square? returned incorrect results")
  (check-equal? (in-square? node1 39 29) false
                "in-square? returned incorrect results")
  (check-equal? (world-after-mouse-event (make-world list-of-tree)
                                         100 500 "drag")
                (make-world
                 (list
                  (make-node #true (make-posn 30 30) -1 -1 false
                 (list (make-node #true (make-posn 30 73) -1 -1 false '() '()))
                 '()) (make-node false (make-posn 99 499) -1 -1 #true
                 (list (make-node #true (make-posn 99 499) -1 -1 false '()
                       (make-node  false (make-posn 70 70) -1 -1 #true
                 (list (make-node #true (make-posn 70 113) -1 -1 false '()
                                  '())) '()))) '())))
                "world after the drag of tree")
  (check-equal? (world-after-mouse-event (make-world list-of-tree3)
                                         100 500 "button-up")
                (make-world
                 (list
                  (make-node #true (make-posn 30 30)  -1  -1 false
                     (list (make-node #true (make-posn 30 73) -1 -1 false
                                      '() '())) '())))
                "world after button up mouse event")
   (check-equal? (world-after-mouse-event (make-world list-of-tree3)
                                          30 30 "button-down")
                 (make-world
                  (list (make-node #true (make-posn 30 30) 0 0 true
                                   (list (make-node #true (make-posn 30 73)
                                                    0 43  false '() '()))
                                   '())))
                 "world after button down mouse event")
   (check-equal? (world-after-mouse-event (make-world list-of-tree3)
                                          30 30 "leave")
                 (make-world
                  (list (make-node #true (make-posn 30 30) -1 -1 false
                                   (list (make-node #true (make-posn 30 73)
                                                    -1 -1  false '() '()))
                                   '())))
                 "world after leave mouse event"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD AFTER KEY EVENT FUNCTIONS

;; is-new-square-key-event? : KeyEvent -> Boolean
;; GIVEN    : a KeyEvent
;; RETURNS  : true iff the KeyEvent represents a create new square instruction
;; EXAMPLE:
;; (is-new-square-key-event? "s") = true
;; STRATEGY : combine simpler functions

(define (is-new-square-key-event? ke)
  (key=? ke "s"))

;; is-new-circle-key-event? : KeyEvent -> Boolean
;; GIVEN    : a KeyEvent
;; RETURNS  : true iff the KeyEvent represents a create new circle instruction
;; EXAMPLE:
;; (is-new-circle-key-event? "f") = false
;; STRATEGY : combine simpler functions

(define (is-new-circle-key-event? ke)
  (key=? ke "c"))

;; is-delete-shape-key-event? : KeyEvent -> Boolean
;; GIVEN    : a KeyEvent
;; RETURNS  : true iff the KeyEvent represents a delete new shape instruction
;; EXAMPLE:
;; (is-delete-shape-key-event? "d") = true
;; STRATEGY : combine simpler functions

(define (is-delete-shape-key-event? ke)
  (key=? ke "d"))

;; TESTS
(begin-for-test
  (check-equal? (is-new-square-key-event? "s") true)
  (check-equal? (is-new-circle-key-event? "f") false)
  (check-equal? (is-delete-shape-key-event? "d") true))
  

;; world-after-key-event : World KeyEvent -> World
;; GIVEN    : a world w
;; RETURNS  : the world that should follow the given world
;; after the given key event.
;; EXAMPLE  : (world-after-key-event (make-world list-of-tree2) "s") =
;;            (make-world (list
;;                (make-node  false (make-posn 250 20) -1 -1 false
;;                     '() '()) (make-node #true (make-posn 30 30) -1 -1
;;                      false '() false) (make-node false (make-posn 70 70)
;;                      -1 -1 false '() false))) 
;; STRATEGY : cases on KeyEvent kev

(define (world-after-key-event w kev)
  (cond
    [(is-new-circle-key-event? kev)
     (world-with-new-circle-added w)] 
    [(is-new-square-key-event? kev)
     (world-with-new-square-added w)]
    [(is-delete-shape-key-event? kev)
     (world-with-shape-deleted w)]
    [else w]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DELETE NODE FUNCTIONS

;; world-with-shape-deleted : World -> World
;; GIVEN    : a world w
;; RETURNS  : a world just like the given one, but deleting the selected shape.
;; EXAMPLES :
;; (world-with-shape-deleted (make-world list-of-tree3)) =
;;            (make-world (list (make-node #true (make-posn 30 30)
;;                   -1 -1 false (list (make-node #true (make-posn 30 73)
;;                   -1 -1 false '() '())) false)))
;; STRATEGY : Use template for world on w.

(define (world-with-shape-deleted w)
  (make-world
   (delete-selected-shape
    (handle-deletion-of-root-node (world-trees w)))))


;; delete-selected-shape : LON -> LON
;; GIVEN    : list of nodes
;; RETURNS  : LON with deleting the selected root nodes.
;; EXAMPLES :
;; (handle-deletion-of-root-node list-of-tree3) =
;;            (make-world (list (make-node #true (make-posn 30 30)
;;                   -1 -1 false (list (make-node #true (make-posn 30 73)
;;                   -1 -1 false '() '())) false)))
;; (handle-deletion-of-root-node list-of-tree3)
;; STRATEGY : combine simpler functions

(define (handle-deletion-of-root-node lst)
  (append (filter-selected-node lst)
          (change-parent (childrens-of-selected-node lst) '())))


;; delete-selected-shape : LON -> LON
;; GIVEN    : list of nodes
;; RETURNS  : LON with deleting the selected nodes.
;; EXAMPLES :
;; (delete-selected-shape list-of-tree3) =
;;            (make-world (list (make-node #true (make-posn 30 30)
;;                   -1 -1 false (list (make-node #true (make-posn 30 73)
;;                   -1 -1 false '() '())) false)))
;; STRATEGY : Use HOF map on lst
;; HALTING MEASURE : Length of LON - lst. The function will halt when
;; all the nodes in the list along with their children processed by finding the
;; and deleting selected node.
(define  (delete-selected-shape lst)  
  (map
   ;; Node -> Node
   ;; RETURNS : A Node where the selected child is deleted.
   (lambda (n)
     (make-node (node-is-shape-circle? n) (node-position n)
                (node-dx n)
                (node-dy n) false
                (delete-selected-shape
                 (append (filter-selected-node (node-children n))
                         (change-parent
                          (childrens-of-selected-node (node-children n))
                          (node-parent n))))
                (node-parent n)))
   lst))


;; filter-selected-node : LON -> LON
;; GIVEN    : list of nodes
;; RETURNS  : LON with filtering the selected nodes.
;; EXAMPLES :
;; (filter-selected-node list-of-tree3) =
;;            (make-world (list (make-node #true (make-posn 30 30)
;;                   -1 -1 false (list (make-node #true (make-posn 30 73)
;;                   -1 -1 false '() '())) false)))
;; STRATEGY : Use HOF filter on lst

(define (filter-selected-node lst)
  (filter
   ;; NODE -> BOOLEAN
   ;; RETURNS : true iff any node is selected
   (lambda (n)
     (not (node-selected? n))) lst))


;; childrens-of-selected-node : LON -> LON
;; GIVEN    : list of nodes
;; RETURNS  : The children of the selected node 
;; EXAMPLES : (childrens-of-selected-node list-of-tree3) = empty
;; STRATEGY : Use HOF Foldr on lst
(define (childrens-of-selected-node lst)
  ;; (NODE LON -> LON) ListOfNodes LON -> LON
  (foldr
   ;; (NODE LON -> LON) 
   ;; RETURNS : LIST OF NODES FOR THE SELECTED NODES
   (lambda (n base)
     (if (and (node-selected? n) (> (length (node-children n)) 0))
         (append (node-children n) base)
         base)) empty lst))


;; change-parent : LON Node -> LON
;; GIVEN    : list of nodes and a parent node
;; RETURNS  : The children with updated parent node.  
;; EXAMPLES : (change-parent list-of-tree3 node1) = 
;;             (list (make-node #true (make-posn 30 30) -1 -1 false
;;             (list (make-node #true (make-posn 30 73) -1 -1 false
;;                              '() '()))
;;             (make-node false (make-posn 50 50) -1 -1 false '() '())))
;; STRATEGY : Use HOF Map on lst

(define (change-parent lst parentNode)
  (map
   ;; NODE -> NODE
   ;; RETURNS : Similar node but with new parent.
   (lambda (n)    
     (make-node (node-is-shape-circle? n) (node-position n)
                (node-dx n) (node-dy n)
                (node-selected? n)
                (node-children n) parentNode)) lst))


;; ADD NODE FUNCTIONS

;; world-with-new-circle-added : World -> World
;; GIVEN    : a world w
;; RETURNS  : a world just like the given one, but with a new circle added
;; either to the selected shapes or at the top center of the canvas.
;; EXAMPLES :
;; (world-with-new-circle-added (make-world list-of-tree3)) =
;;      (make-world (list (make-node #true (make-posn 250 20) -1 -1
;;          false '() '())
;;      (make-node #true (make-posn 30 30) -1 -1 false
;;       (list
;;          (make-node #true (make-posn 30 73) -1 -1 false
;;           '() '())) false)))
;; STRATEGY : Use cases on World w

(define (world-with-new-circle-added w)
  (if (any-shape-selected? (world-trees w))
      (make-world
       (add-shape-to-selected-shapes true (world-trees w)))
      (make-world
       (add-circle-to-world-tree-list (world-trees w)))))


;; any-shape-selected? : LON -> BOOLEAN
;; GIVEN    : a list of nodes
;; RETURNS  : true iff any shape within list of nodes is selected
;; EXAMPLES :
;; (any-shape-selected? list-of-tree3) = false
;; STRATEGY : Use HOF map on lst
;; HALTING MEASURE : Length of LON - lst. The function will halt when
;; all the nodes in the list along with their children are checked if selected.
(define  (any-shape-selected? lst)
  (ormap
   ;; NODE -> BOOLEAN
   ;; RETURNS : true iff the node is selected.  
   (lambda (n)
     (or
      (node-selected? n)
      (any-shape-selected? (node-children n))))
   lst))

;;; tests:
(begin-for-test
  (check-equal?  (any-shape-selected?
                  (nodes-after-button-down list-of-tree 70 94)) true
                  "any-shape-selected? returned incorrect value")
  
  (check-equal?  (any-shape-selected? list-of-tree) true
                 "any-shape-selected? returned incorrect value"))

;; add-circle-to-world-tree-list : LON -> LON
;; GIVEN    : a list of nodes
;; RETURNS  : a LON just like the given one, but with a new circle added
;; at the list.
;; EXAMPLES : (add-circle-to-world-tree-list '()) =
;;               (list (make-node true (make-posn 250 20) -1 -1 false '() '()))
;; STRATEGY : Use template for Node on

(define (add-circle-to-world-tree-list lst)
  (cons
   (make-node true
              (make-posn (/ CANVAS-WIDTH 2) 20) -1 -1 false empty empty) lst))
;;; tests:
(begin-for-test
  (check-equal? (add-circle-to-world-tree-list '())
                (list
                 (make-node true (make-posn 250 20) -1 -1 false '() '()))
                "add-circle-to-world-tree-list added incorrect circles"))

;; add-shape-to-selected-shapes : LON BOOLEAN-> World
;; GIVEN    : a list of nodes and a boolean stating whether its a
;; circle or square
;; RETURNS  : a list of nodes with children added to it each selected node.
;; EXAMPLES : (add-shape-to-selected-shapes true list-of-tree3) =
;;            (list (make-node #true (make-posn 30 30) -1 -1 false
;;            (list (make-node  #true (make-posn 30 73) -1 -1 false
;;            '() '())) false))
;; STRATEGY : use HOF map on lst
;; HALTING MEASURE : Lenght of lst. The function will halt after
;; visiting all the nodes in list as well as visiting its children

(define  (add-shape-to-selected-shapes is-circle? lst)
  (map
   ;; NODE -> NODE
   ;; RETURNS : Similar node but with new node added to its children list if
   ;; node is selected.
   (lambda (n) 
     (if (node-selected? n)
         (add-to-shape-selected is-circle? n)
         (proceed-if-shape-unselected is-circle? n)))      
   lst))

;; add-to-shape-selected : Node BOOLEAN-> Node
;; GIVEN: a node n and a boolean stating whether to represent node as circle or
;; square.
;; RETURNS: a new node with 
;; EXAMPLE: (add-to-shape-selected false node1)  =
;;          (make-node false (make-posn 50 50) -1 -1 false
;;          (list (make-node false (make-posn 50 110) -1 59 false '()
;;              (make-node false (make-posn 50 50) -1 -1 false '() '())))
;;          '())
;; STRATEGY: use template of node on n

(define (add-to-shape-selected is-circle? n)
  (make-node (node-is-shape-circle? n) (node-position n)
                    (node-dx n) (node-dy n)
                    (node-selected? n)
                    (add-shape-to-selected-shapes is-circle?
                     (add-child (node-children n) n is-circle?))
                     (node-parent n)))

;; proceed-if-shape-unselected : Node BOOLEAN -> Node
;; GIVEN    : a node n and a boolean stating whether to represent node as 
;; circle or square.
;; RETURNS  : a same node as given where childrens are in turn checked for
;; being selected.
;; EXAMPLE  : (proceed-if-shape-unselected false node1)  =
;;          (make-node false (make-posn 50 50) -1 -1 false '() '())
;; STRATEGY : use template of node on n

(define (proceed-if-shape-unselected is-circle? n)
  (make-node (node-is-shape-circle? n) (node-position n)
                    (node-dx n) (node-dy n)
                    (node-selected? n)
                    (add-shape-to-selected-shapes is-circle? (node-children n))
                    (node-parent n)))

;; TESTS
(begin-for-test
  (check-equal? (add-shape-to-selected-shapes true
                 (list (make-node true (make-posn 250 20) -3 2 true '() '())))
                
                (list (make-node #true (make-posn 250 20) -3 2 true
                (list (make-node #true (make-posn 250 80) -3 62 false '()
                       (make-node #true (make-posn 250 20) -3 2 true '() '())))
                 '()))
                "add-shape-to-selected-shapes added shapes incorrectly")
  
  (check-equal? (add-shape-to-selected-shapes true
                 (list (make-node true (make-posn 250 20) -3 2 false '() '())))
                (list (make-node #true (make-posn 250 20) -3 2 false '() '()))
                 "add-shape-to-selected-shapes added shapes incorrectly"))
                

;; add-child : LON Node Boolean -> LON
;; GIVEN    : a list of node, a parent node and a Boolean
;; indicating which shape to add
;; RETURNS  : list of node with new child added
;; EXAMPLES :
;; (add-child '() node1 true) =
;; (list (make-node true (make-posn 50 110) -1 59 false '()
;;       (make-node true (make-posn 50 50) -1 1 true '() '())))
;; STRATEGY : cases on LON lst

(define (add-child lst n is-circle?)
  (if (empty? lst)
      (cons (create-shape  is-circle?
             (make-posn (posn-x (node-position n))
                        (+ (posn-y (node-position n))
                           (* CIRCLE 3))) n) empty)
      (cons (create-shape  is-circle?
             (make-posn (- (get-x-of-lson lst) (* CIRCLE 3))
                        (+ (posn-y (node-position n))
                           (* CIRCLE 3))) n) lst)))

;; TESTS
(begin-for-test
  (check-equal? (add-child
                 '()
                (make-node true (make-posn 250 20) -3 2 true '() '()) true)
                (list
                 (make-node true (make-posn 250 80) -3 62 false '()
                 (make-node true (make-posn 250 20) -3 2 true '() '())))
                "add-child added child incorrectly")

  (check-equal? (add-child
                 (list
                  (make-node true (make-posn 250 80) 9 44 false '()
                  (make-node true (make-posn 250 20) 9 -16 true '() '())))
                 (make-node true (make-posn 250 20) 9 -16 false
                   (list (make-node true (make-posn 250 80) 9 44 false '()
                         (make-node true (make-posn 250 20) 9 -16 true '()
                   '()))) '()) true)
                
                 (list (make-node true (make-posn 190 80) -51 44 false '()
                       (make-node true (make-posn 250 20) 9 -16 false
                        (list
                         (make-node true (make-posn 250 80) 9 44 false '()
                         (make-node true (make-posn 250 20) 9 -16 true '()
                         '()))) '()))
                       (make-node true (make-posn 250 80) 9 44 false '()
                       (make-node true (make-posn 250 20) 9 -16 true '() '())))
                  "add-child added child incorrectly")
  (check-equal? (world-after-key-event (make-world list-of-tree) "s")
                (make-world
                 (list
                  (make-node #true (make-posn 30 30) -1 -1 false
                 (list
                  (make-node #true (make-posn 30 73) -1  -1 false '() '()))
                 '())
                  (make-node  false  (make-posn 70 70) -1 -1 #true (list
                   (make-node false (make-posn 10 130) -61 59 false '()
                   (make-node  false (make-posn 70 70)  -1 -1 #true
                 (list
                  (make-node #true (make-posn 70 113) -1 -1 false '() '()))
                 '()))
                   (make-node  #true (make-posn 70 113) -1 -1 false '() '()))
                              '())))
                 "world after adding sqaure to it")
  (check-equal? (world-after-key-event (make-world list-of-tree) "c")
                (make-world
                 (list
                  (make-node #true (make-posn 30 30) -1 -1 false
                 (list
                  (make-node #true (make-posn 30 73) -1  -1 false '() '()))
                 '())
                  (make-node  false  (make-posn 70 70) -1 -1 #true (list
                   (make-node #true (make-posn 10 130) -61 59 false '()
                   (make-node  false (make-posn 70 70)  -1 -1 #true
                 (list
                  (make-node #true (make-posn 70 113) -1 -1 false '() '()))
                 '()))
                   (make-node  #true (make-posn 70 113) -1 -1 false '() '()))
                              empty)))
                "world after adding circle to it")
  (check-equal? (world-after-key-event (make-world list-of-tree) "d")
                (make-world
                 (list
                  (make-node  #true  (make-posn 30 30) -1  -1 false
                  (list (make-node #true (make-posn 30 73) -1 -1 false
                                     '() '())) '())
                  (make-node #true (make-posn 70 113) -1 -1 false '() '())))
                "world after deleting a shape")
  (check-equal? (world-after-key-event (make-world list-of-tree2) "e")
                (make-world
                (list
                (make-node #true (make-posn 30 30) -1  -1 false '() empty)
                (make-node false (make-posn 70 70) -1 -1 false '() empty)))
                "world after a random key event")
  (check-equal? (world-with-new-circle-added (make-world list-of-tree2))
                (make-world
                (list
                (make-node #true (make-posn 250 20) -1 -1 false '() '())
                (make-node  #true (make-posn 30 30) -1 -1  false '() empty)
                (make-node false (make-posn 70 70) -1 -1 false '() empty)))
                "world after adding circle to it"))
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create-shape : BOOLEAN POSN NODE -> Node
;; GIVEN    : a boolean indicating the shape i.e. whether circle or square
;;         a Posn struct indication the x and y coordinates and the parent node.
;; RETURNS  : a Node in shape of circle
;; EXAMPLES :
;; (create-shape true (make-posn 30 30) node1) = 
;;     (make-node #true (make-posn 30 30) -21 -21 false '()
;;     (make-node false (make-posn 50 50) -1 -1 false '() '()))
;; STRATEGY : Use template for Node on n.

(define (create-shape is-circle? position n)
  (make-node is-circle?
             position
             (- (posn-x position)
                (- (posn-x (node-position n)) (node-dx n)))
             (- (posn-y position)
                (- (posn-y (node-position n)) (node-dy n)))
             false empty n))


;; get-x-of-lson : LON -> PosInt
;; GIVEN    : a list of nodes
;; RETURNS  : x coordinate of the lest most node
;; EXAMPLES : 
;; (get-x-of-lson list-of-tree3) = 30
;; STRATEGY : use HOF foldr on lst

(define (get-x-of-lson lst)
  ;; (INTEGER INTEGER -> INTEGER) INTEGER LON -> INTEGER
  (foldr
   ;;(Integer Integer) -> Integer
   ;; RETURNS : compares the two integer value and returns the lowest of two.
   (lambda (x b)
     (if (< (posn-x (node-position x)) b)
         (posn-x (node-position x)) b))
         (posn-x (node-position (first lst))) (rest lst)))

;; TEST
(begin-for-test
  (check-equal?
   (get-x-of-lson
    (list
     (make-node true (make-posn 100 100) -1 -1 false '() '())
     (make-node true (make-posn 200 100) -1 -1 false '() '())
     (make-node true (make-posn 40 100) -1 -1 false '() '())
     (make-node true (make-posn 110 100) -1 -1 false '() '()))) 40
     "get-x-of-lson returned incorrect values"))                 
     
;; world-with-new-square-added : World -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one, but with a new square added
;; at the top center of the canvas
;; EXAMPLE:
;; (world-with-new-square-added (make-world list-of-tree3)) =
;;          (make-world
;;                 (list
;;                  (make-node false (make-posn 250 20) -1 -1 false '() '())
;;                  (make-node #true (make-posn 30 30) -1 -1 false
;;                   (list (make-node #true (make-posn 30 73) -1 -1 false
;;                                    '() '())) false)))
;; STRATEGY: cases on world w

(define (world-with-new-square-added w)
  (if (any-shape-selected? (world-trees w))
      (make-world
       (add-shape-to-selected-shapes false (world-trees w)))
      (make-world
       (cons (make-node false
              (make-posn (/ CANVAS-WIDTH 2) 20) -1 -1 false empty empty)
        (world-trees w)))))

;; TEST
(begin-for-test
  (check-equal? (world-with-new-square-added (make-world list-of-tree3))
                (make-world
                 (list
                  (make-node false (make-posn 250 20) -1 -1 false '() '())
                  (make-node #true (make-posn 30 30) -1 -1 false
                   (list (make-node #true (make-posn 30 73) -1 -1 false
                                    '() '())) empty)))
                "world-with-new-square-added added square incorrectly"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;world-to-trees : World -> ListOfTree
;;GIVEN: a World
;;RETURNS: a list of all the trees in the given world.

(define (world-to-trees w)
  (world-trees w))

;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLE: Consider the tree represented as follows:
;;
;;                 A
;;                 |
;;       +---+-----+-----+
;;       |   |     |     |
;;       B   C     D     E
;;           |           |
;;         +---+      +-----+
;;         |   |      |     |
;;         F   G      H     I
;;
;; If tree-to-root is given the subtree rooted at C, it should return the
;; data structure associated with node C. This data structure may or may
;; not include data associated with rest of the tree, depending on
;; whether you have chosen to represent nodes differently from trees.

(define (tree-to-root t)
  t)
;
;; tree-to-sons : Tree -> ListOfTree
;; GIVEN    : a tree
;; RETURNS  : the data associated with the immediate subtrees of the given
;; tree.  
;; EXAMPLE  : In the situation above, if tree-to-sons is given the subtree
;; rooted at C, it should return a list consisting of the subtree rooted
;; at F and the subtree rooted at G.
;; STRATEGY : combine simpler functions.

(define (tree-to-sons t)
  (node-children t))

;; node-to-center : Node -> Posn
;; GIVEN    : A Node.
;; RETURNS  : the center of the given node as it is to be displayed on the
;; scene.
;; EXAMPLE  : (node-to-center node1) = (make-posn 50 50)
;; STRATEGY : combine simpler functions.

(define (node-to-center n)
  (node-position n))

;; TESTS
(begin-for-test
 (check-equal? (node-to-center
                (make-node false (make-posn 130 80) -125 59 false '() '()))
               (make-posn 130 80)
               "node to center returned incorrect output"))

;; node-to-selected? : Node -> Boolean
;; GIVEN    : A Node.
;; RETURNS: true iff the given node is selected.
;; STRATEGY : combine simpler functions.

(define (node-to-selected? n)
  (node-selected? n))

;; TESTS
(begin-for-test
 (check-equal? (node-to-center node1) (make-posn 50 50))
 (check-equal? (tree-to-sons node1) empty)
 (check-equal? (tree-to-root node1)
               (make-node false (make-posn 50 50)  -1  -1
                          false '() '()))
 (check-equal? (world-to-trees  (make-world list-of-tree))
                list-of-tree
                "world-to-trees returned incorrect output."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;