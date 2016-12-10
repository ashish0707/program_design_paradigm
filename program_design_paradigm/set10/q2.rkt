#lang racket

(require rackunit)
(require "extras.rkt")
(require "interfaces2.rkt")
(require "Sblock.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide

 run
 make-block
 cubelets-init
 Sblock<%>
 SWidget<%>
)

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

;; CANVAS
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define CANVAS-WIDTH-HALF (/ CANVAS-WIDTH 2))
(define CANVAS-HEIGHT-HALF (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; run : PosNum -> Metatoy
;; GIVEN   : a frame rate (in seconds/tick)
;; EFFECT  : creates a world with slbocks toys in it, and runs it using big-bang
;;           at the given frame rate. 
;; RETURNS : the final state of the Metatoy
(define (run rate)
  (local
    (
     (define new-container (container-init CANVAS-WIDTH CANVAS-HEIGHT)))
  (begin
    (send new-container add-stateful-widget (initial-world))
    (send new-container run rate)))) 
 

;; cubelets-init : -> Container
;; GIVEN   : no arguments
;; RETURNS : a Container, initially with no blocks, which when run, will
;; run in a 600x500 canvas and process the events in the description above.
(define (cubelets-init)
   (local
    (
     (define world (container-init CANVAS-WIDTH CANVAS-HEIGHT)))
  (begin
    (send world add-stateful-widget (initial-world))
    world)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world : -> Metatoy
;; RETURNS  : an object of class WorldState
;; EXAMPLE  : check test cases
;; STRATEGY : combine simpler functions
(define (initial-world) 
  (new WorldState%: [toys empty]))
 

;; Constructor template for WorldState:
;; A Metatoy is a (new WorldState [objs ListOfWidget])
;; Interpretation: An object of class WorldState takes 
;; signals from big-bang and distributes them to
;; its toys as appropriate.
(define WorldState%:
  (class* object% (SWidget<%>)
    
    (init-field toys)                     ;  ListOfWidget
    (field [SBLOCK-TOY-EVENT "b"])        ; event to create a new sblock

    (field [saved-mx CANVAS-WIDTH-HALF])  ; saves the x coordinate of the center
                                          ; used to spawn new sblock.
    (field [saved-my CANVAS-HEIGHT-HALF]) ; saves the y coordinate of the center
                                          ; used to spawn new sblock.

    (super-new)
    
    ;; after-tick : -> void
    ;; GIVEN  : no arguments
    ;; EFFECT : updates this widget to the
    ;;          state it should have following a tick.
     (define/public (after-tick)
      (for-each (lambda (toy) (send toy after-tick)) toys))

    
    ;; to-scene : Scene -> Scene
    ;; GIVEN    : a scene.
    ;; RETURNS  : the same scene with all the slbocks in this metatoy
    ;;            painted on it.
    ;; STRATEGY : Use HOFC foldr on the SWidgets in this Metatoy
    (define/public (add-to-scene myscene)
      (foldr
        (lambda (toy base)
          (send toy add-to-scene base)) 
        myscene
        toys))
     
     
    ;; after-key-event : KeyEvent -> Void
    ;; RETURNS  : a Metatoy based on KeyEvent 'b' which create
    ;;            sblock.
    ;; STRATEGY : Cases on key event - kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev SBLOCK-TOY-EVENT)
          (add-sblock-to-toys)]
        
        [else this]))
     
       
    ;; add-sblock-to-toys : -> Void 
    ;; EFFECT   : A sblock toy added to list of toy in Metatoy.
    ;; STRATEGY : Combine simpler functions
    (define (add-sblock-to-toys)
      (local
        ((define new-sblock
           (make-block saved-mx saved-my toys)))
      (begin  
         (for-each
         (lambda (sblock)
           (send sblock add-to-playground-sblocks new-sblock)) toys)

        (set! toys
           (cons new-sblock toys))
         
        ))) 
     
        
    ;; after-mouse-event : NonNegInt NonNegInt MouseEvent -> Metatoy
    ;; GIVEN   : two non negative integers indicating x and y position of 
    ;;           the mouse and a MouseEvent.
    ;; EFFECT  : a Metatoy which follows the given MouseEvent
    ;; STRATGY : Cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (after-button-down mx my)] 
        [(mouse=? mev "drag")
         (after-drag mx my)]
        [(mouse=? mev "button-up")
         (after-button-up mx my)]
        [else (after-move mx my)])) 

    
    ;; after-button-down : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a Metatoy which should follow the MouseEve nt when the mouse
    ;;            is pressed down at the given mouse position
    ;; STRATEGY : Use HOF on toys
    (define/public (after-button-down mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my)
     (for-each
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after after-button-down event.
        (lambda (toy)
          (send toy after-button-down mx my))
        toys)
     ))
       
        
      
    ;; after-button-up : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a Metatoy which should follow the MouseEvent when the mouse
    ;;            is depressed at the given mouse position.
    ;; STRATEGY : Use HOF on toys
    (define/public (after-button-up mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my)
       (for-each
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after after-button-up event.
        (lambda (toy) 
          (send toy after-button-up mx my))
        toys)
       )) 
     
    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a Metatoy which should follow the MouseEvent
    ;;            when the mouse is dragged at the given mouse position.
    ;; STRATEGY : Use HOF on toys 
    (define/public (after-drag mx my)
      (for-each
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after after-drag event.
        (lambda (toy) 
          (send toy after-drag mx my))
        toys))
        
    
    ;; after-move : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a Metatoy which should follow the MouseEvent
    ;             when there is any other mouse event like drag, button-down,
    ;;            or button-up.
    ;; STRATEGY : Use HOF on toys
    (define/public (after-move mx my)
      (for-each
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after after-button-move event.
        (lambda (toy) 
          (send toy after-move mx my)) 
        toys))
     
    
    ;; for-test:explain-state : -> ListOfStateVariable 
    ;; GIVEN    : no argument
    ;; RETURNS  : return a list of variable of all objs of the given state
    ;; STRATEGY : Combine simpler functions
    (define/public (for-test:explain-state)
      (world-test toys))

    ;; world-test : ListOfToy -> ListOfStateVariable 
    ;; GIVEN   : no argument
    ;; RETURNS : return a list of variable of all objs of the given state 
    ;; EXAMPLE :
    ;; (send METATOY-CLOCK-AFTER-C-KEY for-test:explain-state)
    ;;  (list INITIAL-TOY-X INITIAL-TOY-Y false 0 0 0))
    ;; STRATEGY: Combine simpler functions
     (define (world-test all-obj)
      (foldr
       ;; Toy LOV -> LOV
       ;; RETURNS : the list of toys which contains the list of variables of
       ;;          toys.
       (lambda (x y)
         (append
         (send x for-test:explain-state) y))
       empty
       all-obj))
     
    )) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING FRAMEWORK - WORLD ;;;;;;;;;;;;;;;;;;;;;;

;(begin-for-test
;  (local
;    (
;     (define INITIAL-METATOY (initial-world))
;     (define INITIAL-SBLOCK (make-block 100 100 empty))
;     (define METATOY-WITH-INITIAL-SBLOCK
;       (new WorldState%: [toys (list INITIAL-SBLOCK)]))
;     (define METATOY-2 (new WorldState%: [toys (list INITIAL-SBLOCK)]))
;     (define METATOY-ADD-TO-SCENE
;       (send METATOY-WITH-INITIAL-SBLOCK add-to-scene EMPTY-CANVAS))
;     (define CUBELETS (cubelets-init))
;     )
;
;    ;; calling after-tick
;    (send METATOY-WITH-INITIAL-SBLOCK after-tick)
;    (check-equal? (send METATOY-WITH-INITIAL-SBLOCK for-test:explain-state)
;                  (list 100 100 -1 -1 empty #f)
;                   "sblock should not respond to after tick")
;
;    
;    ;; calling after-key-event
;    (send INITIAL-METATOY after-key-event "b")
;    (check-equal?
;     (send INITIAL-METATOY for-test:explain-state)
;     (list 300 250 -1 -1 empty #f)
;     "sblock added incorrectly")
;
;    (send INITIAL-METATOY after-key-event "c")
;    (check-equal?
;     (send INITIAL-METATOY for-test:explain-state)
;     (list 300 250 -1 -1 empty #f)
;     "sblock should ignore any key event except 'b' ")
;
;    ;; calling mouse-events
;    ;; button-down mouse button
;    (send METATOY-WITH-INITIAL-SBLOCK after-mouse-event 109 109 "button-down")
;    (check-equal?
;     (send METATOY-WITH-INITIAL-SBLOCK for-test:explain-state)
;     (list 100 100 -9 -9 empty true)
;     "sblock should be selected")
;
;    ;; button-up mouse button
;    (send METATOY-WITH-INITIAL-SBLOCK after-mouse-event 100 200 "button-up")
;    (check-equal?
;     (send METATOY-WITH-INITIAL-SBLOCK for-test:explain-state)
;     (list 100 100 -9 -9 empty false)
;     "given sblock should be selected at button-up")
;
;    ;; after-drag mouse button
;    (send METATOY-WITH-INITIAL-SBLOCK after-mouse-event 109 109 "button-down")
;    (send METATOY-WITH-INITIAL-SBLOCK after-mouse-event 110 110 "drag")
;    (check-equal?
;     (send METATOY-WITH-INITIAL-SBLOCK for-test:explain-state)
;     (list 101 101 -9 -9 '() #t)
;     "drag motion working incorrectly")
;
;     ;; after-move mouse button
;    (send METATOY-WITH-INITIAL-SBLOCK after-mouse-event 100 200 "move")
;    (check-equal?
;     (send METATOY-WITH-INITIAL-SBLOCK for-test:explain-state)
;     (list 101 101 -9 -9 '() #t)
;     "sblock should ignore after-move and remain in same state")
;
;    
;    ;; calling after-key-event
;    (send INITIAL-METATOY after-key-event "b")
;    (check-equal?
;     (send INITIAL-METATOY for-test:explain-state)
;    (list
;        300 250 -1 -1
;            (list (list 300 300 -1 -1 '() #f)) #f 300 250 -1 -1
;            (list (list 300 300 -1 -1 '() #f)) #f)
;     "sblock added incorrectly")
;    
;    ))

(define SBLOCK-1 (make-block 30 30 '()))
	(define SBLOCK-2 (make-block 60 60 (list SBLOCK-1)))
	(define SBLOCK-3 (make-block 90 90 (list SBLOCK-1 SBLOCK-2)))
	(define SBLOCK-4 (make-block 120 120 (list SBLOCK-1 SBLOCK-2 SBLOCK-3)))

(begin-for-test

   (check-equal?
   (local
    ()
    (send SBLOCK-1 after-button-down 34 34)
    (send SBLOCK-1 after-drag 40 40)
    (send SBLOCK-1 after-button-up 40 40)
    (send SBLOCK-1 sblock-x))
   36)
   
   (check-equal?
     (local () (send SBLOCK-1 sblock-y))
   36)

   (check-equal?
   
   (or (equal?
        (local
         ()
         (send SBLOCK-1 add-teammate SBLOCK-2)
         (send SBLOCK-1 add-teammate SBLOCK-3)
         (length (send SBLOCK-1 get-team)))
        2)
       (equal?
        (local
         ()
         (send SBLOCK-3 after-button-down 92 92)
         (send SBLOCK-3 after-drag 57 57)
         (send SBLOCK-3 after-drag 32 32)
         (send SBLOCK-3 after-button-up 32 32)
         (length (send SBLOCK-1 get-team)))
        2)) true)
   

(check-equal?
   (or (equal?
        (local
         ()
         (send SBLOCK-3 add-teammate SBLOCK-4)
         (length (send SBLOCK-4 get-team)))
        3)
       (equal?
        (local
         ()
         (send SBLOCK-4 after-button-down 122 122)
         (send SBLOCK-4 after-drag 35 35)
         (send SBLOCK-4 after-button-up 35 35)
         (print (length (send SBLOCK-4 get-team)))
         (print (length (send SBLOCK-1 get-team))) 
         (length (send SBLOCK-1 get-team)))
        3)) true)

(check-equal?
   (local ()
     (length (send SBLOCK-3 get-team)))
   3)

 (check-equal?
   (local ()
     (length (send SBLOCK-1 get-team)))
   3)


)








