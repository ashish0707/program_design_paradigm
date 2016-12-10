#lang racket

(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")
(require "Throbber.rkt")
(require "Politician.rkt")
(require "Clock.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide
 make-metatoy
 run
 make-throbber
 make-clock
 make-politician
 Metatoy<%>
 Toy<%>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

;; CANVAS
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define CANVAS-WIDTH-HALF (/ CANVAS-WIDTH 2))
(define CANVAS-HEIGHT-HALF (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run     : PosNum -> Void
;; GIVEN   : a frame rate (in seconds/tick)
;; EFFECT  :  Creates a Container, and places a MetaToy with
;;            no toys in that Container.
;;            The function then runs the Container at the given
;;            frame rate using WidgetWorks.

(define (run rate)
  (local
    (
     (define new-container (container-init CANVAS-WIDTH CANVAS-HEIGHT)))
    (begin
      (send new-container add-stateful-widget (initial-world))
      (send new-container run rate)))) 

  
;; make-metatoy : ListOfToy -> Metatoy
;; GIVEN    : a list of toys
;; RETURNS  : a Metatoy with the given list of toys.
;; EXAMPLE  : see test cases
;; STRATEGY : combine simpler functions
(define (make-metatoy toys)
  (new Metatoy% [toys toys]))


;; initial-world : -> Metatoy
;; RETURNS  : an object of class Metatoy%
;; EXAMPLE  : check test cases
;; STRATEGY : combine simpler functions
(define (initial-world) 
  (make-metatoy empty))


;; Constructor template for Metatoy%:
;; A Metatoy is a (new Metatoy% [objs ListOfWidget])
;; Interpretation: An object of class Metatoy% takes 
;; signals from big-bang and distributes them to
;; its toys as appropriate.
(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field toys)  ;  ListOfWidget
    (field [THROBBER-TOY-EVENT "t"]) ;event to create a throbber toy
    (field [CLOCK-TOY-EVENT "c"]) ;  event to create a clock toy
    (field [POLITICIAN-EVENT "p"]) ;event to create a politician toy
     
    (super-new)
  

    ;; after-tick : -> void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this widget to the
    ;; state it should have following a tick.
    (define/public (after-tick)
      (for-each (lambda (toy) (send toy after-tick)) toys))

    
    ;; to-scene : Scene -> Scene
    ;; GIVEN    : A scene
    ;; RETURNS  : A Scene with toys added 
    ;; STRATEGY : Use HOFC foldr on the SWidgets in this Metatoy
    (define/public (add-to-scene myscene)
      (foldr
       (lambda (toy base)
         (send toy add-to-scene base)) 
       myscene
       toys))
     
     
    ;; after-key-event : KeyEvent -> Metatoy
    ;; GIVEN    : A keyevent
    ;; RETURNS  : a Metatoy based on KeyEvent 't' 'c' 'p' which create
    ;;            thobber, clock and politician repectively.
    ;; STRATEGY : Cases on key event - kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev THROBBER-TOY-EVENT)
         (add-thobber-to-toys)]
         
        [(key=? kev CLOCK-TOY-EVENT)
         (add-clock-to-toys)]
        
        [(key=? kev POLITICIAN-EVENT)
         toys (add-politician-to-toys)]
        
        [else this]))
    
    
    ;; add-throber-to-world-toys : -> Void
    ;; GIVEN    : No arguments
    ;; EFFECT   : Updates Metatoy with addition of throbber toy to list of toy
    ;; STRATEGY : Combine simpler functions
    (define (add-thobber-to-toys)
      (set! toys
            (cons (make-throbber CANVAS-WIDTH-HALF CANVAS-HEIGHT-HALF) toys)))
    
    
    ;; add-clock-to-toys : -> Void
    ;; GIVEN    : No arguments
    ;; EFFECT   : Updates Metatoy with addition of clock to list of toy
    ;; STRATEGY : Combine simpler functions
    (define (add-clock-to-toys)
      (set! toys
            (cons (make-clock  CANVAS-WIDTH-HALF CANVAS-HEIGHT-HALF) toys)))
    
    
    ; add-politician-to-toys : -> Void
    ; EFFECT   : Updates Metatoy with addition of a politician to list of toy
    ; STRATEGY : Combine simpler functions
    (define (add-politician-to-toys)
      (set! toys
            (cons (make-politician CANVAS-WIDTH-HALF CANVAS-HEIGHT-HALF) toys)))
    
    
    ;; after-mouse-event : NonNegInt NonNegInt MouseEvent -> Metatoy
    ;; GIVEN   : two non negative integers indicating x and y position of 
    ;;           the mouse and a MouseEvent.
    ;; RETURNS : a Metatoy which follows the given MouseEvent
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
     

    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : Updates the metatoy following the MouseEvent when the mouse
    ;;            is pressed down at the given mouse position mx and my
    ;; STRATEGY : Use HOF on toys
    (define/public (after-button-down mx my)
      (for-each
       ;; Toy -> Toy
       ;; RETURNS : the same toy but after after-button-down event.
       (lambda (toy)
         (send toy after-button-down mx my))
       toys))
      
      
     
    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : Updates the metatoy following the MouseEvent when the mouse
    ;;            is depressed at the given mouse position mx and my
    ;; STRATEGY : Use HOF on toys
    (define/public (after-button-up mx my)
      (for-each
       ;; Toy -> Toy
       ;; RETURNS : the same toy but after after-button-up event.
       (lambda (toy) 
         (send toy after-button-up mx my))
       toys))
    
    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : Updates the metatoy following the MouseEvent when the mouse
    ;;            is dragged at the given mouse position mx and my
    ;; STRATEGY : Use HOF on toys 
    (define/public (after-drag mx my)
      (for-each
       ;; Toy -> Toy
       ;; RETURNS : the same toy but after after-drag event.
       (lambda (toy) 
         (send toy after-drag mx my))
       toys))
       
    
    ;; after-move : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : Updates the metatoy following the MouseEvent,
    ;;            other than drag, button-down,
    ;;            or button-up.
    ;; STRATEGY : Use HOF on toys
    (define/public (after-move mx my)
      (for-each
       ;; Toy -> Toy
       ;; RETURNS : the same toy but after after-button-move event.
       (lambda (toy) 
         (send toy after-move mx my))
       toys))
     
    
    ;; get-toys : -> ListOfToy
    ;; RETURNS : a list of toys stored in Metatoy. 
    (define/public (get-toys)
      toys)

    ;; for-test:explain-state : -> ListOfStateVariable 
    ;; GIVEN : no argument
    ;; RETURNS : return a list of variable of all objs of the given state
    ;; STRATEGY: Combine simpler functions
    (define/public (for-test:explain-state)
      (metatoy-test toys))

    ;; metatoy-test : ListOfToy -> ListOfStateVariable 
    ;; GIVEN : ListOfToy
    ;; RETURNS : return a list of variable of all objs of the given state
    ;; EXAMPLE :
    ;; (send METATOY-CLOCK-AFTER-C for-test:explain-state)
    ;;  (list 100 100 #f 0 0 0))
    ;; STRATEGY: Combine simpler functions
    (define (metatoy-test all-obj)
      (foldr
       ;; Toy LOV -> LOV
       ;; It returns list of variables which contains the list of variables of
       ;;  all objs of ListOfToy
       (lambda (x y)
         (append
          (send x for-test:explain-state) y))
       empty
       all-obj))
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING FRAMEWORK - WORLD ;;;;;;;;;;;;;;;;;;;;;;



(begin-for-test
  (local
    (
     (define INITIAL-METATOY  (initial-world))
     (define INITIAL-THROBBER (make-throbber 100 100))
     (define INITIAL-CLOCK (make-clock 100 100))
     (define INITIAL-POLITICIAN (make-politician 100 100))
     (define METATOY-AFTER-C (make-metatoy (list INITIAL-CLOCK)))
     (define METATOY-AFTER-T (make-metatoy (list INITIAL-THROBBER)))
     (define METATOY-AFTER-P (make-metatoy (list INITIAL-POLITICIAN)))
<<<<<<< HEAD
     (define METATOY-ADD-TO-SCENE (send METATOY-AFTER-P add-to-scene EMPTY-CANVAS))
     (define w0 (make-metatoy empty))
     )
=======
     (define METATOY-ADD-TO-SCENE
       (send METATOY-AFTER-P add-to-scene EMPTY-CANVAS))
     (define w0 (make-metatoy empty)))
>>>>>>> origin/master

    
    (check-equal? (send w0 get-toys) empty
                  "list of toy should be empty for w0")

    (send INITIAL-METATOY after-key-event "c")
    (check-equal? (send INITIAL-METATOY for-test:explain-state)
                  (list 250 300 -1 -1 #f)
                  "returns metatoy with clock added")


    (send INITIAL-METATOY after-key-event "t")
    (check-equal? (send INITIAL-METATOY for-test:explain-state)
                  (list 250 300 -1 -1 5 #f #t 250 300 -1 -1 #f)
                  "returns metatoy with clock as well as a throbber added")


    (send INITIAL-METATOY after-key-event "p")
    (check-equal? (send INITIAL-METATOY for-test:explain-state)
<<<<<<< HEAD
                  (list 250 300 250 300 0 250 300 -1 -1 5 #f #t 250 300 -1 -1 #f)
                  "returns metatoy with clock, throbber and a politician added")

    (send INITIAL-METATOY after-key-event "k")
    (check-equal? (send INITIAL-METATOY for-test:explain-state)
                  (list 250 300 250 300 0 250 300 -1 -1 5 #f #t 250 300 -1 -1 #f)
                  "returns the metatoy as it is")
=======
                (list 250 300 250 300 0 250 300 -1 -1 5 #f #t 250 300 -1 -1 #f)
                "returns metatoy with clock, throbber and a politician added")

    (send INITIAL-METATOY after-key-event "k")
    (check-equal? (send INITIAL-METATOY for-test:explain-state)
                (list 250 300 250 300 0 250 300 -1 -1 5 #f #t 250 300 -1 -1 #f)
                "returns the metatoy as it is")
>>>>>>> origin/master

    (send METATOY-AFTER-C after-mouse-event 100 200 "button-down")
    (check-equal? (send METATOY-AFTER-C for-test:explain-state)
                  (list 100 100 -1 -1 #f)
<<<<<<< HEAD
                  "returns metatoy with clock in it when the mouse is pressed down")
=======
                  "returns metatoy with clock in it when the mouse is
                   pressed down")
>>>>>>> origin/master

    (send METATOY-AFTER-C after-mouse-event 100 200 "button-up")
    (check-equal? (send METATOY-AFTER-C for-test:explain-state)
                  (list 100 100 -1 -1 #f)
<<<<<<< HEAD
                  "returns metatoy with clock in it when the mouse event is button-up")
=======
                  "returns metatoy with clock in it when the mouse event is
                   button-up")
>>>>>>> origin/master

    (send METATOY-AFTER-T after-mouse-event 100 200 "drag")
    (check-equal? (send METATOY-AFTER-T for-test:explain-state)
                  (list 100 100 -1 -1 5 #f #t)
                  "returns metatoy with clock in it when the mouse is dragged")

    (send METATOY-AFTER-T after-mouse-event 100 200 "move")
    (check-equal? (send METATOY-AFTER-T for-test:explain-state)
                  (list 100 100 -1 -1 5 #f #t)
<<<<<<< HEAD
                  "returns metatoy with clock in it when the mouse event is other than
                   button-up, button-down or drag")
=======
                  "returns metatoy with clock in it when the mouse event is 
                   other than button-up, button-down or drag")
>>>>>>> origin/master

    (send METATOY-AFTER-P after-tick)
    (check-equal? (send METATOY-AFTER-P for-test:explain-state)
                  (list 115 120 250 300 0)
                  "returns the metatoy after a tick")

    
    ))
