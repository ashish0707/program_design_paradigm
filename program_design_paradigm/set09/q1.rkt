#lang racket

(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")
(require "Throbber.rkt")
(require "Politician.rkt")
(require "Clock.rkt")
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

;; run : PosNum -> Metatoy
;; GIVEN   : a frame rate (in seconds/tick)
;; EFFECT  : creates a MetaToy with no toys in it, and runs it using big-bang
;;           at the given frame rate. 
;; RETURNS : the final state of the Metatoy
(define (run rate)
  (big-bang (initial-world)
            (on-tick
             (lambda (w) (send w after-tick))
             rate)
            (on-draw
             (lambda (w) (send w to-scene)))
            (on-key
             (lambda (w kev)
               (send w after-key-event kev)))
            (on-mouse
             (lambda (w mx my mev)
               (send w after-mouse-event mx my mev)))))


;; make-metatoy : ListOfToy -> Metatoy
;; GIVEN    : a list of toys
;; RETURNS  : a Metatoy with the given list of toys.
;; EXAMPLE  : see test cases
;; STRATEGY : combine simpler functions
(define (make-metatoy toys)
 (new Metatoy% [toys toys]))


;; initial-world : -> World
;; RETURNS  : an object of class Metatoy%
;; EXAMPLE  : check test cases
;; STRATEGY : combine simpler functions
(define (initial-world) 
  (make-metatoy empty))


;; Constructor template for Metatoy%:
;; A Metatoy is a (new Metatoy% [objs ListOfWidget][x Integer] [y Integer])
;; Interpretation: An object of class Metatoy% takes 
;; signals from big-bang and distributes them to
;; its toys as appropriate.
(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field toys)  ;  ListOfWidget
    (field [THROBBER-TOY-EVENT "t"])
    (field [CLOCK-TOY-EVENT "c"])
    (field [POLITICIAN-EVENT "p"])
    
    (super-new)
    
    ;; after-tick : -> Metatoy
    ;; RETURNS  : A world like this one but after one tick.
    ;; STRATEGY : Use HOF map on the toys in world
    (define/public (after-tick)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after tick.
        (lambda (toy) (send toy after-tick))
        toys)))
    
    ;; to-scene : -> Scene
    ;; RETURNS  : A scene with all toys placed on it
    ;; STRATEGY : Use HOF foldr on the toys in world
    (define/public (to-scene)
      (foldr
       ;; Widget Scene -> Scene
       ;; RETURN : a scene with toy painted on it.
       (lambda (toy scene)
         (send toy add-to-scene scene))
       EMPTY-CANVAS
       toys))
    
    
    ;; after-key-event : KeyEvent -> Metatoy
    ;; RETURNS  : a Metatoy based on KeyEvent 't' 'c' 'p' which create
    ;;            thobber, clock and politician repectively.
    ;; STRATEGY : Cases on key event - kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev THROBBER-TOY-EVENT)
         (new Metatoy% [toys (add-thobber-to-toys)])]
        
        [(key=? kev CLOCK-TOY-EVENT)
         (new Metatoy% [toys (add-clock-to-toys)])]
        
        [(key=? kev POLITICIAN-EVENT)
         (new Metatoy% [toys (add-politician-to-toys)])]
        
        [else this]))
    
    
    ;; add-throber-to-world-toys : -> ListOfToy
    ;; RETURNS  : A throbber toy added with to list of toy in Metatoy
    ;; STRATEGY : Combine simpler functions
    (define (add-thobber-to-toys)
      (cons (make-throbber CANVAS-WIDTH-HALF CANVAS-HEIGHT-HALF) toys))
    
    
    ;; add-clock-to-toys : -> ListOfToy
    ;; RETURNS  : A clock toy added with to list of toy in Metatoy
    ;; STRATEGY : Combine simpler functions
    (define (add-clock-to-toys)
      (cons (make-clock  CANVAS-WIDTH-HALF CANVAS-HEIGHT-HALF) toys))
    
    
    ; add-politician-to-toys : -> ListOfToy
    ; RETURNS  : A Politician toy added with to list of toy in Metatoy
    ; STRATEGY : Combine simpler functions
    (define (add-politician-to-toys)
      (cons (make-politician CANVAS-WIDTH-HALF CANVAS-HEIGHT-HALF) toys))
    
    
    ;; after-mouse-event : NonNegInt NonNegInt MouseEvent -> Metatoy
    ;; GIVEN   : two non negative integers indicating x and y position of 
    ;;           the mouse and a MouseEvent.
    ;; RETURNS : a Metatoy which follows the given MouseEvent
    ;; STRATGY : Cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [else (world-after-mouse-move mx my)]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
    ;; the next few functions are local functions,
    ;; not in the interface.
    
    ;; world-after-button-down : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; RETURNS  : a Metatoy which should follow the MouseEvent when the mouse
    ;;            is pressed down at the given mouse position
    ;; STRATEGY : Use HOF on toys
    (define (world-after-button-down mx my)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after after-button-down event.
        (lambda (toy)
          (send toy after-button-down mx my))
        toys)))
    
    
    ;; world-after-button-up : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; RETURNS  : a Metatoy which should follow the MouseEvent when the mouse
    ;;            is depressed at the given mouse position.
    ;; STRATEGY : Use HOF on toys
    (define (world-after-button-up mx my)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after after-button-up event.
        (lambda (toy) 
          (send toy after-button-up mx my))
        toys)))
    
    
    ;; world-after-drag : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; RETURNS  : a Metatoy which should follow the MouseEvent
    ;;            when the mouse is dragged at the given mouse position.
    ;; STRATEGY : Use HOF on toys 
    (define (world-after-drag mx my)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after after-button-down event.
        (lambda (toy) 
          (send toy after-drag mx my))
        toys)))
    
    
    ;; world-after-mouse-move : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; RETURNS  : a Metatoy which should follow the MouseEvent
    ;             when there is any other mouse event like drag, button-down,
    ;;            or button-up.
    ;; STRATEGY : Use HOF on toys
    (define (world-after-mouse-move mx my)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; RETURNS : the same toy but after after-button-move event.
        (lambda (toy) 
          (send toy after-move mx my))
        toys)))
    
    
    ;; -> ListOfToy<%>
    ;; RETURNS : a list of toys stored in Metatoy. 
    (define/public (get-toys)
      toys)
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING FRAMEWORK - WORLD ;;;;;;;;;;;;;;;;;;;;;;

(define METATOY1  (make-metatoy (list (make-clock 300 250)
                                      (make-throbber 300 250))))

(define WORLD1 (initial-world))

(define w0 (make-metatoy empty))
(define w1 (send w0 after-tick))
(define w2 (send w0 after-key-event "t"))
(define w3 (send w0 after-key-event "c"))
(define w4 (send w0 after-key-event "p"))
(define w14 (send w0 after-key-event "s"))
(define w5 (send w2 after-mouse-event 250 300 "button-down"))
(define w6 (send w5 after-tick))
(define w7 (send w1 after-mouse-event 400 150 "button-down"))
(define w8 (send w5 after-mouse-event 300 150 "button-up"))
(define w9 (send w5 after-mouse-event 400 150 "move"))
(define w10 (send w5 after-mouse-event 100 150 "drag"))
(define w11 (send (send w1 after-key-event "t")
                  after-mouse-event 100 150 "drag"))
(define w12 (send w8 to-scene))
(define w13 (send w0 get-toys))


(begin-for-test
  (check-equal? (send w0 get-toys) empty
                "list of toy should be empty")
  
  (check-equal? (send (first (send w2 get-toys)) toy-x)
                  250
                  "thobber added at incorrect location in world")
  
  (check-equal? (send (first (send w3 get-toys)) toy-x)
                  250
                  "clock added at incorrect location in world")
                       
 (check-equal?  (send (first (send w4 get-toys)) for-test:politician-state)
                '(250 300 250 10 0)
                "state of the politician is incorrect")
 
 (check-equal?  (send (first (send w2 get-toys)) for-test:selected?)
                false
                "this thobber must not be selected on button down
                 as mouse co-ordinate doesn't lie within its range")

 (check-equal?  (send (first (send w6 get-toys)) toy-data)
                6
                "this thobber must have its radius increased by 1 after tick")
 
  (check-equal? (send (first (send (send (send w1 after-key-event "t")
                  after-mouse-event 100 150 "drag") get-toys)) toy-data)
                5
                "this thobber must not be dragged as it isn't selected"))


  

  




