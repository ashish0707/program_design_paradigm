#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "MainController.rkt")
(require "Model-test.rkt")

(provide HandleController%)


;; A HandleController is a (new HandleController%)
;; INTERPRETATION:
;; HandleController% represents a HandleController with listed properties.
;; This class is responsible for the handle's behaviour like drag,
;; button down and button up.
(define HandleController%
  (class* MainController% (Controller<%>)

  (inherit-field    
     model       ; the unique model object inherited from main controller
     
     x ; x coordinate of the center of HandleController
     y ; y coordinate of the center of HandleController
     saved-mx ; difference between mouse and handle controllers x cord.
     saved-my ; difference between mouse and handle controllers y cord.
     
     dot-x ; x coordinate of the particle
     dot-y ; y coordinate of the particle
     
     width            ; width of the controller
     height           ; height of the controller
     half-width       ; half width of the controller
     half-height      ; height of the controller
     handle-selected? ; true if the handle is selected
     handle-x-offset  ; offset for displaying handle
     handle-y-offset  ; offset for displaying handle
     handle-size      ; size of the handle
     )

    (field [paused? false]) ; determies if the animation is paused.
        
    (super-new)
    (set! handle-x-offset 25) ;x offset for handle
    (set! handle-y-offset 25) ;y offset for handle.
    
    (send model register this) ;register this controller with model.

    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN   : the location of a button-down event.
    ;; EFFECT  : updates the value of saved-mx and saved-my for handle
    ;;           or if cursor is in controller it halts the simulation by 
    ;;           updating model and at the same time it updates saved-mx and 
    ;;           saved-my for dragging paticle.
    ;; STRATEGY: Cases on whether the point is inside this object.
    (define/override (after-button-down mx my)
      (cond
        [(in-handle? mx my)
         (begin
           (set! handle-selected? true)
           (set! saved-mx (- mx x))
           (set! saved-my (- my y))
           )]
        [(in-controller? mx my)
         (begin
           (send model execute-command true)
           (set! paused? true)
           (set! saved-mx (- mx dot-x))
           (set! saved-my (- my dot-y))
           )]
        [else 0]))


    ;; in-handle? : Nat Nat -> Boolean
    ;; GIVEN    : a location of a point on the canvas
    ;; RETURNS  : true iff the point is inside controller.
    ;; STRATEGY : call superclass function
    (define/override (in-handle? mx my)
      (super in-handle? mx my))

    ;; in-controller? : Nat Nat -> Boolean
    ;; GIVEN    : a location of a point on the canvas
    ;; RETURNS  : true iff the point is inside controller.
    ;; STRATEGY : call superclass function
    (define/override (in-controller? mx my)  
      (super in-controller? mx my))
   

    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN    : the coordinates of point on button-up event
    ;; EFFECT   : unpause the simulation and handle and updates the model
    ;; STRATEGY : combine simpler functions
    (define/override (after-button-up mx my)
      (begin
        (set! paused? false)
        (send model execute-command false)
        (set! handle-selected? false)))


    ;; controller ignore these methods
    (define/override (after-key-event kev) this)
    (define/override (after-move mx my) this)
    (define/override (after-drag mx my) this)
    (define/override (add-to-scene scene) this)

    ;; For Test cases
    (define/public (for-test:explain-state)
      (list saved-mx saved-my handle-x-offset handle-y-offset))

      ;; RETURNS : whether controller is selected
    (define/public (for-test:handle-selected?)
      handle-selected?)
    
    ))

;;;;;;;;;;;;;TESTING-FRAMEWORK;;;;;;;;;;;;;;


(begin-for-test
  (local
    ((define Controller1 (new HandleController% [model (new Model%)]))
     (define hc2 (new HandleController% [model (new Model%)])))

    (check-equal?
      (local
        ()
        (send hc2 after-button-down 275 225)
        (send hc2 for-test:handle-selected?))
       false 
      "Incorrect value of handle-selected, must be false")

     (check-equal?
      (local
        ()
        (send hc2 after-button-down 200 175)
        (send hc2 for-test:handle-selected?))
       true 
      "Incorrect value of handle-selected, must be true")

    (send Controller1 after-button-down 205 230)
    (check-equal? (send Controller1 for-test:explain-state)
                  (list 0 0 25 25)
                  "Mouse Should be inside the handle")  
    
    (send Controller1 after-button-down 350 350)
    (check-equal? (send Controller1 for-test:explain-state)
                  (list 0 0 25 25)
                  "Mouse should be inside the controller")

    (send Controller1 after-button-down 10 20)
    (check-equal? (send Controller1 for-test:explain-state)
                  (list 0 0 25 25)
                  "Incorrect position of Mouse. Should not be
                   outside the controller after-button-down")

    (send Controller1 after-button-up 225 250)
    (check-equal? (send Controller1 for-test:explain-state)
                  (list 0 0 25 25)
                  "Incorrect position of Mouse. Should not be
                   outside the controller after-button-up")


    (send Controller1 after-move 225 250)
    (check-equal? (send Controller1 for-test:explain-state)
                  (list 0 0 25 25)
                  "Incorrect position of Mouse. Should not be
                   outside the controller after-move")

    (send Controller1 after-drag 225 250)
    (check-equal? (send Controller1 for-test:explain-state)
                  (list 0 0 25 25)
                  "Incorrect position of Mouse. Should not be
                   outside the controller after-drag")

    (send Controller1 after-key-event "v")
    (check-equal? (send Controller1 for-test:explain-state)
                  (list 0 0 25 25)
                  "controller should not change its state")

    (send Controller1 add-to-scene (empty-scene 600 500))
    (check-equal? (send Controller1 for-test:explain-state)
                  (list 0 0 25 25)
                  "controller should not change its state
                   as it doesnt respond to add-to-scene")

    

    ))







