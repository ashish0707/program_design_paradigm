#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "PerfectBounce.rkt")

(provide MainController%)
(define DEFAULT-X-CENTER 300)
(define DEFAULT-Y-CENTER 250)
(define DEFAULT-HEIGHT 100)
(define DEFAULT-WIDTH 150)
;; The MainController% class is a
;; (new MainController% [x Nat][y Nat][height Nat][width Nat])


(define MainController%
  (class* object% (Controller<%>)
    
    (init-field model) ;; It is the object of class Model
                       ;; It is unique through out the program.
    
    (init-field [x DEFAULT-X-CENTER] [y DEFAULT-Y-CENTER])
    ; x- y- coordinates of the controller

    (init-field [width DEFAULT-WIDTH][height DEFAULT-HEIGHT])
    ; width and height of the controller

    (field [half-width  (/ width  2)])  ; half of the width of controller
    (field [half-height (/ height 2)])  ; half of the height of controller
    
  
    (field [dot-x 0])  ; x position of the particle
    (field [dot-y 0])  ; y position of the particle

    (field [dot-vx 0]) ; x velocity of the particle
    (field [dot-vy 0]) ; y velocity of the particle

    (field [saved-mx 0]) ; distance between mouse and controller's x coordinate
    (field [saved-my 0]);distance between mouse and controller's y coordinate

    
    (field [handle-selected? false]) ; determines if the handle is selected.
    (field [handle-size 10]) ; determines the size of the handle.
    (field [handle-x-offset 0]) ; determines the x offsect of handle.
    (field [handle-y-offset 0]) ; determines the y offsect of handle.
    
    (super-new)
    
    (send model register this) ; register this controller with the model.

    ;; receive-signal : -> Void
    ;; EFFECT   : updates the x,y coordinates or x,y velocities depending upon
    ;;            the type of message or signal received from model.
    ;; STRATEGY : Divide on case on type of signal struct.
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (if (string=? "x" (report-position-which sig))
          (set! dot-x (report-position-pos sig))
          (set! dot-y (report-position-pos sig)))]
        [(report-velocity? sig)
         (if (string=? "x" (report-velocity-which sig))
             (set! dot-vx (report-velocity-v sig))
             (set! dot-vy (report-velocity-v sig)))]))

    ;; These are the abstract method. Child class will implement them
    ;; as required
    (abstract after-button-down)
    (abstract after-button-up)
    (abstract after-drag)
    (abstract add-to-scene) 
    (abstract after-key-event)
    (abstract after-move)
 
    
    ;; after-tick : -> Void
    ;; This Controller ignores key events
    (define/public (after-tick) this)

    ;; in-controller? : Nat Nat -> Boolean
    ;; GIVEN    : a location of a point on the canvas
    ;; RETURNS  : true iff the point is inside controller.
    ;; STRATEGY : combine simpler functions
    (define/public (in-controller? other-x other-y)
     
      (and
        (<= (- x half-width) other-x (+ x half-width))
        (<= (- y half-height) other-y (+ y half-height))))

    ;; in-handle? : Nat Nat -> Boolean
    ;; GIVEN    : a location of a point on the canvas
    ;; RETURNS  : true iff the point is inside controller.
    ;; STRATEGY: combine simpler functions
    (define/public (in-handle? other-x other-y)
     
      (and
       (<= (- x (+ half-width handle-x-offset))
           other-x
           (+ (- x (+ half-width handle-x-offset)) 10))
        (<= (- y (+ half-height handle-y-offset))
            other-y
            (+ (- y (+ half-height handle-y-offset)) 10))))
    
    
    ))


;;;;;;;;;;;;TESTING-FRAMEWORK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For testing cannot instantiate class with abstract methods,
;;so testing not possible


















