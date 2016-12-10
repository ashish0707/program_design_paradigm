#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "HandleController.rkt")
(require "PartcileParamController.rkt")
(require rackunit)
(require "extras.rkt")
(require "Model.rkt")


(provide Velocity-Controller%)
;; This class inherit from PartcileParamController. It sends command
;; to alter the velocity of the particle depending upon which arrow key is
;; pressed.
;; The VelocityController% class is a (new VelocityController%)
;; INTERPRETATION:
;; A VelocityController represents a VelocityController%

(define Velocity-Controller%
  (class* PartcileParamController% (Controller<%>)

    (inherit-field    
     model       ;; represents model
     x           ;; x position of the center of the PartcileParamController%
     y           ;; y position of the center of the PartcileParamController%
     saved-mx    ;; the difference between mouse pointer and
                 ;; controller's  x-coordinates
     saved-my    ;; the difference between mouse pointer and
                 ;; controller's  y-coordinates
     
     handle-selected? ;; Boolean -- is the handle selected?
     handle-x-offset  ;; offset for displaying handle
     handle-y-offset  ;; offset for displaying handle
     handle-size      ;; size of the handle
     
     dot-x       ;; x position of the particle
     dot-y       ;; y position of the particle
     dot-vx      ;; x velocity of the particle
     dot-vy      ;; y velocity of the particle
     width       ;; width of the controller
     height      ;; height of the controller  
     half-width  ;; half width of the controller
     half-height ;; half height of the controller
     controller-selected?) ;; states whether this controller is selected?

    (super-new)

    (send model register this) ;register this controller with the model.
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN    : a key event
    ;; EFFECT   : It updates the model by sending a command to change 
    ;;            the velocity of the particle.
    ;; STRATEGY : Divide into cases on kev based upon controller-selected?
    (define/override (after-key-event kev)
      (if controller-selected?
          (cond
            [(key=? "up" kev)
             (send model execute-command
                   (make-set-velocity (- dot-vy 5) "y"))]
            [(key=? "down" kev)
             (send model execute-command
                   (make-set-velocity (+ dot-vy 5) "y"))]
            [(key=? "right" kev)
             (send model execute-command
                   (make-set-velocity (+ dot-vx 5) "x"))]
            [(key=? "left" kev)
             (send model execute-command
                   (make-set-velocity (- dot-vx 5) "x"))])
          1))
 
    
    ;; get-display-text : -> String
    ;; GIVEN    : no parameter
    ;; RETURNS  : String representing display text
    ;; STRATEGY : combine simpler functions
    (define/override (get-display-text)
      "Arrow Keys Change Velocity")

    

    ))



;;****************************************************************

;; TESTS

(begin-for-test
  (local
    (

     (define m (new Model%))
     (define v1 (new Velocity-Controller% [model m]))
     (define v2 (new Velocity-Controller% [model m]))
     )

    (check-equal? (send m for-test:explain-state)
                  (list 75 50 0 0)
                  "incorrect x and y velocities... must be 0")

                  
    (send v2 after-button-down 300 300)
    (send v1 after-key-event "down")

    (check-equal? (send m for-test:explain-state)
                  (list 75 50 0 0)
                  "incorrect x and y velocities... must be 0")
  
    (send v2 after-key-event "down")
    (check-equal? (send m for-test:explain-state)
                  (list 75 50 0 5)
                  "incorrect y velocities... must be 0")
    
    (send v2 after-key-event "up")
    (check-equal? (send m for-test:explain-state)
                  (list 75 50 0 0)
                  "incorrect x and y velocities... must be 0")
    
    (send v2 after-key-event "right")
    (check-equal? (send m for-test:explain-state)
                  (list 75 50 5 0)
                  "incorrect x velocities... must be 5")
    
    (send v2 after-key-event "left")
    (check-equal? (send m for-test:explain-state)
                  (list 75 50 0 0)
                  "incorrect x and y velocities... must be 0")
    
    (check-equal? (send v2 get-display-text)
                  "Arrow Keys Change Velocity"
                  "incorrect text is displayed")
    ))
