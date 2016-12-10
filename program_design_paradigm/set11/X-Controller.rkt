#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "HandleController.rkt")
(require rackunit)
(require "extras.rkt")
(require "Model-test.rkt")

(provide X-Controller%)


(define OUTER-CIRCLE (circle 10 "solid" "red"))
(define INNER-CIRCLE (circle 2 "solid" "black"))

;; The X-Controller% class is a (new X-Controller%)
;; INTERPRETATION:
;; A XController represents a X-Controller% with below listed properties.

(define X-Controller%
  (class* HandleController% (Controller<%>)

  
    (inherit-field    
     model       ;; represents model
     x           ;; x position of the center of the HandleController%
     y           ;; y position of the center of the HandleController%
     saved-mx    ;; the difference between mouse pointer and
     ;; y-controller's  x-coordinates
     saved-my    ;; the difference between mouse pointer and
     ;; y-controller's  y-coordinates
     
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
     paused?)    ;; true iff simulation is halted
   
    (super-new)
    
    (send model register this)
    (set! handle-y-offset 0)
    (set! height 50)
    (println height)
   
    (set! half-height (/ height 2))
    (println half-height)
    

    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: the location of mouse after a drag event
    ;; EFFECT: updates the model if the simulation is paused, else
    ;; updates saved-mx and saved-my if handle is selected for
    ;; dragging the viewer
    ;; STRATEGY: Cases on whether simulation is paused? or handle-selected?.
    (define/override (after-drag mx my)
      (cond
        [paused?
         (begin
           (send model execute-command
                 (make-set-position
                  (within-bounds? 1 (- mx saved-mx) 149) "x")))]
        [handle-selected?
         (begin
           (set! x (- mx saved-mx))
           (set! y (- my saved-my)))]
        [else 1]))

    ;; within-bounds? : PosInt PosInt PosInt -> PosInt
    ;; GIVEN          : low and hign value along with actual value.
    ;; RETURNS        : max value between low and min value between
    ;;                  actual and high value.
    ;; STRATEGY       : combine simpler functions 
    (define (within-bounds? lo val hi)
      (max lo (min val hi)))

  
    ;; get-color : -> String
    ;; GIVEN    : no parameters
    ;; RETURNS  : a string value color depending on
    ;;            whether the handle is selected.
    ;; STRATEGY : Divide into Cases on handle-selected?      
    (define (get-color)
      (if handle-selected? "red" "black"))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    (define/override (add-to-scene scene)
      (place-image (boundry-image) x y scene)) 

    
   
    ;; boundry-image : -> Scene
    ;; GIVEN    : no arguments
    ;; RETURNS  : scene with the ball and particle image painted on it.
    ;; STRATEGY : combine simpler functions
    (define (boundry-image)
      (let ((the-ball-image (ball-with-particle-image)))
        (overlay
         the-ball-image
         (rectangle width height "outline" "blue")
         (overlay/xy (square handle-size "outline" (get-color)) 0 0
                     (rectangle (+ width (* 2 handle-x-offset))
                                (+ height (* 2 handle-y-offset))
                                "outline" "black")))))
    
    ;; -> Scene
    ;; GIVEN    : no arguments
    ;; RETURNS  : scene with the data image painted on it.
    ;; STRATEGY : combine simpler functions
    (define (ball-with-particle-image)
      (place-image (overlay INNER-CIRCLE
                            OUTER-CIRCLE)
                   dot-x half-height
                   (rectangle width height "outline" "blue")))

;; For Test cases
    (define/public (for-test:state)
      (list x y handle-selected? paused?))

    ))


;;;;;;;;;;;;;;TESTING-FRAMEWORK;;;;;;;;;;;;;;;;;;;;;


(begin-for-test
  (local
    (
     (define m1 (new Model%))
     (define m2 (new Model%))
     (define m3 (new Model%))
     (define m4 (new Model%))
     (define m5 (new Model%))
     (define m6 (new Model%))
     (define m7 (new Model%))

     (define x-cont1 (new X-Controller% [model m2]))
     (define x-cont2 (new X-Controller% [model m3]))
     (define x-cont3 (new X-Controller% [model m4]))
     (define x-cont4 (new X-Controller% [model m5]))
     (define x-cont5 (new X-Controller% [model m6]))
     (define xc5 (new X-Controller% [model m7]))

     )
      (send m1 after-tick)
      (send x-cont5 add-to-scene  (empty-scene 600 250))
      (send x-cont1 after-drag 200 210)
      
  
      (check-equal? (send x-cont1 for-test:state)
                    (list 300 250 #f #f)
                    "returns a controller with x at 300 and y at 300")
    
      (send x-cont2 after-drag 160 26)
      (check-equal? (send x-cont2 for-test:state)
                    (list 300 250 #f #f)
                    "returns a controller with x at 300 and y at 300")
    
     
      (send x-cont4 after-button-down 201 225)
      (check-equal? (send x-cont4 for-test:state)
                    (list 300 250 #t #f)
                    "returns a controller with handle selected")

      (send x-cont4 add-to-scene (empty-scene 600 250))
    
      (send x-cont3 after-button-down 200 225)
      (check-equal? (send x-cont3 for-test:state)
                    (list 300 250 #t #f)
                    "returns a controller paused")
    
      (send x-cont3 after-drag 100 100)
      (check-equal? (send x-cont3 for-test:state)
                    (list 200 125 #t #f)
                    "returns a controller paused and x and y at 200 and 125")
    
      (send x-cont4 after-drag 100 100)
      (check-equal? (send x-cont4 for-test:state)
                    (list 199 125 #t #f)
                    "returns a controller with x at 300 and y at 250")

    (send xc5 after-button-down 300 250)
      (check-equal? (local
                      ()
                      (send xc5 after-drag 340 290)
                      (send xc5 for-test:state))
                    (list 300 250 #f #t)
                    "returns a controller should not be dragged")

    ))
  

