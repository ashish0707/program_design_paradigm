#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "HandleController.rkt")
(require rackunit)
(require "extras.rkt")
(require "Model.rkt")


(provide XY-Controller%)

(define OUTER-CIRCLE (circle 10 "solid" "red"))
(define INNER-CIRCLE (circle 2 "solid" "black"))

;; The XY-Controller% class is a (new XY-Controller%)
;; INTERPRETATION:
;; A XY-Controller represents a XY-Controller% with below listed properties 

(define XY-Controller%
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
                  (within-bounds? 1 (- mx saved-mx) 149) "x")) 
           (send model execute-command
                 (make-set-position
                  (within-bounds? 0 (- my saved-my) 99) "y")))]
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


    ;; add-to-scene : Scene -> Scene
    ;; RETURNS  : a scene as given but with this wall painted
    ;;            on it.
    ;; STRATEGY : combine simpler funtions.
    (define/override (add-to-scene scene)
      (place-image (boundry-image) x y scene))

    ;; get-color : -> String
    ;; GIVEN    : no parameters
    ;; RETURNS  : a string value color depending on
    ;;            whether the handle is selected.
    ;; STRATEGY : Divide into Cases on handle-selected? 
    (define (get-color)
      (if handle-selected? "red" "black"))
    
    ;; assemble the image of the viewer
    
    ;; boundry-image : -> Scene
    ;; GIVEN    : no arguments
    ;; RETURNS  : scene with the y-controller image painted on it
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

    
    ;; ball-with-particle-image : -> Scene
    ;; GIVEN    : no arguments
    ;; RETURNS  : scene with the data image painted on it
    ;; STRATEGY : combine simpler functions
    (define (ball-with-particle-image)
      (place-image (overlay INNER-CIRCLE
                            OUTER-CIRCLE)
                   dot-x dot-y 
                   (rectangle width height "outline" "blue")))
    
    ;; RETURN : the state of controller in a list
    (define/public (for-test:state)
      (list x y handle-selected? paused?))


    ))


  
(begin-for-test
  (local
    (
     (define m1 (new Model%))
     (define m2 (new Model%))
     (define m3 (new Model%))
     (define m4 (new Model%))
     (define m5 (new Model% [paused? true]))
     (define m6 (new Model%))
      (define m7 (new Model%))

      (define xy-c (new XY-Controller% [model m7]))
     (define xy-cont1 (new XY-Controller% [model m2]))
     (define xy-cont2 (new XY-Controller% [model m3]))
     (define xy-cont3 (new XY-Controller% [model m4]))
     (define xy-cont4 (new XY-Controller% [model m5]))
     (define xy-cont5 (new XY-Controller% [model m6]))

     )
      (send m1 after-tick)
      (send xy-cont5 add-to-scene  (empty-scene 600 250))
    
      (send xy-cont1 after-drag 200 210) 
      (check-equal? (send xy-cont1 for-test:state)
                    (list 300 250 #f #f)
                    "should return a controller at x=300 and y=250")


    
      (send xy-cont4 after-button-down 200 175)
      (check-equal? (send xy-cont4 for-test:state)
                    (list 300 250 #t #f)
                    "should return the controller with handle selected")
    
    (send xy-cont4 after-drag 201 276)
    (check-equal? (send xy-cont4 for-test:state)
                  (list 301 351 #t #f)
                  "should return a controller at x=296 and y=346")
    
      (send xy-cont2 after-drag 160 26)
      (check-equal? (send xy-cont2 for-test:state)
                    (list 300 250 #f #f)
                    "should return a controller at x=300 and y=300")
   

      (send xy-cont4 add-to-scene (empty-scene 600 250))
    
      (send xy-cont3 after-button-down 200 175)
      (check-equal? (send xy-cont3 for-test:state)
                    (list 300 250 #t #f)
                    "should return a controller paused")

     (send xy-c after-button-down 300 250)
      (check-equal? (local
                      ()
                      (send xy-c after-drag 210 200)
                      (send xy-c for-test:state)
                      )
                    (list 300 250 #f #t)
                    "should return a controller paused")
    
      (send xy-cont3 after-drag 100 100)
      (check-equal? (send xy-cont3 for-test:state)
                    (list 200 175 #t #f)
                    "should return a controller paused")
    
      (send xy-cont4 after-drag 100 100)
      (check-equal? (send xy-cont4 for-test:state)
                    (list 200 175 #t #f)
                    "shoudl return a controller at x=195 and y=170")

    ))
  

  
