#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "HandleController.rkt")
(require rackunit)
(require "extras.rkt")
(require "Model.rkt")


(provide Y-Controller%)

;;******************************* CONSTANTS *******************************

(define OUTER_RECT_COLOR "blue")
(define INNER_RECT_COLOR "black")
(define OUTER-CIRCLE (circle 10 "solid" "red"))
(define INNER-CIRCLE (circle 2 "solid" "black"))

;;*************************************************************************

(define Y-Controller%
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
    (set! handle-x-offset 0)
    (set! width 50)
    (set! half-width (/ width 2))
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN    : the coordinate of a point on drag event.
    ;; EFFECT   : Model is updated when simulation is paused, else
    ;;            saved-mx and saved-my is updated if handle is selected.
    ;; STRATEGY : Divide into Cases on whether simulation is paused or
    ;;            the handle is selected.
    (define/override (after-drag mx my)
      (cond
        [paused?
         (begin
           (send model execute-command
                 (make-set-position
                  (within-bounds? 1 (- my saved-my) 99) "y")))]
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

    
    ;; boundry-image : -> Scene
    ;; GIVEN    : no arguments
    ;; RETURNS  : scene with the y-controller image painted on it
    ;; STRATEGY : combine simpler functions
    (define (boundry-image)
      (let ((the-ball-image (ball-with-particle-image)))
      (overlay
       the-ball-image
       (rectangle width height "outline" OUTER_RECT_COLOR)
       (overlay/xy (square handle-size "outline" (get-color)) 0 0
       (rectangle (+ width (* 2 handle-x-offset))
                  (+ height (* 2 handle-y-offset))
                  "outline" INNER_RECT_COLOR)))))

    
    ;; ball-with-particle-image : -> Scene
    ;; GIVEN    : no arguments
    ;; RETURNS  : scene with the data image painted on it
    ;; STRATEGY : combine simpler functions
    (define (ball-with-particle-image)
         (place-image (overlay INNER-CIRCLE
                               OUTER-CIRCLE)
                      half-width dot-y 
                    (rectangle width height "outline" OUTER_RECT_COLOR)))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS  : a scene as given but with this wall painted
    ;;            on it. 
    ;; STRATEGY : combine simpler funtions.
    (define/override (add-to-scene scene)
      (place-image (boundry-image) x y scene))

    ;; for test 
    (define/public (for-test:state)
      (list x y handle-selected? paused?))

   ;; RETURNS : y position of the particle 
   (define/public (for-test:dot-y)
      dot-y)

    ;; RETURNS : half width of the controller
    (define/public (for-test:half-width)
      half-width)


     ;; RETURNS : half width of the controller
    (define/public (for-test:x)
      x)
    
     ;; RETURNS : half width of the controller
    (define/public (for-test:y)
      y)
    
     ;; RETURNS : half width of the controller
    (define/public (for-test:paused?)
      paused?)
    
     
    ;; RETURNS : x offset of the handle
    (define/public (for-test:handle-x-offset)
      handle-x-offset)
    
    ;; RETURNS : x offset of the handle
    (define/public (for-test:handle-y-offset)
      handle-y-offset)
    
    ;; RETURNS : size of the handle
    (define/public (for-test:handle-size)
      handle-size)
    
    ;; RETURNS : height of the controller
    (define/public (for-test:height)
      height)
    
    ;; RETURNS : height of the controller
    (define/public (for-test:width)
      width)

    ;; RETURNS : color of the controller
    (define/public (for-test:getcolor)
      (get-color))
    


    ))

;;;;;;;;;;;;;;;;TESTING-FRAMEWORK;;;;;;;;;;;;;;;;;;;

  
(begin-for-test
  (local
    (
     (define m1 (new Model%))
     (define m2 (new Model%))
     (define m3 (new Model%))
     (define m4 (new Model%))
     (define m5 (new Model% [paused? true]))
     (define m6 (new Model%))
     
     (define yc1 (new Y-Controller% [model m2]))
     (define yc2 (new Y-Controller% [model m3]))
     (define yc3 (new Y-Controller% [model m4]))
     (define yc4 (new Y-Controller% [model m5]))
     (define yc5 (new Y-Controller% [model m6]))
     (define yc6 (new Y-Controller% [model m1]))

     )
      (send m1 after-tick)
      (send yc5 add-to-scene  (empty-scene 600 250))
    
      (send yc1 after-drag 200 210) 
      (check-equal? (send yc1 for-test:state)
                    (list 300 250 #f #f)
                    "Incorrect value of x. It has to be 300")

    (check-equal?
      (local
        ()
        (send yc4 after-button-down 300 300)
        (send yc4 for-test:paused?))
       true 
      "amination should be paused")
    
     (check-equal?
      (local
        ()
        (send yc4 after-button-down 275 175)
        (send yc4 for-test:handle-selected?))
       true 
      "Incorrect value of handle-selected, must be true")

    (check-equal?
      (local
        ()
        (send yc4 after-button-down 275 175)
        (send yc4 for-test:getcolor))
       "red" 
      "Incorrect value of Color.It must be red")


     (check-equal?
      (local
        ()
        (send yc6 after-button-down 275 175)
        (println (send yc6 for-test:paused?))
        (println (send yc6 for-test:handle-selected?))
        (send yc6 after-drag 150 150)
        (send yc6 for-test:x))
      175
      "Incorrect value of x.")
    
     (check-equal?
      (local
        ()
        (send yc3 after-button-down 300 300)
        (send yc3 after-drag 100 100)
        (send yc3 for-test:x))
         300
         "x should not change")

    (check-equal?
      (local
        ()
        (send yc4 after-button-down 275 225)
        (send yc4 after-drag 100 100)
        (send yc4 for-test:x))
         300
         "x value must be 300")

   
    

    (check-equal?
    (send yc5 add-to-scene  (empty-scene 300 500))
    (place-image (overlay
                 (place-image
                  (overlay
                   INNER-CIRCLE OUTER-CIRCLE)
                  (send yc5 for-test:half-width)
                  (send yc5 for-test:dot-y )
                  (rectangle (send yc5 for-test:width)
                             (send yc5 for-test:height) "outline" "blue"))
                 (rectangle (send yc5 for-test:width)
                            (send yc5 for-test:height) "outline" "blue")
                 (overlay/xy (square (send yc5 for-test:handle-size)
                                     "outline" "black") 0 0
                             (rectangle (+ (send yc5 for-test:width)
                                           (* 2
                                              (send yc5
                                                    for-test:handle-x-offset)))
                                        (+ (send yc5 for-test:height)
                                           (* 2
                                              (send yc5
                                                    for-test:handle-y-offset)))
                                        "outline" "black")))
                (send yc5 for-test:x)
                (send yc5 for-test:y)
                (empty-scene 300 500))
   "image placed incorrect. Please have a visual check and correct it.")
    
    
      (send yc2 after-drag 160 26)
      (check-equal? (send yc2 for-test:state)
                    (list 300 250 #f #f)
                    "x must be 300")
   

      
      
    
     

    ))
  

  