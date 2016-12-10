#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "MainController.rkt")
(require "Model.rkt")

(provide PartcileParamController%)

(define DISPLAY-SUFFIX-X "X = ")
(define DISPLAY-SUFFIX-Y " Y = ")
(define DISPLAY-SUFFIX-VX "VX = ")
(define DISPLAY-SUFFIX-VY " VY = ")
(define TEXT-SIZE 12)

;; The PartcileParamController class is a (new PartcileParamController%)
;; INTERPRETATION:
;; A ParameterController% represents a ParameterController 
;; with listed properties. It does all the processing related to handle
;; like actions on button down, drag, etc. It is also responsible to print
;; x,v,vx, vy on the scene given along with specific display text from the
;; child class.

(define PartcileParamController%
  (class* MainController% (Controller<%>)

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
     )   

    
    (field [controller-selected? false]) ;check if the controller is selected.
        
    (super-new)

    (send model register this) ; register this cass with the model.

    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN    : the point coordinates of mouse on button-down.
    ;; EFFECT   : makes the handle selected and the controller selected.
    ;; STRATEGY : Cases on whether the event is inside this controller.
    (define/override (after-button-down mx my)
      (cond
        [(in-handle? mx my)
         (begin
          (set! handle-selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))]
        [(in-controller? mx my)
        (begin
          (set! controller-selected? true))]
        [else 1]))

    ;; in-handle? : Nat Nat -> Boolean
    ;; GIVEN    : a location of a point on the canvas
    ;; RETURNS  : true iff the point is inside handle.
    ;; STRATEGY : combine simpler functions
    ;; DETAILS  : Method is called in super class.
    (define/override (in-handle? mx my)
      (super in-handle? mx my))

    ;; in-controller? : Nat Nat -> Boolean 
    ;; GIVEN    : a location of a point on the canvas
    ;; RETURNS  : true iff the point is inside controller.
    ;; STRATEGY : combine simpler functions
    ;; DETAILS  : Method is called in super class.
    (define/override (in-controller? mx my)
      (super in-controller? mx my))
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN  : the location of a point on button-up event.
    ;; EFFECT : unselects contoller and handle.
    (define/override (after-button-up mx my)
      (set! controller-selected? false)
      (set! handle-selected? false))
    
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN    : the location of a point on drag event
    ;; EFFECT   : updates the x and y coordinates of controller for drag event.
    ;; STRATEGY : combine simpler functions
    (define/override (after-drag mx my)
      (if handle-selected?
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        1))
   
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS  : a similar scene, but with this 
    ;;            this controller painted on it.
    ;; STRATEGY : combine simpler functions.
    (define/override (add-to-scene scene)
      (place-image (get-view) x y scene))

    ;; get-color -> String
    ;; GIVEN    : no parameters
    ;; RETURNS  : a string value depending upon state of handle.
    ;; STRATEGY : Divide into cases on handle-selected?
    (define (get-color)
      (if handle-selected? "red" "black"))

    ;; get-text-color : -> String
    ;; GIVEN    : no parameters
    ;; RETURNS  : a string value depending upon state of controller.
    ;; STRATEGY : Divide into cases on controller-selected?  
    (define (get-text-color)
      (if controller-selected? "red" "black"))

    
    ;; get-view : -> Scene
    ;; GIVEN    : no arguments
    ;; RETURNS  : scene with the particle param view painted on it.
    ;; STRATEGY : combine simpler functions
    (define (get-view)
      (let ((inner-image (inner-text-image)))
        (overlay/xy (square handle-size "outline" (get-color)) 0 0
                    (overlay 
          inner-image
          (rectangle
            (max width (+ (image-width inner-image) 10))
            (max height (+ (image-height inner-image) 10))
            "outline"
            "black")))))

    ;; inner-text-image : -> Scene
    ;; GIVEN    : no arguments
    ;; RETURNS  : scene with the x, y, vx, vy of particle painted on it
    ;; STRATEGY : combine simpler functions
    (define (inner-text-image)
      (above
       (text (get-display-text) 10 (get-text-color))
        (text (string-append
                DISPLAY-SUFFIX-X
                (number->string
                 (/ (round (* 100 (exact->inexact dot-x))) 100))
                DISPLAY-SUFFIX-Y
                (number->string
                 (/ (round (* 100 (exact->inexact dot-y))) 100)))
              TEXT-SIZE (get-text-color))
        (text (string-append
               DISPLAY-SUFFIX-VX
                (number->string (round (exact->inexact dot-vx)))
                DISPLAY-SUFFIX-VY
                (number->string (round (exact->inexact dot-vx))))
              TEXT-SIZE (get-text-color))))

    
    ;; to be supplied by the subclass
    (abstract get-display-text)

    ;; Controller ignores this method.
    (define/override (after-move mx my) this)

   ))



;;;;;;;;;;;;;TESTING-FRAMEWORK;;;;;;;;;;;;;;
;; cannot instantiate class with abstract methods hence testing not possible.