#lang racket

(require "interfaces.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide Clock%:
         make-clock)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(define INTIAL-TIME 0)
(define CLOCK-TEXT-SIZE 12)
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; make-clock : Int Int -> Clock
;; GIVEN    : the x and y coordinate of the clock toy.
;; RETURNS  : a new object of class Clock%: at the given location
;; EXAMPLES : (make-clock 100 100) -> CLOCK-AT-100-100
;; STRATEGY : combine simpler functions.
(define (make-clock x y)
  (new Clock%: [x x][y y]))


;; Constructor template for Clock%:
<<<<<<< HEAD
;; A Clock is a (new Clock%: [x Integer][y Integer]
;;     [t Integer][clock-mouse-x-diff Integer] [clock-mouse-y-diff Integer]
;;     [selected? Boolean])
=======
;; A Clock is a (new Clock%: [x Integer][y Integer])
>>>>>>> origin/master
;; Interpretation : An object of class Clock%: represents a clock displaying
;; time.
(define Clock%:
  (class* object% (Toy<%>)
    
    (init-field x y)                ; the clock's x and y position    
<<<<<<< HEAD
    (init-field t)                  ; the time of the creation of the clock.    
    (init-field clock-mouse-x-diff) ; difference between clocks's x and mouse
                                    ; pointer's x coordinate.
    (init-field clock-mouse-y-diff) ; difference between clock's y and mouse
                                    ; pointer's y coordinate.
    (init-field selected?)          ; is the clock selected? Default is false
=======
    (init-field [t INTIAL-TIME])    ; the time of the creation of the clock.    
    (init-field [clock-mouse-x-diff -1]) ; difference between clocks's x and 
                                         ; mouse pointer's x coordinate.
    (init-field [clock-mouse-y-diff -1]) ; difference between clock's y and 
                                         ; mouse pointer's y coordinate.
    (init-field [selected? false])  ; is the clock selected? Default is false
>>>>>>> origin/master
    (field [CLOCK-RADIUS 20])       ; indicates the radius of the clock.
    
    ;; image for displaying the clock
    (field [CLOCK-IMG (circle CLOCK-RADIUS "solid" "red")])
    ;; image for displaying the clock's text
    (field [CLOCK-IMG-TEXT (text (format "~a" t) CLOCK-TEXT-SIZE "black")])
     
    
    (super-new)
    
    
    ;; after-tick : -> Void
    ;; GIVEN    : No arguments
    ;; EFFECT   : Updates the clock to the state,
    ;;            it should have following a tick.
    ;; EXAMPLE  : (send INITIAL-CLOCK toy-data) ==> 1
    ;; STRATEGY : Combine simpler function
    (define/public (after-tick)
      (set! t (+ t 1)))
     
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN    : A scene
    ;; RETURNS  : a scene like the given one, but with
    ;;            this clock painted on it.
    ;; STRATEGY : Combine simpler functions  
    (define/public (add-to-scene scene)
      (place-image
       (text (format "~a" t) CLOCK-TEXT-SIZE "black") x y
       (place-image CLOCK-IMG x y scene))) 
     
    
    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; GIVEN    : the location of a button-down event
    ;; EFFECT   : If the mouse pointer is inside clock,
    ;;            make the clock selected.
    ;; EXAMPLE  : See test cases
    ;; STRATEGY : Cases on whether the event is inside the clock
    (define/public (after-button-down mx my)
      (if (in-clock? x y mx my)
          (begin
            (set! clock-mouse-x-diff (- x mx))
            (set! clock-mouse-y-diff (- y my))
            (set! selected? true))
          this))
      
    
    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN    : the location of a drag event
<<<<<<< HEAD
    ;; EFFECT   : If clock is selected, move it so that the vector from its position
    ;;            to the drag event is equal to the mx
    ;; EXAMPLE  : See test cases
=======
    ;; EFFECT   : If clock is selected, move it so that the vector from 
    ;;            its position to the drag event is equal to the mx
>>>>>>> origin/master
    ;; STRATEGY : Cases on whether the toy is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ mx clock-mouse-x-diff))
            (set! y (+ my clock-mouse-y-diff))
            (set! selected? true))
          this))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN    : the location of a button-down event
    ;; EFFECT   : Makes the clock unselected
    ;; EXAMPLE  :  See test cases
    ;; STRATEGY : Combine simpler function
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    
    ;; in-clock? : NonNegInt NonNegInt NonNegInt NonNegInt -> Boolean
    ;; GIVEN: a location on the canvas and mouse position
    ;; RETURNS: true iff the location is inside this clock.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY: Combine simpler functions
    (define (in-clock? x y mx my)
      (<= (sqrt(+ (sqr (- x mx))  (sqr (- y my))))
          CLOCK-RADIUS))
    
    
    ;; after-key-event : KeyEvent -> Clock
    ;; GIVEN   : A keyevent
    ;; RETURNS : A Clock like this one, but as it should be after the
    ;;           given key event.
    ;; EXAMPLE : See test cases
    ;; DETAILS : a clock ignores key events
    (define/public (after-key-event kev) this)
    

    
    
    ;; after-button-up : NonNegInt NonNegInt -> Clock
    ;; GIVEN   : the location of a mouse move event
    ;; RETURNS : a clock toy like the given one, but after the mouse
    ;;           button is moved
    ;; EXAMPLE : See test cases
    (define/public (after-move  mx my) this)
    
    
    ;; toy-x : -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-x)
      (exact-round x))
    
    
    ;; toy-y : -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-y)
      (exact-round y))
    
     
    ;; toy-data : -> Int
    ;; RETURNS: the time of the clock
    (define/public (toy-data) t)

    ;; For Test cases
    (define/public (for-test:explain-state)
      (list x y clock-mouse-x-diff clock-mouse-y-diff
            selected?))
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING FRAMEWORK - CLOCK ;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (local
    ((define INITIAL-CLOCK (make-clock 100 100))
     (define UNSELECTED-CLOCK (new Clock%: [x 100][y 100][t 0]
                                   [clock-mouse-x-diff -1]
                                   [clock-mouse-y-diff -1]
                                   [selected? false]))
     (define SELECTED-CLOCK (new Clock%: [x 100][y 100][t 0]
                                 [clock-mouse-x-diff -1]
                                 [clock-mouse-y-diff -1]
                                 [selected? true]))
     
     )
    
    (check-equal? (send INITIAL-CLOCK toy-x)
                  100 "returns the x-coordinate of clock")

    (check-equal? (send INITIAL-CLOCK toy-y)
                  100 "returns y-coordinate of the clock")

    (send INITIAL-CLOCK after-tick)
    (check-equal? (send INITIAL-CLOCK toy-data)
                  1
                  "returns the time on clock after first tick")

    (check-equal? (send INITIAL-CLOCK add-to-scene EMPTY-CANVAS)
                  (place-image (text "1" 12 "black")
                               100 100
                               (place-image (circle 20 "solid" "red")
                                            100 100
                                            EMPTY-CANVAS))
                  "returns a scene with clock drawn on the EMPTY-CANVAS")


    (send INITIAL-CLOCK after-key-event "p")
    (check-equal? (send SELECTED-CLOCK for-test:explain-state)
                  (list 100 100 -1 -1 #true)
                  "returns clock as it is after any key event")

    (send INITIAL-CLOCK after-button-down 100 100)
    (check-equal? (send INITIAL-CLOCK for-test:explain-state)
                  (list 100 100 0 0 #true)
<<<<<<< HEAD
                  "returns clock after mouse event is button-down and it is outside the clock")
=======
                  "returns clock after mouse event is button-down and
                   it is outside the clock")
>>>>>>> origin/master

    (send INITIAL-CLOCK after-button-down 20 30)
    (check-equal? (send INITIAL-CLOCK for-test:explain-state)
                  (list 100 100 0 0 #true)
<<<<<<< HEAD
                  "returns clock after mouse event when mouse event is button-down inside the clock")
=======
                  "returns clock after mouse event when mouse event is
                   button-down inside the clock")
>>>>>>> origin/master

    (send SELECTED-CLOCK after-button-up 20 30)
    (check-equal? (send SELECTED-CLOCK for-test:explain-state)
                  (list 100 100 -1 -1 #f)
<<<<<<< HEAD
                  "returns clock after mouse event when mouse event is button-up inside the clock")
=======
                  "returns clock after mouse event when mouse event is
                   button-up inside the clock")
>>>>>>> origin/master

    (send INITIAL-CLOCK after-drag 20 30)
    (check-equal? (send INITIAL-CLOCK for-test:explain-state)
                  (list 20 30 0 0 #t)
                  "returns clock after mouse event when mouse event is drag
                   and clock is not selected")

    (send SELECTED-CLOCK after-drag 20 30)
    (check-equal? (send SELECTED-CLOCK for-test:explain-state)
                  (list 100 100 -1 -1 #f)
                  "returns clock after mouse event when mouse event is drag
                   and clock is selected")

    (send SELECTED-CLOCK after-move 20 30)
    (check-equal? (send SELECTED-CLOCK for-test:explain-state)
                  (list 100 100 -1 -1 #f)
                  "returns clock after mouse event when mouse event is
                   other than button-up, button-down or drag")

    
    ))

