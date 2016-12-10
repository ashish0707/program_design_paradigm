#lang racket

(require "interfaces.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide
Clock%:
make-clock
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

;; CANVAS
(define THROBBER-CHANGING-FACTOR 1)
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define CANVAS-WIDTH-HALF (/ CANVAS-WIDTH 2))
(define CANVAS-HEIGHT-HALF (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define INTIAL-TIME 0)
(define CLOCK-TEXT-SIZE 12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; make-clock : Int Int NonNegInt-> Widget
;; GIVEN    : the x and y coordinate of the clock toy.
;; RETURNS  : a new object of class Clock%: at the given location
;; EXAMPLES : (make-clock 100 100) -> CLOCK-AT-100-100
;; STRATEGY : combine simpler functions.
(define (make-clock x y)
  (new Clock%: [x x][y y][t INTIAL-TIME]
       [clock-mouse-x-diff -1][clock-mouse-y-diff -1]
       [selected? false]))


;; Constructor template for Clock%:
;; (new Clock%: [x Integer][y Integer]
;;     [t Integer][clock-mouse-x-diff Integer] [clock-mouse-y-diff Integer]
;;     [selected? Boolean])
;; Interpretation : An object of class Clock%: represents a clock displaying
;; time.
(define Clock%:
  (class* object% (Toy<%>)
    (init-field x y)               ; the clock's x and y position
    (init-field t)                 ; the time of the creation of the clock.
    (init-field clock-mouse-x-diff); difference between clocks's x and mouse
                                   ; pointer's x coordinate.
    (init-field clock-mouse-y-diff); difference between clock's y and mouse
                                   ; pointer's y coordinate.
    (init-field selected?) ; is the clock selected? Default is false
    (field [CLOCK-RADIUS 20]) ; indicates the radius of the clock.
    ;; image for displaying the clock
    (field [CLOCK-IMG (circle CLOCK-RADIUS "solid" "red")])
    ;; image for displaying the clock's text
    (field [CLOCK-IMG-TEXT (text (format "~a" t) CLOCK-TEXT-SIZE "black")])
     
    
    (super-new)
    
    
    ;; after-tick : -> Clock%:
    ;; RETURNS  : A Clock like this one, but as it should
    ;;            be after a tick.
    ;; EXAMPLE  : CLOCK-AT-100-100 -> (new Clock%: [x 100][y 100][t 1]
    ;;                                [clock-mouse-x-diff -1]
    ;;                                [clock-mouse-y-diff -1]
    ;;                                [selected? false])
    ;; STRATEGY : Cases on selected?
    (define/public (after-tick)
      (new Clock%: [x x][y y][t (+ t 1)]
           [clock-mouse-x-diff clock-mouse-x-diff]
           [clock-mouse-y-diff clock-mouse-y-diff]
           [selected? selected?]))
    
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS  : a scene like the given one, but with
    ;;            this clock painted on it.
    ;; STRATEGY : Combine simpler functions  
    (define/public (add-to-scene scene)
      (place-image CLOCK-IMG-TEXT x y (place-image CLOCK-IMG x y scene)))   
     
    
    ;; after-button-down : NonNegInt NonNegInt -> Clock%:
    ;; GIVEN    : the location of a button-down event
    ;; RETURNS  : a clock like the given one, but after the mouse
    ;;            button is pressed
    ;; EXAMPLE  : CLOCK-AT-100-100 -> (new Clock%: [x 100][y 100][t 1]
    ;;                                [clock-mouse-x-diff 80]
    ;;                                [clock-mouse-y-diff 80]
    ;;                                [selected? true]) where
    ;;                                mx, my is 80,80 resp.
    ;; STRATEGY : Cases on whether the event is inside the clock
    (define/public (after-button-down mx my)
      (if (in-clock? x y mx my)
          (new Clock%: [x x][y y][t t]
               [clock-mouse-x-diff (- x mx)]
               [clock-mouse-y-diff (- y my)]
               [selected? true])
          this))
    
    
    ;; after-drag : NonNegInt NonNegInt -> Clock%:
    ;; GIVEN    : the location of a drag event
    ;; RETURNS  : a clock toy like the given one, but after a drag mouse event
    ;; EXAMPLE  : CLOCK-AT-100-100 -> (new Clock%: [x 100][y 100][t 1]
    ;;                                [clock-mouse-x-diff 81]
    ;;                                [clock-mouse-y-diff 81]
    ;;                                [selected? true]) where
    ;;                                mx, my is 81,81 resp.
    ;; STRATEGY : Cases on whether the toy is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%: [x (+ mx clock-mouse-x-diff)]
               [y (+ my clock-mouse-y-diff)]
               [t t]
               [clock-mouse-x-diff clock-mouse-x-diff]
               [clock-mouse-y-diff clock-mouse-y-diff]
               [selected? true])
          this))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> Clock%:
    ;; GIVEN : the location of a button-down event
    ;; RETURNS : a clock toy like the given one, but after the mouse
    ;; button is depressed
    ;; EXAMPLE  : CLOCK-AT-100-100 -> (new Clock%: [x 100][y 100][t 1]
    ;;                                [clock-mouse-x-diff 180]
    ;;                                [clock-mouse-y-diff 180]
    ;;                                [selected? false]) where
    ;;                                mx, my is 180,180 resp.
    ;; STRATEGY: Cases on whether the event is inside the clock
    (define/public (after-button-up mx my)
      (new Clock%: [x x]
           [y y][t t]
           [clock-mouse-x-diff clock-mouse-x-diff]
           [clock-mouse-y-diff clock-mouse-y-diff]
           [selected? false]))
    
    
    ;; in-clock? : NonNegInt NonNegInt NonNegInt NonNegInt -> Boolean
    ;; GIVEN: a location on the canvas and mouse position
    ;; RETURNS: true iff the location is inside this clock.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY: Combine simpler functions
    (define (in-clock? x y mx my)
      (<= (sqrt(+ (sqr (- x mx))  (sqr (- y my))))
          CLOCK-RADIUS))
    
    
    ;; after-key-event : KeyEvent -> Clock%:
    ;; RETURNS : A Clock like this one, but as it should be after the
    ;;           given key event.
    ;; EXAMPLE : CLOCK-AT-100-100 -> CLOCK-AT-100-100
    ;; DETAILS : a clock ignores key events
    (define/public (after-key-event kev) this)
    
    
    ;; test methods, to test; the bomb state.
    (define/public (for-test:x) x)
    (define/public (for-test:y) y)
    (define/public (for-test:selected?) selected?)
    
    
    ;; after-button-up : NonNegInt NonNegInt -> Clock%:
    ;; GIVEN : the location of a mouse move event
    ;; RETURNS : a clock toy like the given one, but after the mouse
    ;; button is moved
    ;; EXAMPLE : CLOCK-AT-100-100 -> CLOCK-AT-100-100
    (define/public (after-move  mx my) this)
    
    
    ;; -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-x)
      (exact-round x))
    
    
    ;; -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-y)
      (exact-round y))
    
    
    ;; -> Int
    ;; RETURNS: the time of the clock
    (define/public (toy-data) t)
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING FRAMEWORK - CLOCK ;;;;;;;;;;;;;;;;;;;;;

(define CLOCK-AT-100-100 (make-clock 100 100))

; clock-equal?: Clock Clock -> Boolean
(define (clock-equal? c1 c2)
  (= (send c1 toy-data)
     (send c2 toy-data)))

(define (clock-selected-equal? c1 c2)
  (equal? (send c1 for-test:selected?)
          (send c2 for-test:selected?)))

(define (clock-xy-equal? c1 c2)
  (and (= (send c1 for-test:x)
          (send c2 for-test:x))
       (= (send c1 for-test:y)
          (send c2 for-test:y))))

(begin-for-test
  (local
    ((define c0 (make-clock 200 150))
     (define c1 (send c0 after-tick))
     (define c9 (send c0 after-key-event "c"))
     (define c2 (send c1 after-button-down 200 150))
     (define c10 (send c2 after-tick))
     (define c6 (send c1 after-button-down 400 150))
     (define c3 (send c2 after-button-up 200 150))
     (define c7 (send c2 after-button-up 400 150))
     (define c4 (send c2 after-drag 100 150))
     (define c8 (send c1 after-drag 100 150))
     (define c5 (send c3 add-to-scene EMPTY-CANVAS))
     (define c11 (send c2 after-move 100 150)))
    
    (check-equal? (send c0 toy-x)
                  200
                  "incorrect value of x is generated")
    
    (check-equal? (send c0 toy-y)
                  150
                  "incorrect value of y is generated")
    
    (check-equal? (send c0 toy-data)
                  0
                  "incorrect value of time is generated")
    
    (check clock-equal? c10 c10
           "two clocks have different field values")
    
    (check clock-selected-equal? c2
           (new Clock%: [x 200][y 150][t 0][clock-mouse-x-diff -1]
                [clock-mouse-y-diff -1]
                [selected? true])
           "two clocks have different selected field values")
    
    (check clock-selected-equal? c3 
           (new Clock%: [x 200][y 150][t 0][clock-mouse-x-diff -1]
                [clock-mouse-y-diff -1]
                [selected? false])
           "two clocks have different selected field values")
    
    (check clock-selected-equal? c6 c6
           "two clocks have different selected field values")
    
    (check clock-selected-equal? c7 c7
           "two clocks have different field values")
    
    (check clock-selected-equal? c8 c8
           "two clocks have different selected field values")
    
    (check clock-xy-equal? c4 
           (new Clock%: [x 100][y 150][t 0][clock-mouse-x-diff -1]
                [clock-mouse-y-diff -1]
                [selected? true])
           "two clocks have different selected x,y field values")
    
    (check clock-xy-equal? c9 c9
           "two clocks have different selected x,y field values")
    ))

