#lang racket
(require "interfaces.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide
 Throbber%:
 make-throbber
 Toy<%>
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

;; CANVAS
(define THROBBER-CHANGING-FACTOR 1)
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THROBBER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-throbber: PosInt PosInt -> Throbber
;; GIVEN    : an x and a y position of a throbber
;; RETURNS  : an object representing a throbber at the given position.
;; EXAMPLES : see test cases
;; STRATEGY : combine simpler functions.
(define (make-throbber x y)
<<<<<<< HEAD
  (new Throbber%: [x x][y y]
       ))
=======
  (new Throbber%: [x x][y y]))
>>>>>>> origin/master
      


;; Constructor template for Throbber%:
;; A Throbber is a (new Throbber%: [x Integer][y Integer]
;;  [throb-mouse-x-diff Integer][throb-mouse-y-diff Integer] [r Integer]
;;  [selected? Boolean][expanding? Boolean])
;; Interpretation : An object of class Throbber%: represents
;;                  a thobber.

(define Throbber%:
  (class* object% (Toy<%>)
    
    (init-field x y)                     ; x and y coordinate of Throbber    
<<<<<<< HEAD
    (init-field [throb-mouse-x-diff -1]) ; difference between circle's x and mouse
                                         ; pointer's x coordinate.
    (init-field [throb-mouse-y-diff -1]) ; difference between circle's y and mouse
                                         ; pointer's y coordinate.    
=======
    (init-field [throb-mouse-x-diff -1]) ; difference between circle's x and
                                         ; mouse pointer's x coordinate.
    (init-field [throb-mouse-y-diff -1]) ; difference between circle's y and 
                                         ; mouse pointer's y coordinate.    
>>>>>>> origin/master
    (init-field [r 5])                   ; the radius of the throbber.
    (init-field [selected? false])       ; the states whether the thobber is
                                         ; selected.
    (init-field [expanding? true])       ; the states whether the thobber is
                                         ; expanding.

    ; target throbber parameters
    (field [MIN-THROBBER-RADIUS 5])
    (field [MAX-THROBBER-RADIUS 20])
    
    ;; image for displaying the thobber when unselected
    (field [THOBBER-IMG-UNSELECTED (circle r "solid" "green")])
    
    ;; image for displaying the thobber when selected
    (field [THOBBER-IMG-SELECTED (circle r "outline" "green")])
    
     
    (super-new)
    
    ;; after-tick : -> Void
    ;; GIVEN    : No arguments
    ;; EFFECT   : Updates the throbber to the state,
    ;;            it should have following a tick.
<<<<<<< HEAD
    ;; EXAMPLE  : See test cases below
=======
>>>>>>> origin/master
    ;; STRATEGY : Combine simpler function
    (define/public (after-tick)
      (begin
        (set! r (next-radius r))
<<<<<<< HEAD
        (set! expanding? (should-expand?))
        ))
=======
        (set! expanding? (should-expand?))))
>>>>>>> origin/master
    

    ;; next-radius: PosInt -> PosInt
    ;; GIVEN    : the radius of the thobber.
    ;; RETURNS  : Throbber's radius increased or decresed by
    ;;            THROBBER-CHANGING-FACTOR depending
    ;;            on whether its expanding.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Divide on cases whether throbber is expanding or contracting.
    (define (next-radius r) 
      (if expanding?
          (+ r THROBBER-CHANGING-FACTOR)
          (- r THROBBER-CHANGING-FACTOR)))
    
    
    ;; should-expand? :-> Boolean
    ;; RETURN   : true if radius of throbber becomes less than 5.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Divide into case on radius - r.
    (define (should-expand?)
      (cond
        [(>= r MAX-THROBBER-RADIUS) false]
        [(<= r MIN-THROBBER-RADIUS) true]
        [else expanding?]))
    
    
    ;; to-scene : Scene -> Scene
    ;; GIVEN    : a scene to paint the throbber.
    ;; RETURNS  : a scene like the given one, but with
    ;;            this thobber painted on it.
    ;; STRATEGY : Cases on whether the throbber is selected.
    (define/public (add-to-scene scene)
      (if selected?
          (place-image (circle r "outline" "green")  x y scene)
          (place-image (circle r "solid" "green") x y scene)))
    
    
    ;; after-button-down : NonNegInt NonNegInt -> void
    ;; GIVEN    : the location of a button-down event
    ;; EFFECT   : If the mouse pointer is inside throbber,
    ;;            make the throbber selected.
<<<<<<< HEAD
    ;; EXAMPLE  : See test cases
=======
>>>>>>> origin/master
    ;; STRATEGY : Cases on whether the mouse is within the throbber.
    (define/public (after-button-down mx my)
      (if (inside-throbber? x y mx my r)
          (begin
            (set! throb-mouse-x-diff (- x mx)) 
            (set! throb-mouse-y-diff (- y my))
            (set! selected? true))
          this))
      
     
    
    ;; after-drag : NonNegInt NonNegInt -> void
    ;; GIVEN    : the x and y coordinates of the mouse pointer.
<<<<<<< HEAD
    ;; EFFECT   : If throbber is selected, move it so that the vector from its position
    ;;            to the drag event is equal to the mx
=======
    ;; EFFECT   : If throbber is selected, move it so that the vector from 
    ;;            its position to the drag event is equal to the mx
>>>>>>> origin/master
    ;; EXAMPLE  : See test cases
    ;; STRATEGY : Divide into Cases on whether the throbber is selected.
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ mx throb-mouse-x-diff))
            (set! y (+ my throb-mouse-y-diff)))
          this))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> void
    ;; GIVEN    : the x and y coordinates of mouse pointer.
    ;; EFFECT   : makes the throbber unselected
    ;; EXAMPLE  : See test cases
    ;; STRATEGY : combine simpler functions
    (define/public (after-button-up mx my)
      (begin
        (set! selected? false)))
        
    
    ;; inside-throbber? : Integer Integer Integer Integer Integer -> Boolean
    ;; GIVEN    : x and y coordinates of two points respectively.
    ;; RETURNS  : true iff the given coordinate is inside the given throbber.
    ;; EXAMPLES : see tests below
    ;; STRATEGY : Combine simpler function
    (define (inside-throbber? x y mx my r)
      (<= (sqrt(+ (sqr (- x mx))  (sqr (- y my))))
          r))
    
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN    : A Key event
    ;; DETAILS: a Throbber ignores key events
    (define/public (after-key-event kev) this)
    

    ;; after-drag : NonNegInt NonNegInt -> Throbber<%>
    ;; GIVEN    : the location of a move event
    ;; DETAILS: a Throbber ignores after-move
    (define/public (after-move  mx my) this)
    
    
    ;; toy-x :-> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-x)
      (exact-round x))
    
    ;; toy-y : -> Int
    ;; RETURNS: the y position of the center of the toy
    (define/public (toy-y)
      (exact-round y))
    
    ;; toy-data : -> Int
    ;; RETURNS: the radius of the thobber
    (define/public (toy-data) r)

    ;; For Test cases
    (define/public (for-test:explain-state)
      (list x y throb-mouse-x-diff throb-mouse-y-diff
            r selected? expanding?))

    ;; For Test cases
    (define/public (throbbers-after-tick n)
      (cond
        ((= n 0) empty)
        (else
         (cons
          (toy-data)
          (begin
            (after-tick)
            (throbbers-after-tick (- n 1)))))))

    ;; For Test cases 
    (define/public (throbbers-radius-less-than-max? n)
      (andmap (lambda (x) (<= x 20)) (throbbers-after-tick n)))
    


    
    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING FRAMEWORK - Throbber ;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (local
    ((define INITIAL-THROBBER (make-throbber 100 100))
     (define EXPANDING-THROBBER (new Throbber%: [x 100][y 100]
                                     [throb-mouse-x-diff -1]
                                     [throb-mouse-y-diff -1]
                                     [r 19][selected? false][expanding? false]))
     (define SHRINKING-THROBBER (new Throbber%: [x 300] [y 150]
                                     [throb-mouse-x-diff -1]
                                     [throb-mouse-y-diff -1]
                                     [r 7] [selected? false]
                                     [expanding? false]))
     (define BIGGER-THROBBER (new Throbber%: [x 300] [y 150]
                                  [throb-mouse-x-diff -1]
                                  [throb-mouse-y-diff -1]
                                  [r 21] [selected? false]
                                  [expanding? false]))
     (define SMALLER-THROBBER (new Throbber%: [x 300] [y 150]
                                   [throb-mouse-x-diff -1]
                                   [throb-mouse-y-diff -1]
                                   [r 4] [selected? false]
                                   [expanding? true]))
     (define THROBBER1 (make-throbber 100 100))

     )

    (send THROBBER1 throbbers-after-tick 21)
    (check-equal? (send THROBBER1 throbbers-radius-less-than-max? 21) true)
    
    (check-equal? (send INITIAL-THROBBER toy-x)
                  100
                  "returns x-coordinate of throbber")
<<<<<<< HEAD

    (check-equal? (send INITIAL-THROBBER toy-y)
                  100
                  "returns y-coordinate of throbber")

=======

    (check-equal? (send INITIAL-THROBBER toy-y)
                  100
                  "returns y-coordinate of throbber")

>>>>>>> origin/master
    ;; calling after-tick
    (send INITIAL-THROBBER after-tick)
    (check-equal? (send INITIAL-THROBBER toy-data)
                  6
                  "returns radius of throbber")

    (check-equal? (send INITIAL-THROBBER add-to-scene EMPTY-CANVAS)
                  (place-image (circle 6 "solid" "green")
                               100 100 EMPTY-CANVAS)
                  "returns a throbber added to the EMPTY-CANVAS")

    (send EXPANDING-THROBBER after-tick)
    (check-equal? (send EXPANDING-THROBBER toy-data)
                  18
                  "returns radius of the throbber after one tick")

    (send BIGGER-THROBBER after-tick)
    (check-equal? (send BIGGER-THROBBER toy-data)
                  20
<<<<<<< HEAD
                  "returns radius of the throbber after tick when radius of throbber
                   is more than 20")
=======
                  "returns radius of the throbber after tick when radius
                    of throbber is more than 20")
>>>>>>> origin/master

    (send SMALLER-THROBBER after-tick)
    (check-equal? (send SMALLER-THROBBER toy-data)
                  5
<<<<<<< HEAD
                  "returns radius of the throbber after tick when radius is less than 5")

=======
                  "returns radius of the throbber after tick when radius is 
                  less than 5")
    
>>>>>>> origin/master
    (send INITIAL-THROBBER after-key-event "p")
    (check-equal? (send EXPANDING-THROBBER for-test:explain-state)
                  (list 100 100 -1 -1 18 #f #f)
                  "retruns throbber unchanged after any key event")

    (send INITIAL-THROBBER after-button-down 100 100)
    (check-equal? (send INITIAL-THROBBER for-test:explain-state)
                  (list 100 100 0 0 6 #t #t)
                  "returns throbber after mouse event button-down")

    (send EXPANDING-THROBBER after-button-down 0 0)
    (check-equal? (send EXPANDING-THROBBER for-test:explain-state)
                  (list 100 100 -1 -1 18 #f #f)
<<<<<<< HEAD
                  "returns throbber after mouse event button-down for an expanding throbber")
=======
                  "returns throbber after mouse event button-down for
                   an expanding throbber")
>>>>>>> origin/master

    (send EXPANDING-THROBBER after-button-up 20 30)
    (check-equal? (send EXPANDING-THROBBER for-test:explain-state)
                  (list 100 100 -1 -1 18 #f #f)
                  "returns throbber after mouse event button-up")

    (send EXPANDING-THROBBER after-move 20 30)
    (check-equal? (send EXPANDING-THROBBER for-test:explain-state)
                  (list 100 100 -1 -1 18 #f #f)
                  "returns throbber after mouse event after move")

    (send SHRINKING-THROBBER after-tick)
    (check-equal? (send EXPANDING-THROBBER for-test:explain-state)
                  (list 100 100 -1 -1 18 #f #f)
                  "returns radius of the throbber after tick")

    (send INITIAL-THROBBER after-drag 20 30)
    (check-equal? (send INITIAL-THROBBER for-test:explain-state)
                  (list 20 30 0 0 6 #t #t)
<<<<<<< HEAD
                  "returns throbber after drag event of mouse for initial-throbber")
=======
                  "returns throbber after drag event of mouse for
                   initial-throbber")
>>>>>>> origin/master

    (send EXPANDING-THROBBER after-drag 20 30)
    (check-equal? (send INITIAL-THROBBER for-test:explain-state)
                  (list 20 30 0 0 6 #t #t)
<<<<<<< HEAD
                  "returns throbber after drag event of mouse for expanding throbber")
=======
                  "returns throbber after drag event of mouse for
                   expanding throbber")
>>>>>>> origin/master

    (check-equal? (send INITIAL-THROBBER add-to-scene EMPTY-CANVAS)
                  (place-image (circle 6 "outline" "green")
                               20 30 EMPTY-CANVAS)
                  "adds a throbber to the EMPTY-CANVAS")

    
    ))
