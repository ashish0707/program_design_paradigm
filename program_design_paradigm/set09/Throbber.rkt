#lang racket
(require "interfaces.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide
Throbber%:
make-throbber
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THROBBER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-throbber: PosInt PosInt -> Toy
;; GIVEN    : an x and a y position of a throbber
;; RETURNS  : an object representing a throbber at the given position.
;; EXAMPLES : see test cases
;; STRATEGY : combine simpler functions.
(define (make-throbber x y)
  (new Throbber%: [x x][y y]
       [throb-mouse-x-diff -1]
       [throb-mouse-y-diff -1]
       [r 5][selected? false][expanding? true]))


;; Constructor template for Throbber%:
;; A Throbber is a (new Throbber%: [x Integer][y Integer]
;;  [throb-mouse-x-diff Integer][throb-mouse-y-diff Integer] [r Integer]
;;  [selected? Boolean][expanding? Boolean])
;; Interpretation : An object of class Throbber%: represents
;;                  a thobber.
(define Throbber%:
  (class* object% (Toy<%>)
    (init-field x y)                ; x and y coordinate of Throbber
    (init-field throb-mouse-x-diff) ; difference between circle's x and mouse
    ; pointer's x coordinate.
    (init-field throb-mouse-y-diff) ; difference between circle's y and mouse
    ; pointer's y coordinate.
    (init-field r)                  ; the radius of the throbber.
    (init-field selected?)          ; the states whether the thobber is
    ; selected.
    (init-field expanding?)         ; the states whether the thobber is
    ; expanding.
    
    ; target throbber parameters
    (field [MIN-THROBBER-RADIUS 5])
    (field [MAX-THROBBER-RADIUS 20])
    
    ;; image for displaying the thobber when unselected
    (field [THOBBER-IMG-UNSELECTED (circle r "solid" "green")])
    
    ;; image for displaying the thobber when selected
    (field [THOBBER-IMG-SELECTED (circle r "outline" "green")])
    
     
    (super-new)
    
    ;; after-tick : -> Throbber
    ;; RETURNS  : A throbber like this one, but as it should
    ;;            be after a tick
    ;; EXAMPLE  : THROBBER-AT-100-100 -> (new Throbber%: [x x][y y]
    ;;                                   [throb-mouse-x-diff -1]
    ;;                                   [throb-mouse-y-diff -1]
    ;;                                   [r 6][selected? false]
    ;;                                   [expanding? true])
    ;; STRATEGY : combine simpler functions
    (define/public (after-tick)
    (local
      ((define expand? (should-expand?)))
     (new Throbber%: [x x][y y]
           [throb-mouse-x-diff throb-mouse-x-diff]
           [throb-mouse-y-diff throb-mouse-y-diff]
           [r (next-radius r expand?)]
           [selected? selected?]
           [expanding? expand?])))
    
    ;; next-radius: PosInt Boolean-> PosInt
    ;; GIVEN    : the radius of the thobber.
    ;; RETURNS  : Throbber's radius increased or decresed by
    ;;            THROBBER-CHANGING-FACTOR depending
    ;;            on whether its expanding.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Divide on cases whether toy is expanding or contracting.
    (define (next-radius r expanding?) 
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
          (place-image THOBBER-IMG-SELECTED x y scene)
          (place-image THOBBER-IMG-UNSELECTED x y scene)))   
    
    
    ;; after-button-down : NonNegInt NonNegInt -> Throbber<%>
    ;; GIVEN    : the location of a button-down event
    ;; RETURNS  : a throbber like the given one, but after the mouse
    ;;            button is pressed.
    ;; EXAMPLE  : THROBBER-AT-100-100 -> (new Throbber%: [x x][y y]
    ;;                                   [throb-mouse-x-diff 100]
    ;;                                   [throb-mouse-y-diff 100]
    ;;                                   [r 6][selected? true]
    ;;                                   [expanding? true]) where
    ;;                                   mx, my is 100,100 resp.
    ;; STRATEGY : Cases on whether the mouse is within the throbber.
    (define/public (after-button-down mx my)
      (if (inside-throbber? x y mx my r)
          (new Throbber%: [x x][y y]
               [throb-mouse-x-diff (- x mx)]
               [throb-mouse-y-diff (- y my)]
               [r r]
               [selected? true]
               [expanding? expanding?])
          this))
    
    
    ;; after-drag : NonNegInt NonNegInt -> Throbber<%>
    ;; GIVEN    : the x and y coordinates of the mouse pointer.
    ;; RETURNS  : a throbber like the given one, dragging it based on the
    ;;            given mouse coordinates.
    ;; EXAMPLE  : THROBBER-AT-100-100 -> (new Throbber%: [x x][y y]
    ;;                                   [throb-mouse-x-diff 101]
    ;;                                   [throb-mouse-y-diff 101]
    ;;                                   [r 6][selected? true]
    ;;                                   [expanding? true]) where
    ;;                                   mx, my is 101,101 resp.
    ;; STRATEGY : Divide into Cases on whether the throbber is selected.
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%: [x (+ mx throb-mouse-x-diff)]
               [y (+ my throb-mouse-y-diff)]
               [throb-mouse-x-diff throb-mouse-x-diff]
               [throb-mouse-y-diff throb-mouse-y-diff]
               [r r]
               [selected? true]
               [expanding? expanding?])
          this))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> Throbber<%>
    ;; GIVEN    : the x and y coordinates of mouse pointer.
    ;; RETURNS  : a throbber like the given one, but making it unselected.
    ;; EXAMPLE  : THROBBER-AT-100-100 -> (new Throbber%: [x x][y y]
    ;;                                   [throb-mouse-x-diff 100]
    ;;                                   [throb-mouse-y-diff 100]
    ;;                                   [r 6][selected? false]
    ;;                                   [expanding? true]) where
    ;;                                   mx, my is 100,100 resp.
    ;; STRATEGY : combine simpler functions
    (define/public (after-button-up mx my)
      (new Throbber%: [x x]
           [y y]
           [throb-mouse-x-diff throb-mouse-x-diff]
           [throb-mouse-y-diff throb-mouse-y-diff]
           [r r]
           [selected? false]
           [expanding? expanding?]))
    
    
    ;; inside-throbber? : Integer Integer Integer Integer -> Boolean
    ;; GIVEN    : x and y coordinates of two points respectively.
    ;; RETURNS  : true iff the given coordinate is inside the given throbber.
    ;; EXAMPLES : see tests below
    ;; STRATEGY : Combine simpler function
    (define (inside-throbber? x y mx my r)
      (<= (sqrt(+ (sqr (- x mx))  (sqr (- y my))))
          r))
    
    
    ;; after-key-event : KeyEvent -> Throbber<%>
    ;; RETURNS: A Throbber like this one, but as it should be after the
    ;; given key event.
    ;; EXAMPLE  : THROBBER-AT-100-100 -> THROBBER-AT-100-100
    ;; DETAILS: a Throbber ignores key events
    (define/public (after-key-event kev) this)
    
    
    ;; test methods, to test; the throbber state.
    (define/public (for-test:x) x)
    (define/public (for-test:y) y)
    (define/public (for-test:selected?) selected?)
    
    
    ;; after-drag : NonNegInt NonNegInt -> Throbber<%>
    ;; GIVEN: the location of a move event
    ;; RETURNS : a throbber like the given one, but after a move mouse event
    ;; EXAMPLE  : THROBBER-AT-100-100 -> THROBBER-AT-100-100
    ;; DETAILS: a Throbber ignores after-move
    (define/public (after-move  mx my) this)
    
    
    ;; -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-x)
      (exact-round x))
    
    ;; -> Int
    ;; RETURNS: the y position of the center of the toy
    (define/public (toy-y)
      (exact-round y))
    
    ;; -> Int
    ;; RETURNS: the radius of the thobber
    (define/public (toy-data) r)
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING FRAMEWORK - Throbber ;;;;;;;;;;;;;;;;;;;;;

(define THROBBER-AT-100-100 (make-throbber 100 100))
;; throbber-equal? Throbber Throbber -> Boolean
(define (throbber-equal? t1 t2)
  (= (send t1 toy-data)
     (send t2 toy-data)))

(define (throbber-selected-equal? t1 t2)
  (equal? (send t1 for-test:selected?)
          (send t2 for-test:selected?)))

(define (throbber-xy-equal? t1 t2)
  (and (= (send t1 for-test:x)
          (send t2 for-test:x))
       (= (send t1 for-test:y)
          (send t2 for-test:y))))

(begin-for-test
  (local
    ((define t0 (make-throbber 300 150))
     (define t1 (send t0 after-tick))
     (define t13 (send t1 after-tick))
     (define t9 (send t0 after-key-event "t"))
     (define t2 (send t1 after-button-down 300 150))
     (define t10 (send t2 after-tick))
     (define t6 (send t1 after-button-down 400 150))
     (define t3 (send t2 after-button-up 300 150))
     (define t7 (send t2 after-button-up 400 150))
     (define t4 (send t2 after-drag 100 150))
     (define t8 (send t1 after-drag 100 150))
     (define t11
       (send
        (send
         (send
          (send
           (send
            (send
             (send
              (send
               (send
                (send
                 (send
                  (send
                   (send
                    (send
                     (send
                      (send
                       t0
                       after-tick)
                      after-tick)
                     after-tick)
                    after-tick)
                   after-tick)
                  after-tick)
                 after-tick)
                after-tick)
               after-tick)
              after-tick)
             after-tick)
            after-tick)
           after-tick)
          after-tick)
         after-tick)
        after-tick))
     
     (define t5 (send t3 add-to-scene EMPTY-CANVAS))
     (define t12 (send t2 add-to-scene EMPTY-CANVAS)))
    
    (define t14 (new Throbber%: [x 300][y 150]
                     [throb-mouse-x-diff -1]
                     [throb-mouse-y-diff -1]
                     [r 6][selected? false][expanding? false]))
    
    (define t15 (send t14 after-tick)
      )
    
    (check-equal? (send t0 toy-x)
                  300
                  "incorrect value of x is generated")
    
    (check-equal? (send t0 toy-y)
                  150
                  "incorrect value of y is generated")

    
    (check-equal? (send t0 toy-data)
                  5
                  "incorrect value of radius is generated")
    
    (check throbber-equal? t1 
           (new Throbber%: [x 300][y 150]
                [throb-mouse-x-diff -1]
                [throb-mouse-y-diff -1]
                [r 6][selected? false][expanding? true])
           "incorrect field values of thobber objects are generated")
    
    (check throbber-equal? t10 t10
           "incorrect field values of thobber objects are generated")
    
    (check throbber-equal? t10 (send t10 after-move 100 100)
           "incorrect field values of thobber objects are generated
            after sending after-move command")
                                
    (check throbber-selected-equal? t2 
           (new Throbber%: [x 300][y 150]
                [throb-mouse-x-diff -1]
                [throb-mouse-y-diff -1]
                [r 6][selected? true][expanding? true])
           "incorrect field value of selected of thobber objects is generated")
    
    (check throbber-selected-equal? t3 
           (new Throbber%: [x 300][y 150]
                [throb-mouse-x-diff -1]
                [throb-mouse-y-diff -1]
                [r 6][selected? false][expanding? true])
           "incorrect field value of selected of thobber objects is generated")
    
    (check throbber-selected-equal? t6 t6
           "incorrect field value of selected of thobber objects is generated")
    
    (check throbber-selected-equal? t7 t7
           "incorrect field value of selected of thobber objects is generated
           after sending after button up command")
    
    (check throbber-selected-equal? t8 t8
           "incorrect field value of selected of thobber objects is generated
            after sending after drag command")
    
    (check throbber-xy-equal? t4 
           (new Throbber%: [x 100][y 150]
                [throb-mouse-x-diff -1]
                [throb-mouse-y-diff -1]
                [r 6][selected? true][expanding? true])
           "incorrect field values of x and y of thobber objects are generated")
    
    (check throbber-xy-equal? t9 t9
           "incorrect field values of x and y of thobber objects are generated
            after new throbber event")
    
    (check-equal? t5 (place-image
                      (circle 6 "solid" "green") 300 150 EMPTY-CANVAS))
    "incorrect placement of images"))

	(define TOY-X 250)
	(define TOY-Y 300)

	(define (throbbers-after-tick n throbber)
	  (cond
	   ((= n 0) empty)
	   (else
            (begin
              (println (send throbber toy-data))
            (cons
	     (send throbber toy-data)
	     (throbbers-after-tick (- n 1) (send throbber after-tick))))
            )))

	(define (throbbers-radius-less-than-max? n throbber)
	  (andmap (lambda (x) (<= x 20)) (throbbers-after-tick n throbber)))

	(define THROBBER-MIN-RADIUS 5)
	(define THROBBER-MAX-RADIUS 20)
	(define THROBBER1 (make-throbber TOY-X TOY-Y))



(begin-for-test

(check-equal? (throbbers-radius-less-than-max? 21 THROBBER1) true))









