#lang racket
(require "interfaces.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide
 Politician%:
 make-politician
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define CANVAS-WIDTH-HALF (/ CANVAS-WIDTH 2))
(define CANVAS-HEIGHT-HALF (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define POLITICIAN-JUMP-VALUE 100)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; make-politician : Int Int -> Politician
;; GIVEN    : the x and y coordinate of the politician toy.
;; RETURNS  : a new object of class Politician%: at the given location.
;; EXAMPLES : see test cases
;; STRATEGY : combine simpler functions.
(define (make-politician x y)
  (new Politician%: [x x][y y]))


;; Constructor template for Politician%:
<<<<<<< HEAD
;; A Politician is a (new Politician%: [x Integer][y Integer]
;;     [mx Integer][my Integer] [face-number Integer])
=======
;; A Politician is a (new Politician%: [x Integer][y Integer])
>>>>>>> origin/master
;; Interpretation: An object of class Politician%: represents a politician.
(define Politician%:
  (class* object% (Toy<%>)
    
<<<<<<< HEAD
    (init-field x y)                            ; the politician's x and y position
    (init-field mx my)                          ; x and y position of the mouse pointer
    (init-field face-number)                    ; indicate which face the politician should depict
    
=======
    (init-field x y)                            ; the politician's x and
                                                ; y position
    (init-field [mx CANVAS-WIDTH-HALF])         ; x position of the
                                                ; mouse pointer
    (init-field [my CANVAS-HEIGHT-HALF])        ; y position of the
                                                ; mouse pointer
    (init-field [face-number 0])                ; indicate which face the 
                                                ; politician should depict
>>>>>>> origin/master
    (field [POLITICIAN1 (bitmap "hilary.png")]) ; potician1 denotes a picture 
                                                ; of hilary clinton.
    (field [POLITICIAN2 (bitmap "donald.png")]) ; potician2 denotes a picture 
                                                ; of donald trump.
<<<<<<< HEAD
    (field [SPEED 25])                          ; speed of the politician's movement.
    (field [FEAR-DISTANCE 75])                  ; the distance from which the politician retract
=======
    (field [SPEED 25])         ; speed of the politician's movement.
    (field [FEAR-DISTANCE 75]) ; the distance from which the politician retract
>>>>>>> origin/master
    
    (super-new)
    
    ;; after-tick : -> Void
    ;; GIVEN    : no arguments
    ;; EFFECT   : Updates the politician to the state,
    ;;            it should have following a tick.
    ;; STRATEGY : Divide into cases on is-frightened?.
    (define/public (after-tick)    
      (local
        ((define distance (get-distance-between-points x y mx my)))
        (if (is-frightened? distance)
            (begin
              (set! x (get-x-jump-value distance POLITICIAN-JUMP-VALUE))
              (set! y (get-y-jump-value distance POLITICIAN-JUMP-VALUE))
              (set! face-number (toggle-face-number)))
            
            (begin 
              (set! x (get-new-x-step distance SPEED))
              (set! y (get-new-y-step distance SPEED))))))
    
    
    ;; get-x-jump-value: PosInt PosInt -> Integer
    ;; GIVEN    : Distance between mouse and politician's coordinates
    ;;            and jump distance
    ;; RETURN   : New x- coordinate of the politician when politician is within
    ;;            distance of 75 of the mouse pointer.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Use cases on x and mx
    (define (get-x-jump-value distance jump)
      (cond
        [(< x mx) (- x (* (/ (- mx x) distance) jump))]
        [(> x mx) (+ x (* (/ (- x mx) distance) jump))]
        [else x]))
    
    
    ;; get-y-jump-value: PosInt PosInt -> Integer
    ;; GIVEN    : Distance between mouse and politician's coordinates
    ;;            and jump distance
    ;; RETURN   : New y - coordinate of the politician when politician is 
    ;;            within distance of 75 of the mouse pointer.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Use cases on y and my
    (define (get-y-jump-value distance jump)
      (cond
        [(< y my) (- y (* (/ (- my y) distance) jump))]
        [(> y my) (+ y (* (/ (- y my) distance) jump))]
        [else y]))
    
    
    ;; get-new-x-step: PosInt PosInt -> Integer
    ;; GIVEN    : Distance between mouse and politician's coordinates
    ;;            and jump distance
    ;; RETURN   : New x- coordinate of the politician when mouse pointer
    ;;            is not nearby.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Use cases on x and mx
    (define (get-new-x-step distance jump)
      (cond
        [(< x mx) (+ x (* (/ (- mx x) distance) jump))]
        [(> x mx) (- x (* (/ (- x mx) distance) jump))]
        [else x]))
    
    
    ;; get-new-y-step: Integer Integer -> Integer
    ;; GIVEN    : Distance between mouse and politician's coordinates
    ;;            and jump distance
    ;; RETURN   : New y - coordinate of the politician when mouse pointer
    ;;            is not nearby.
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Use cases on y and my
    (define (get-new-y-step distance jump)
      (cond
        [(< y my) (+ y (* (/ (- my y) distance) jump))]
        [(> y my) (- y (* (/ (- y my) distance) jump))]
        [else y]))
    
    
    ;; toggle-face-number : -> Integer
    ;; RETURN   : change the face of the politician
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Divide on cases on face-number.
    (define (toggle-face-number)
      (if (= face-number 0) 1 0))
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN    : a scene
    ;; RETURNS  : a scene like the given one, but with politician painted on it
    ;;            The politician's face-number decides the which face is to be
    ;;            painted.
    ;; STRATEGY : Divide on cases on face-number.
    (define/public (add-to-scene scene)
      (if (= face-number 0)
          (place-image POLITICIAN1 x y scene)
          (place-image POLITICIAN2 x y scene)))   
    
  
   ;; after-button-down : NonNegInt NonNegInt -> Politician
    ;; GIVEN    : the location of a button-down event
    ;; RETURNS  : a politician like the given one, but after the mouse
    ;;            button is pressed
    ;; EXAMPLE  : See test cases
    ;; DETAILS : Poltician ignores after-button-down event
    (define/public (after-button-down mousex mousey)
      this)
    
    
    ;; after-drag : NonNegInt NonNegInt -> void
    ;; GIVEN   : the location of the mouse pointer.
    ;; RETURNS : a politician like the given one, but after a drag mouse event
    ;; EXAMPLE : See test cases
    ;; DETAILS : Poltician ignores after-drag event
    (define/public (after-drag mousex mousey)
      this)
    
    
    ;; after-button-up : NonNegInt NonNegInt -> void
    ;; GIVEN   : the location of the mouse pointer.
    ;; RETURNS : a politician like the given one, but after a button-up event
    ;; EXAMPLE : See test cases
    ;; DETAILS : Poltician ignores after-button-up event
    (define/public (after-button-up mousex mousey)
      this)
    
    
    ;; is-frightened? : Integer -> Boolean
    ;; GIVEN    : distance between mouse pointer and potician.
    ;; RETURNS  : true iff the distance between politician and mouse pointer
    ;;            is more than FEAR-DISTANCE.
    ;; EXAMPLES : see tests below
    ;; STRATEGY : Combine simpler functions.
    (define (is-frightened? distance)
      (<= distance FEAR-DISTANCE))
    
    
    ;; get-distance-between-points : Integer Integer Integer Integer
    ;; GIVEN    : x and y coordinates of politician and mouse pointer resp.
    ;; RETURN   : distance between the position of poilitician and the mouse
    ;; EXAMPLES : see test cases below.
    ;; STRATEGY : Combine simpler functions
    (define (get-distance-between-points x y x1 y1)
      (sqrt(+ (sqr (- x x1))  (sqr (- y y1)))))
    
    
   
    ;; after-key-event : KeyEvent -> Politician
    ;; GIVEN   : A keyevent
    ;; RETURNS : A Politician like this one, but as it should be after the
    ;;           given key event.
    ;; DETAILS : a politician ignores key events
    ;; EXAMPLE : See test cases
    ;; after-key-event : KeyEvent -> void
    ;; DETAILS : a politician ignores key events
    (define/public (after-key-event kev) this)
    
    
    ;; after-move : NonNegInt NonNegInt -> void
    ;; GIVEN    : the location of a mouse move event
    ;; EFFECT   : If politician is frightened, toggle the politician
    ;;            otherwise keep moving the politician.
    ;; STRATEGY : Divide into cases on is-frightened?.
    (define/public (after-move  mousex mousey)
      (local
        ((define distance (get-distance-between-points x y mousex mousey)))
        (if (is-frightened? distance)
            
            (begin
              (set! x (get-x-jump-value distance POLITICIAN-JUMP-VALUE))
              (set! y (get-y-jump-value distance POLITICIAN-JUMP-VALUE))
              (set! mx mousex)
              (set! my mousey)
              (set! face-number (toggle-face-number)))
            
            (begin 
              (set! mx mousex)
              (set! my mousey)))))
    

    ;; -> Int
    ;; RETURNS : the x or y position of the center of the politician
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    ;; toy-data : -> Int
    ;; RETURNS: the distance between politician and mouse
    (define/public (toy-data)
      (sqrt(+ (sqr (- x mx))  (sqr (- y my)))))

    ;; For Test cases
    (define/public (for-test:explain-state)
      (list x y mx my face-number))
           
    
    ))

;;;;;;;;;;;;TESTING FRAMEWORK - POLITICIAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-politician1 : Int Int  -> Politician
;; GIVEN    : the x and y coordinate of the thobber and a time
;;            value indicating the creation time of that widget.
;; RETURNS  : a new object of class Politician%: at the given location
;             with second politician
;; EXAMPLES : see test cases
;; STRATEGY : combine simpler functions.
(define (make-politician1 x y)
  (new Politician%: [x x][y y][mx CANVAS-WIDTH-HALF][my CANVAS-HEIGHT-HALF]
       [face-number 1]))

(begin-for-test
  (local
    ((define INITIAL-POLITICIAN (make-politician 100 100))
     (define POLITICIAN3 (new Politician%: [x CANVAS-WIDTH-HALF]
                              [y CANVAS-HEIGHT-HALF]
                              [mx CANVAS-WIDTH-HALF]
                              [my CANVAS-HEIGHT-HALF]
                              [face-number 1] ))
     (define IMAGE-P (send INITIAL-POLITICIAN add-to-scene EMPTY-CANVAS))
     (define POLITICIAN2 (make-politician1 250 300))    
     (define IMAGE-P2 (send POLITICIAN2 add-to-scene EMPTY-CANVAS))
     (define POLITICIAN4 (new Politician%: [x 300]
                              [y 350]
                              [mx CANVAS-WIDTH-HALF]
                              [my CANVAS-HEIGHT-HALF]
                              [face-number 1] ))
     (define POLITICIAN5 (new Politician%: [x 400]
                              [y 450]
                              [mx CANVAS-WIDTH-HALF]
                              [my CANVAS-HEIGHT-HALF]
                              [face-number 1] ))
     (define POLITICIAN6 (make-politician1 250 30))
     (define POLITICIAN7 (make-politician1 20 300))

     )
    
    (check-equal? (send INITIAL-POLITICIAN toy-x)
	                  100
<<<<<<< HEAD
			                    "returns x-coordinate of the politician")
=======
		"returns x-coordinate of the politician")
>>>>>>> origin/master

    (check-equal? (send INITIAL-POLITICIAN toy-y)
                  100
                  "returns y-coordinate of the politician")

    (check-equal? (send INITIAL-POLITICIAN toy-data)
                  250
                  "returns distance between politician and mouse")

     (send INITIAL-POLITICIAN after-tick)
    (check-equal? (send INITIAL-POLITICIAN toy-data)
                  225
<<<<<<< HEAD
                  "returns the distance between initial politician and mouse after tick")
=======
                  "returns the distance between initial politician
                   and mouse after tick")
>>>>>>> origin/master

    (send POLITICIAN2 after-tick)
    (check-equal? (send POLITICIAN2 for-test:explain-state)
                  (list 250 300 250 300 0)
                  "returns the parameters of POLITICIAN2 after tick")

    (send POLITICIAN3 after-tick)
    (check-equal? (send POLITICIAN3 for-test:explain-state)
                  (list 250 300 250 300 0)
                  "returns the parameters of POLITICIAN3 after tick")

    (send POLITICIAN4 after-tick)
    (check-equal? (send POLITICIAN4 for-test:explain-state)
                  (list 370.71067811865476 420.71067811865476 250 300 0)
                  "returns the parameters of POLITICIAN4 after tick")

    (send POLITICIAN5 after-tick)
    (check-equal? (send POLITICIAN5 for-test:explain-state)
                  (list 382.32233047033634 432.32233047033634 250 300 1)
                  "returns the parameters of POLITICIAN5 after tick")
    (send POLITICIAN6 after-tick)
    (check-equal? (send POLITICIAN6 for-test:explain-state)
                  (list 250 55 250 300 1)
                  "returns the parameters of POLITICIAN6 after tick")
    
    (send POLITICIAN7 after-tick)
    (check-equal? (send POLITICIAN7 for-test:explain-state)
                  (list 45 300 250 300 1)
                  "returns the parameters of POLITICIAN7 after tick")


   (send INITIAL-POLITICIAN after-key-event "p")
    (check-equal? (send INITIAL-POLITICIAN for-test:explain-state)
                  (list 115 120 250 300 0)
<<<<<<< HEAD
                  "returns the parameters of INITIAL-POLITICIAN after a key event")
=======
                  "returns the parameters of INITIAL-POLITICIAN after a
                   key event")
>>>>>>> origin/master

    (send INITIAL-POLITICIAN after-button-down 100 100)
    (check-equal? (send INITIAL-POLITICIAN for-test:explain-state)
                  (list 115 120 250 300 0)
                  "returns the parameters of INITIAL-POLITICIAN after
                   button-down mouse event")

    (send INITIAL-POLITICIAN after-drag 100 100)
    (check-equal? (send INITIAL-POLITICIAN for-test:explain-state)
                  (list 115 120 250 300 0)
                  "returns the parameters of INITIAL-POLITICIAN after
                   drag mouse event")

    (send INITIAL-POLITICIAN after-button-up 100 100)
    (check-equal? (send INITIAL-POLITICIAN for-test:explain-state)
                  (list 115 120 250 300 0)
                  "returns the parameters of INITIAL-POLITICIAN after
                   button-up mouse event")
 
    (send INITIAL-POLITICIAN after-move 100 100)
    (check-equal? (send INITIAL-POLITICIAN for-test:explain-state)
                  (list -425 -600 100 100 1)
                  "returns the parameters of INITIAL-POLITICIAN after
                   move mouse event")

    (send INITIAL-POLITICIAN after-move 20 30)
    (check-equal? (send INITIAL-POLITICIAN for-test:explain-state)
                  (list -425 -600 20 30 1)
                  "returns the parameters of INITIAL-POLITICIAN after
                   move mouse event")
    
                  
    ))

