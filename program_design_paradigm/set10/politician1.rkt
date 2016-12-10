#lang racket
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
(define INTIAL-POLI-MOUSE-Y 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; make-politician : Int Int -> Toy
;; GIVEN    : the x and y coordinate of the politician toy.
;; RETURNS  : a new object of class Politician%: at the given location.
;; EXAMPLES : see test cases
;; STRATEGY : combine simpler functions.
(define (make-politician x y)
  (new Politician%: [x x][y y][mx CANVAS-WIDTH-HALF][my CANVAS-HEIGHT-HALF]
       [face-number 0]))


;; Constructor template for Politician%:
;; (new Politician%: [x Integer][y Integer]
;;     [mx Integer][my Integer] [face-number Integer])
;; Interpretation: An object of class Politician%: represents a politician.
(define Politician%:
  (class* object% (Toy<%>)
    
    (init-field x y) ;the politician's x and y position
    
    (init-field mx my)  ;x and y position of the mouse pointer
    
    (init-field face-number) ;indicate which face the politician should depict
    
    (field [POLITICIAN1 (bitmap "hilary.png")]) ; potician1 denotes a picture 
                                                ; of hilary clinton.
    (field [POLITICIAN2 (bitmap "donald.png")]) ; potician2 denotes a picture 
                                                ; of donald trump.
    (field [SPEED 25])  ; speed of the politician's movement.
    (field [FEAR-DISTANCE 75]) ; the distance from which the politician retract
    
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
    ;; STRATEGY : Use cases on y and my.
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
    
    
    ;; after-button-down : NonNegInt NonNegInt -> void
    ;; GIVEN   : the location of a button-down event
    ;; DETAILS : Poltician ignores after-button-down event
    (define/public (after-button-down mousex mousey)
      this)
    
    
    ;; after-drag : NonNegInt NonNegInt -> void
    ;; GIVEN   : the location of the mouse pointer.
    ;; DETAILS : Poltician ignores after-drag event
    (define/public (after-drag mousex mousey)
      this)
    
    
    ;; after-button-up : NonNegInt NonNegInt -> void
    ;; GIVEN   : the location of the mouse pointer.
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
    
    ;; -> Int
    ;; RETURNS: the distance between politician and mouse
    (define/public (toy-data)
      (sqrt(+ (sqr (- x mx))  (sqr (- y my)))))

    ;; For Test cases
    (define/public (for-test:explain-state)
      (list x y mx my face-number))
           
    
    ))
