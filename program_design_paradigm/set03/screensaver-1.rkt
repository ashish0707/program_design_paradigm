;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; start with (screensaver 0)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-circ1
         world-circ2
         world-paused?
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver speed )
  (big-bang (initial-world speed)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGTH 300)
(define circ1-x 200)

(define CIRC1-X-COORD 200)
(define CIRC1-Y-COORD 200)

(define CIRC2-X-COORD 200)
(define CIRC2-Y-COORD 100)

(define circ1-y 100)
(define circ2-x 200)
(define circ2-y 200)
(define CIRC1-XVEL -12)
(define CIRC1-YVEL 20)
(define CIRC2-XVEL 23)
(define CIRC2-YVEL -14)

(define circ1-vx -12)
(define circ1-vy 20)
(define circ2-vx 23)
(define circ2-vy -14)
(define SIMLPE_CIRCLE (circle 40 "outline" "blue"))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGTH))
(define TEXT_HEIGHT 10)
(define CIRCLE_RADIUS 40)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DATA DEFINITIONS

(define-struct world (circ1 circ2 paused?))
;; A World is a (make-world Mycircle Mycircle Boolean) ASK-about circ
;; circ1 and circ2 are the two circles bouncing within the world
;; paused? describes whether or not the world is paused

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;  (... (world-circ1 w)
;;    (world-circ2 w)
;;    (world-paused? w)))


(define-struct mycircle (x y vx vy))
;; A Mycircle is a (make-mycircle NonNegInt NonNegInt Int Int)
;; Interpretation: 
;; x, y give the position of the circle. 
;; vx, vy give the velocity of the circle. 
;; selected? describes whether or not the cat is selected.

;; template:
;; mycircle-fn : Mycircle -> ??
;; (define (circ-fn c)
;; (... (circ-x-pos w)
;;   (circ-y-pos w)
;;   (circ-vx w)
;;   (circ-vy w)))


;;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS FOR CIRCLE

;; circ-x : Mycircle -> NonNegInt
;; GIVEN: a Mycircle struct
;; RETURNS: the X-coordinate of Mycircle
;; STRATEGY : use template for Mycircle on c
(define (circ-x c)
  (mycircle-x c))

;; circ-y : Mycircle -> NonNegInt
;; GIVEN: a Mycircle struct
;; RETURNS: the Y-coordinate of Mycircle
;; STRATEGY : use template for Mycircle on c
(define (circ-y c)
  (mycircle-y c))

;; circ-vx : Mycircle -> NonNegInt
;; GIVEN: a Mycircle struct
;; RETURNS: the X velocity of Mycircle
;; STRATEGY : use template for Mycircle on c
(define (circ-vx c)
  (mycircle-vx c))

;; circ- : Mycircle -> NonNegInt
;; GIVEN: a Mycircle struct
;; RETURNS: the Y velocity of Mycircle
;; STRATEGY : use template for Mycircle on c
(define (circ-vy c)
  (mycircle-vy c))

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; STRATEGY : combine simpler functions
(define (new-circle x y vx vy)
  (make-mycircle x y vx vy)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples of worlds, for testing

(define circle-at-200-30 (make-mycircle 200 30 CIRC2-XVEL CIRC2-YVEL)) 
(define circle-at-200-261 (make-mycircle 200 261 CIRC2-XVEL CIRC2-YVEL))
(define circle-at-361-100 (make-mycircle 361 100 CIRC2-XVEL CIRC2-YVEL))
(define circle1-at-188-220 (make-mycircle 188 220 CIRC1-XVEL CIRC1-YVEL))
(define circle2-at-223-186 (make-mycircle 223 86 CIRC2-XVEL CIRC2-YVEL))
(define circle1-at-200-200
  (make-mycircle CIRC1-X-COORD CIRC1-Y-COORD CIRC1-XVEL CIRC1-YVEL))
(define circle2-at-200-100
  (make-mycircle CIRC2-X-COORD CIRC2-Y-COORD CIRC2-XVEL CIRC2-YVEL))


(define paused-world-at-senario1
  (make-world
    circle1-at-200-200
    circle2-at-200-100
    true))

(define unpaused-world-after-senario1
  (make-world
    circle1-at-188-220
    circle2-at-223-186
    false))

(define unpaused-world-at-senario1
  (make-world
    circle1-at-200-200
    circle2-at-200-100
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(is-pause-key-event? kev)
     (world-with-paused-toggled w)]
    [else w]))

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: use template for World on w
(define (world-with-paused-toggled w)
  (make-world
   (world-circ1 w)
   (world-circ2 w)
   (not (world-paused? w))))


;; examples for testing
(define pause-key-event " ")
(define non-pause-key-event "q")  

(begin-for-test
  (check-equal?
    (world-after-key-event paused-world-at-senario1 pause-key-event)
    unpaused-world-at-senario1
    "after pause key, a paused world should become unpaused")
  
  (check-equal?
    (world-after-key-event paused-world-at-senario1 non-pause-key-event)
    paused-world-at-senario1
    "after non pause key, a paused world should stay paused"))

;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
(define (is-pause-key-event? ke)
  (key=? ke " "))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; EXAMPLES: see tests below
;; World Unpaused:
;; (world-after-tick unpaused-world-at-senario1) = unpaused-world-at-senario1
;; world paused:
;; (world-after-tick paused-world-at-senario1) = paused-world-at-senario1
;; STRATEGY: Use template for World on w

(define (world-after-tick w)
  (if (world-paused? w)
    w
    (make-world
      (circle-after-tick (world-circ1 w))
      (circle-after-tick (world-circ2 w))
      (world-paused? w))))

;; tests:
(begin-for-test

  (check-equal? 
    (world-after-tick unpaused-world-at-senario1) 
     unpaused-world-after-senario1
     "In unpaused world, the circle should move with given velocity
      and world should still be unpaused")

  (check-equal? 
    (world-after-tick paused-world-at-senario1)
    paused-world-at-senario1
    "In paused world, circle should be unmoved")

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-circle : Mycircle Scene -> Scene
;; GIVEN : a Mycircle and a scene
;; RETURNS: a scene like the given one, but with the given circle painted
;; on it.
;; STRATEGY : combine simpler function
(define (place-circle c s)
  (place-image 
    (circle CIRCLE_RADIUS "outline" "blue")
    (circ-x c) (circ-y c)
    s))

;; tests

;;; check this visually to make sure it's what you want
(define image-of-circle-at-200
  (place-image SIMLPE_CIRCLE CIRC1-X-COORD CIRC1-Y-COORD EMPTY-CANVAS))

;;; note: these only test whether place-circle calls place-image properly.
;;; it doesn't check to see whether image-of-circle-at-200 is the right image!
(begin-for-test
 (check-equal? 
   (place-circle circle1-at-200-200 EMPTY-CANVAS)
   image-of-circle-at-200
   "place-circle circle1-at-200-200 EMPTY-CANVAS)
    returned unexpected image or value")) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-text : Mycircle Scene -> Scene
;; GIVEN : a Mycircle structure and a scene to print circle on it
;; RETURNS: a scene like the given one, but with the given text painted
;; on it.
;; STRATEGY : combine simpler function
(define (place-text c s)
  (place-image 
    (text (format "( ~a, ~a)" (circ-vx c) (circ-vy c)) TEXT_HEIGHT "blue")
     (circ-x c) (circ-y c)
    s))

;; tests
;;; check this visually to make sure it's what you want
(define image-of-circle-and-text-at-200
  (place-image 
    (text (format "( ~a, ~a)" CIRC1-XVEL CIRC1-YVEL) TEXT_HEIGHT "blue")
     CIRC1-X-COORD CIRC1-Y-COORD
     (place-image SIMLPE_CIRCLE CIRC1-X-COORD CIRC1-Y-COORD EMPTY-CANVAS)))

;;; note: these only test whether place-circle calls place-image properly.
;;; it doesn't check to see whether image-of-circle-at-200 is the right image!
(begin-for-test
 (check-equal? 
   (place-text circle1-at-200-200
     (place-circle circle1-at-200-200 EMPTY-CANVAS))
   image-of-circle-and-text-at-200
   "place-text circle1-at-200-200 EMPTY-CANVAS)
    returned unexpected image or value")) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: see test cases below
;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (place-text (world-circ1 w)
              (place-circle
              (world-circ1 w)
              (place-text (world-circ2 w)
              (place-circle (world-circ2 w) EMPTY-CANVAS)))))


;; tests 

;; an image showing the moving circles at (200,200) and (200,100)
;; check this visually to make sure it's what you want
(define image-of-unpaused-world-at-senario1
 (place-image 
    (text (format "( ~a, ~a)" CIRC1-XVEL CIRC1-YVEL) TEXT_HEIGHT "blue")
     CIRC1-X-COORD CIRC1-Y-COORD
    (place-image SIMLPE_CIRCLE CIRC1-X-COORD CIRC1-Y-COORD
     (place-image 
      (text (format "( ~a, ~a)" CIRC2-XVEL CIRC2-YVEL) TEXT_HEIGHT "blue")
       CIRC2-X-COORD CIRC2-Y-COORD
      (place-image SIMLPE_CIRCLE CIRC2-X-COORD CIRC2-Y-COORD
       EMPTY-CANVAS)))))

;; To test whether world->scene calls place-image properly.
;; Visual verification is needed.
(begin-for-test
  (check-equal? 
    (world-to-scene unpaused-world-at-senario1)
    image-of-unpaused-world-at-senario1
    "test of world-to-scene for unpaused-world-at-senario1"))

  (check-equal?
    (world-to-scene paused-world-at-senario1)
    image-of-unpaused-world-at-senario1
    "test of world-to-scene for paused-world-at-senario1")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; STRATEGY : combine simpler function
(define (initial-world y)
  (make-world
   (new-circle circ1-x circ1-y circ1-vx circ1-vy)
   (new-circle circ2-x circ2-y circ2-vx circ2-vy)
   true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-x-distance : Mycircle -> Mycircle
;; RETURNS: a Mycircle that has adjusted the X-coordinate
;; according to the canvas width.
;; EXAMPLE: (adjust-x-distance circle1-at-361-200) -> circle1-at-360-200
;; STRATEGY: Divide into cases on mycircle-x
(define (adjust-x-distance c)
  (cond
    [(>=  (+ (circ-x c) (circ-vx c)) (- CANVAS-WIDTH CIRCLE_RADIUS))
      (make-mycircle  360 (+ (circ-y c) (circ-vy c) )          
                  (* (circ-vx c) -1) (circ-vy c))]
      
    [(<=  (+ (circ-x c) (circ-vx c)) CIRCLE_RADIUS)
      (make-mycircle  CIRCLE_RADIUS (+ (circ-y c) (circ-vy c) )          
                  (* (circ-vx c) -1) (circ-vy c))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-y-distance : Mycircle -> Mycircle
;; RETURNS: a Mycircle that has adjusted the X-coordinate
;; according to the canvas width.
;; EXAMPLE: (adjust-x-distance circle1-at-200-261) -> circle1-at-200-260
;; a Mycircle with Y-coordinate at 260 and remaining params remains unchanged.
;; STRATEGY: Divide into cases on mycircle-y
(define (adjust-y-distance c)
  (cond
    [(>=  (+ (circ-y c) (circ-vy c)) (- CANVAS-HEIGTH CIRCLE_RADIUS))
     (make-mycircle  (+ (circ-x c) (circ-vx c) )(- CANVAS-HEIGTH CIRCLE_RADIUS)          
                 (circ-vx c) (* (circ-vy c) -1))]
    
    [(<=  (+ (circ-y c) (circ-vy c)) CIRCLE_RADIUS)
     (make-mycircle  (+ (circ-x c) (circ-vx c)) CIRCLE_RADIUS        
                 (circ-vx c)  (* (circ-vy c) -1))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-crossing-x : Mycircle -> Boolean
;; RETURNS: a Boolean indicating whether the circle has breached
;; Canvas Width - Radius boundries
;; EXAMPLE: (is-crossing-x circle1-at-361-200) -> True
;; STRATEGY: Divide into cases on mycircle-x
(define (is-crossing-x c)
  (or  (>=  (+ (circ-x c) (circ-vx c)) 360)
             (<=  (+ (circ-x c) (circ-vx c)) CIRCLE_RADIUS)))

;; tests
(begin-for-test
 (check-equal? (is-crossing-x  circle1-at-200-200) false)
 (check-equal? (is-crossing-x
               (new-circle 30 186 10 10)) true)
 (check-equal? (is-crossing-x  (new-circle 370 186 10 10)) true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-crossing-y : Mycircle -> Boolean
;; RETURNS: a Boolean indicating whether the circle has breached
;; Canvas Width - Radius boundries
;; EXAMPLE: (is-crossing-y circle1-at-100-280) -> True
;; STRATEGY: Divide into cases on mycircle-y
(define (is-crossing-y c)
(or  (>=  (+ (circ-y c) (circ-vy c)) (- CANVAS-HEIGTH CIRCLE_RADIUS))
             (<=  (+ (circ-y c) (circ-vy c)) CIRCLE_RADIUS)))

;; tests
(begin-for-test
 (check-equal? (is-crossing-y  circle1-at-200-200) false)
 (check-equal? (is-crossing-y  circle-at-200-30) true)
 (check-equal? (is-crossing-y  circle-at-200-261) false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-after-tick : Circle -> Circle
;; GIVEN: the state of a circle c
;; RETURNS: the state of the given circle after a tick if it were in an
;; unpaused world.
;; STRATEGY: Divide into cases on c
(define (circle-after-tick c) 
 (cond
  [(is-crossing-x c) (adjust-x-distance c)]
  [(is-crossing-y c) (adjust-y-distance c)]
  [else (make-mycircle
    (+ (circ-x c) (circ-vx c) )
    (+ (circ-y c) (circ-vy c) )          
    (circ-vx c) (circ-vy c))]))


(begin-for-test
 (check-equal?
  (circle-after-tick circle1-at-200-200)
   circle1-at-188-220
   "test of circle-after-tick return wrong circle state"))

 (check-equal?
  (circle-after-tick (new-circle 300 30 10 10))
   (new-circle 310 40 10 -10)
   "test of circle-after-tick return wrong circle state")

 (check-equal?
  (circle-after-tick (new-circle 300 270 10 10))
   (new-circle 310 260 10 -10)
   "test of circle-after-tick return wrong circle state")

 (check-equal?
  (circle-after-tick (new-circle 39 200 -10 10))
   (new-circle 40 210 10 10)
   "test of circle-after-tick return wrong circle state")

 (check-equal?
  (circle-after-tick circle-at-361-100)
   (new-circle 360 86 (* CIRC2-XVEL -1) CIRC2-YVEL)
   "test of circle-after-tick return wrong circle state")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  

(define PAUSE-KEY " ")
	(define INITIAL-WORLD (initial-world "Any"))
	(define UNPAUSED-INITIAL-WORLD (world-after-key-event INITIAL-WORLD PAUSE-KEY))
	(define CIRCLE-NEAR-EDGE (new-circle 10 10 30 20))
	(define WORLD-AFTER-10-TICKS
	  (world-after-tick
	   (world-after-tick
	    (world-after-tick
	     (world-after-tick
	      (world-after-tick
	       (world-after-tick
	        (world-after-tick
	         (world-after-tick
	          (world-after-tick (world-after-tick UNPAUSED-INITIAL-WORLD)))))))))))
	(define WORLD-AFTER-20-TICKS
	  (world-after-tick
	   (world-after-tick
	    (world-after-tick
	     (world-after-tick
	      (world-after-tick
	       (world-after-tick
	        (world-after-tick
	         (world-after-tick
	          (world-after-tick (world-after-tick WORLD-AFTER-10-TICKS)))))))))))


INITIAL-WORLD

(begin-for-test

(check-equal? (circ-y (world-circ1 (world-after-tick UNPAUSED-INITIAL-WORLD))) (+ 100 20)))
