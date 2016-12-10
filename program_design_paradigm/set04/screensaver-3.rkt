;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(provide
	 screensaver
	 initial-world
	 world-after-tick
	 world-after-key-event
	 world-after-mouse-event
	 world-paused?
	 world-circles
	 circle-pen-down?
         circle-after-key-event
	 new-circle
	 circ-x
	 circ-y
	 circ-vx
	 circ-vy
	 circ-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define INITIAL-VEL 0)
(define INITIAL-MOUSE-X -1)
(define INITIAL-MOUSE-Y -1)
(define CANVAS-HEIGHT 300)
(define TEXT_HEIGHT 10)
(define SIMLPE_CIRCLE (circle 40 "outline" "blue"))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CIRCLE-RADIUS 40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DATA DEFINITIONS

(define-struct world (x y circles mouse-button-down? paused?))
;; A World state is a (make-world Int Int LOC Boolean Boolean)
;; x and y indicate the mouse x and y coordinates respectively.
;; circles is a List of Mycircle
;; mouse-button-down? indicates whether the red circle indicating mouse pointer
;; should be printed on canvas
;; paused? describes whether or not the world is paused

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-x w) (world-y w)
;;     (world-circ1 w) (world-circ2 w)
;;     (mouse-button-down? w) (world-paused? w)))



(define-struct mycircle
  (x y vx vy diff-x-circle-mouse diff-y-circle-mouse selected?
  center-trail pen-down?))
;; A Mycircle is a
;; (make-mycirclele NonNegInt NonNegInt Int Int Int Int Boolean LOC Boolean)
;; Interpretation: 
;; x, y give the position of the circle. 
;; vx, vy give the velocity of the circle.
;; selected? states whether the circle is selected by the mouse.
;; diff-x-circle-mouse : give the abs difference between x coordinate of
;; center and x coordinate of mouse pointer
;; diff-y-circle-mouse : give the abs difference between y coordinate of
;; center and y coordinate of mouse pointer
;; center-trail - denotes a list of center co-ordinates
;; the circle has travelled. Its a list of Mycircle.
;; pen-down? : states whether the dots trail is to be drawn or not.


;; template:
;; mycircle-fn : Mycircle -> ??
;; (define (mycircle-fn c)
;; (... (mycircle-x w) (mycircle-y w) (mycircle-vx w) (mycircle-vy w)
;;      (mycircle-diff-x-circle-mouse c)  (mycircle-diff-y-circle-mouse c)
;;      (circ-selected? c) (mycircle-center-trail c)))


;; A ListOfMycircle (LOC) is either
;; -- empty
;; -- (cons Mycircle LOC)

;; loc-fn : LOC -> ??
;; (define (loc-fn loc)
;;   (cond
;;     [(empty? loc) ...]
;;     [else (...
;;             (mycircle-fn (first loc))
;;             (loc-fn (rest loc)))]))


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

;; circ-selected? : Mycircle -> Boolean
;; GIVEN: a Mycircle struct
;; RETURNS: a boolean stating whether the circle is selected
;; STRATEGY : use template for Mycircle on c
(define (circ-selected? c)
  (mycircle-selected? c))

;; circ-mouse-x-diff : Mycircle -> Int
;; GIVEN: a Mycircle struct
;; RETURNS: a int giving the difference between mouse and circle x coordinates
;; STRATEGY : use template for Mycircle on c
(define (circ-mouse-x-diff c)
  (mycircle-diff-x-circle-mouse c))

;; circ-mouse-y-diff : Mycircle -> Int
;; GIVEN: a Mycircle struct
;; RETURNS: a int giving the difference between mouse and circle y coordinates
;; STRATEGY : use template for Mycircle on c
(define (circ-mouse-y-diff c)
  (mycircle-diff-y-circle-mouse c))

;; circle-pen-down? : Mycircle -> Int
;; GIVEN: a Mycircle struct
;; RETURNS: whether pen is down
;; STRATEGY : use template for Mycircle on c
(define (circle-pen-down? c)
  (mycircle-pen-down? c))


;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; STRATEGY : combine simpler functions
(define (new-circle x y vx vy)
  (make-mycircle x y vx vy 0 0 false empty false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples of worlds, for testing

(define paused-world-at-senario1
  (make-world 4 -7
   (list
    (make-mycircle 230 100 6 -10 212 130 false
      (list (make-mycircle 224 110 0 0 0 0 false '() false)
            (make-mycircle 218 120 0 0 0 0 #false '() #false)
            (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
    (make-mycircle 310 260 10 -10 0 0 #false '() #false)) false true))

(define unpaused-world-at-senario1
  (make-world 4 -7
   (list
     (make-mycircle 230 100 6 -10 212 130 false
      (list (make-mycircle 224 110 0 0 0 0 false '() false)
            (make-mycircle 218 120 0 0 0 0 #false '() #false)
            (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
     (make-mycircle 310 260 10 -10 0 0 #false '() #false)) false false))

(define unpaused-world-after-senario1
  (make-world 4 -7
   (list
     (make-mycircle 236 90 6 -10 212 130 false
      (list (make-mycircle 230 100 0 0 0 0 false '() false)
            (make-mycircle 224 110 0 0 0 0 false '() false)
            (make-mycircle 218 120 0 0 0 0 #false '() #false)
            (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
     (make-mycircle 320 250 10 -10 0 0 #false '() #false)) false false))


(define world-after-new-cirlce-at-senario1
  (make-world 4 -7
   (list
    (make-mycircle 200 150 0 0 0 0 #false '() #false)
    (make-mycircle 230 100 6 -10 212 130 false
       (list (make-mycircle 224 110 0 0 0 0 false '() false)
             (make-mycircle 218 120 0 0 0 0 #false '() #false)
             (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
    (make-mycircle 310 260 10 -10 0 0 #false '() #false)) false true))

(define world-with-first-circle-selected-at-senario1
  (make-world 4 -7
    (list
     (make-mycircle 230 100 6 -10 212 130 true
       (list (make-mycircle 224 110 0 0 0 0 false '() false)
             (make-mycircle 218 120 0 0 0 0 #false '() #false)
             (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
  (make-mycircle 310 260 10 -10 0 0 true '() true)) false false))

(define world-with-trail-erased-for-selected-circle
  (make-world 4 -7
   (list
    (make-mycircle 230 100 6 -10 212 130 true '() true)
     (make-mycircle 310 260 10 -10 0 0 true '() true)) false false))


(define world-with-second-circle-selected-at-senario1
  (make-world 4 -7
    (list
     (make-mycircle 230 100 6 -10 212 130 false
       (list (make-mycircle 224 110 0 0 0 0 false '() false)
             (make-mycircle 218 120 0 0 0 0 #false '() #false)
             (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
  (make-mycircle 310 260 10 -10 0 0 true '() #false)) false false))



(define world-with-pen-down-for-selected-circle
  (make-world 4 -7
   (list
    (make-mycircle 230 100 6 -10 212 130 false
     (list (make-mycircle 224 110 0 0 0 0 false '() false)
           (make-mycircle 218 120 0 0 0 0 #false '() #false)
           (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
     (make-mycircle 310 260 10 -10 0 0 true '() true)) false false))



(define world-with-pen-up-for-selected-circle
  (make-world 4 -7
   (list
    (make-mycircle 230 100 6 -10 212 130 false
     (list (make-mycircle 224 110 0 0 0 0 false '() false)
           (make-mycircle 218 120 0 0 0 0 #false '() #false)
           (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
     (make-mycircle 310 260 10 -10 0 0 true '() false)) false false))

(define world-with-increased-x-y-vel-for-selected-circle
  (make-world 4 -7
   (list
    (make-mycircle 230 100 6 -10 212 130 false
     (list (make-mycircle 224 110 0 0 0 0 false '() false)
           (make-mycircle 218 120 0 0 0 0 #false '() #false)
           (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
     (make-mycircle 310 260 12 -8 0 0 true '() false)) false false))

(define world-with-decreased-x-y-vel-for-selected-circle
  (make-world 4 -7
   (list
    (make-mycircle 230 100 6 -10 212 130 false
     (list (make-mycircle 224 110 0 0 0 0 false '() false)
           (make-mycircle 218 120 0 0 0 0 #false '() #false)
           (make-mycircle 212 130 0 0 0 0 #false '() #false)) true)
     (make-mycircle 310 260 10 -10 0 0 true '() false)) false false))

(define world-at-senario-2 
  (make-world 100 200
   (list (make-mycircle 200 188 20 -12 -30 4 false
     (list
      (make-mycircle 180 200 0 0 0 0 #false '() #false)
      (make-mycircle 160 212 0 0 0 0 #false '() #false)
      (make-mycircle 140 224 0 0 0 0 #false '() #false)
      (make-mycircle 120 236 0 0 0 0 #false '() #false)
      (make-mycircle 100 248 0 0 0 0 #false '() #false)
      (make-mycircle 80 260 0 0 0 0 #false '() #false)
      (make-mycircle 60 258 0 0 0 0 #false '() #false)
      (make-mycircle 40 246 0 0 0 0 #false '() #false)
      (make-mycircle 60 234 0 0 0 0 #false '() #false)
      (make-mycircle 80 222 0 0 0 0 #false '() #false)
      (make-mycircle 100 210 0 0 0 0 #false '() #false)
      (make-mycircle 120 198 0 0 0 0 #false '() #false)
      (make-mycircle 140 186 0 0 0 0 #false '() #false)
      (make-mycircle 160 174 0 0 0 0 #false '() #false))
      true)
   (make-mycircle 262 152 -14 -12 -10 -23 #false '() #false)) true false))

;(define unpaused-world-at-senario1
;  (make-world
;   -1 -1
;    circle1-at-200-200
;    circle2-at-200-100
;    false false))
;
;(define unpaused-world-buttondown-at-senario1
;  (make-world
;   220 220
;    circle1-at-200-200
;    circle2-at-200-100
;    true false))
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
    
    [(is-new-circle-key-event? kev)
     (world-with-new-circle-added w)]
    
    [(is-increase-xvelocity-key-event? kev)
     (world-with-new-xVelocity w 2)]
    
    [(is-decrease-xvelocity-key-event? kev)
     (world-with-new-xVelocity w -2)]
    
    [(is-increase-yvelocity-key-event? kev)
     (world-with-new-yVelocity w +2)]
    
    [(is-decrease-yvelocity-key-event? kev)
     (world-with-new-yVelocity w -2)]
    
    [(is-drop-pen-event? kev)
     (world-with-pen-dropped w)]
    
    [(is-erase-pen-event? kev)
     (world-with-pen-marks-erased w)]
    
    [(is-pen-up-event? kev)
     (world-with-pen-up w)]
    
    [else w]))


;; examples for testing
(define pause-key-event " ")
(define new-cicle-key-event "n")
(define non-pause-key-event "q")
(define pen-down-event "d")
(define pen-up-event "u")
(define erase-pen-event "e")
(define increase-x-velocity-event "right")
(define increase-y-velocity-event "left")
(define decrease-x-velocity-event "up") 
(define decrease-y-velocity-event "down")


(begin-for-test
  (check-equal?
   (world-after-key-event paused-world-at-senario1 new-cicle-key-event)
   world-after-new-cirlce-at-senario1
   "after pause key, a new circle should be added to world")
  
  (check-equal?
   (world-after-key-event paused-world-at-senario1 pause-key-event)
   unpaused-world-at-senario1
   "after pause key, a paused world should become unpaused")
  
  (check-equal?
   (world-after-key-event
    world-with-second-circle-selected-at-senario1 pen-down-event)
   world-with-pen-down-for-selected-circle
   "after pen down on second circle, the boolean value for pen-down
    is not true")

   (check-equal?
   (world-after-key-event
    world-with-pen-down-for-selected-circle pen-up-event)
   world-with-pen-up-for-selected-circle
   "after pen up on second circle, the boolean value for pen-down
    is not false")

   (check-equal?
   (world-after-key-event
    world-with-first-circle-selected-at-senario1 erase-pen-event)
    world-with-trail-erased-for-selected-circle
   "cirlce trail not erased after erase pen event")

  (check-equal?
   (world-after-key-event paused-world-at-senario1 non-pause-key-event)
   paused-world-at-senario1
   "after non pause key, a paused world should stay paused")

  (check-equal?
    (world-after-key-event
    (world-after-key-event
    (world-after-key-event
    (world-after-key-event
     world-with-second-circle-selected-at-senario1 increase-x-velocity-event)
      increase-y-velocity-event)
       decrease-x-velocity-event)
        decrease-y-velocity-event)
     world-with-decreased-x-y-vel-for-selected-circle
     "x and y velocities are not updated corrected"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-paused-toggled : World -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: use template for World on w
(define (world-with-paused-toggled w)
  (make-world
   (world-x w) (world-y w)
   (world-circles w)  (world-mouse-button-down? w)
   (not (world-paused? w))))

;; world-with-new-circle-added : World -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one, but with a new circle added
;; at the center of the canvas
;; STRATEGY: use template for World on w
(define (world-with-new-circle-added w)
  (make-world
   (world-x w) (world-y w)
   (add-circle (world-circles w)) (world-mouse-button-down? w)
   (world-paused? w)))

;; add-circle : LOC -> LOC
;; GIVEN: a list of Mycircles
;; RETURNS: a list with a new circle added to it
;; at the center of the canvas
;; STRATEGY: combine simpler functions
(define (add-circle lst)
  (cons (new-circle (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0) lst))

;; world-with-new-xVelocity : World Int -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one but either increasing or decreasing
;; x-velocity of the selected circle
;; STRATEGY: combime simpler function
(define (world-with-new-xVelocity w adjust_factor)
  (make-world
   (world-x w) (world-y w)
   (adjust-xVelocity-for-circles (world-circles w) adjust_factor)
   (world-mouse-button-down? w) (world-paused? w)))


;; world-with-new-yVelocity : World Int -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one but either increasing or decreasing
;; y-velocity of the selected circle
;; STRATEGY: combime simpler function
(define (world-with-new-yVelocity w adjust_factor)
  (make-world
   (world-x w) (world-y w)
   (adjust-yVelocity-for-circles (world-circles w) adjust_factor)
   (world-mouse-button-down? w) (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DROP PEN FUNCTIONS

;; world-with-pen-dropped : World -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one but pen dropped
;; STRATEGY: combime simpler function
(define (world-with-pen-dropped w)
  (make-world
   (world-x w) (world-y w)
   (drop-pen (world-circles w))
   (world-mouse-button-down? w) (world-paused? w)))

;; drop-pen : LOC -> LOC
;; GIVEN: a list of Mycircles
;; RETURNS: a list of Mycircle but with pen-drop set to true for the
;; for the selected circle.
;; STRATEGY: Use template for LOC on lst.
;; HALTING MEASURE : Lenght of LOC.
(define (drop-pen lst)
  (cond
    [(empty? lst) empty]
    [else  (cons (drop-pen-if-selected (first lst))
                 (drop-pen (rest lst)))]))

;; drop-pen-if-selected : Mycircle -> Mycircle
;; GIVEN: a Mycircle 
;; RETURNS: a Mycircle just like the given one but with pen drop set to true.
;; STRATEGY: combime simpler function
(define (drop-pen-if-selected c)
  (if (circ-selected? c)  
      (make-mycircle (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
                    (circ-mouse-x-diff c)
                     (circ-mouse-y-diff c) (circ-selected? c)
                     (mycircle-center-trail c) true ) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERASE PEN FUNCTIONS

;; world-with-pen-marks-erased : World -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one but pen marks erased
;; for the selected circle
;; STRATEGY: combime simpler function
(define (world-with-pen-marks-erased w)
  (make-world
   (world-x w) (world-y w)
   (erase-pen-marks (world-circles w))
   (world-mouse-button-down? w) (world-paused? w)))

;; erase-pen-marks : LOC -> LOC
;; GIVEN: a list of Mycircles
;; RETURNS: a list of Mycircle but with circle trail erased for the
;; for the selected circle.
;; STRATEGY: Use template for LOC on lst.
;; HALTING MEASURE : Lenght of LOC.
(define (erase-pen-marks lst)
  (cond
    [(empty? lst) empty]
    [else  (cons (erase-marks-if-selected (first lst))
                 (erase-pen-marks (rest lst)))]))

;; erase-marks-if-selected : Mycircle -> Mycircle
;; GIVEN: a Mycircle 
;; RETURNS: a Mycircle just like the given one but with circle trail erased
;; STRATEGY: combime simpler function
(define (erase-marks-if-selected c)
  (if (circ-selected? c)  
      (make-mycircle (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
                    (circ-mouse-x-diff c)
                     (circ-mouse-y-diff c) (circ-selected? c)
                     empty (circle-pen-down? c)) c))

;;; tests:
(begin-for-test
  (check-equal? (erase-marks-if-selected
                (make-mycircle 200 200 10 10 -20 -20 false '() false))
                (make-mycircle 200 200 10 10 -20 -20 false '() false)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PEN UP FUNCTIONS

;; world-with-pen-marks-erased : World -> World
;; GIVEN: a world w
;; RETURNS: a world just like the given one but pen marks erased
;; for the selected circle
;; STRATEGY: combime simpler function
(define (world-with-pen-up w)
  (make-world
   (world-x w) (world-y w)
   (pen-up (world-circles w))
   (world-mouse-button-down? w) (world-paused? w)))


;; pen-up : LOC -> LOC
;; GIVEN: a list of Mycircles
;; RETURNS: a list of Mycircle but with pen-drop set to false for the
;; for the selected circles.
;; STRATEGY: Use template for LOC on lst.
;; HALTING MEASURE : Lenght of LOC.
(define (pen-up lst)
  (cond
    [(empty? lst) empty]
    [else  (cons (pen-up-if-selected (first lst))
                 (pen-up (rest lst)))]))


;; pen-up-if-selected : Mycircle -> Mycircle
;; GIVEN: a Mycircle 
;; RETURNS: a Mycircle just like the given one but with pen drop set to false.
;; STRATEGY: combime simpler function
(define (pen-up-if-selected c)
  (if (circ-selected? c)  
      (make-mycircle (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
                    (circ-mouse-x-diff c)
                     (circ-mouse-y-diff c) (circ-selected? c)
                     (mycircle-center-trail c)  false) c))


;; circle-after-key-event : Circle KeyEvent -> Circle
;; GIVEN : a circle and a key event.
;; RETURNS: the state of the circle that should follow the given
;; circle after the given key event
;; STRATEGY : Divide on cases for Key Event - kev
(define (circle-after-key-event c kev)
(cond
    
    [(is-increase-xvelocity-key-event? kev)
     (adjust-xVelocity-for-circle c 2)]
    
    [(is-decrease-xvelocity-key-event? kev)
     (adjust-xVelocity-for-circle c -2)]
    
    [(is-increase-yvelocity-key-event? kev)
     (adjust-yVelocity-for-circle c +2)]
    
    [(is-decrease-yvelocity-key-event? kev)
     (adjust-yVelocity-for-circle c -2)]
    
    [(is-drop-pen-event? kev)
     (drop-pen-if-selected c)]
    
    [(is-erase-pen-event? kev)
     (erase-marks-if-selected c)]
    
    [(is-pen-up-event? kev)
     (pen-up-if-selected c)]

  ))

;;; tests:
(begin-for-test
  (check-equal? (circle-after-key-event
                 (make-mycircle 200 200 10 10 -20 -20 true '() false)
                 "right")
                (make-mycircle 200 200 12 10 -20 -20 #true '() false))
  (check-equal? (circle-after-key-event
                 (make-mycircle 200 200 10 10 -20 -20 true '() false)
                 "left")
                (make-mycircle 200 200 8 10 -20 -20 #true '() false))
  (check-equal? (circle-after-key-event
                 (make-mycircle 200 200 10 10 -20 -20 true '() false)
                 "up")
                (make-mycircle 200 200 10 12 -20 -20 #true '() false))
  (check-equal? (circle-after-key-event
                 (make-mycircle 200 200 10 10 -20 -20 true '() false)
                 "down")
                (make-mycircle 200 200 10 8 -20 -20 #true '() false))
  (check-equal? (circle-after-key-event
                 (make-mycircle 200 200 10 10 -20 -20 true '() false)
                 "d")
                (make-mycircle 200 200 10 10 -20 -20 #true '() true))
  (check-equal? (circle-after-key-event
                 (make-mycircle 200 200 10 10 -20 -20 true '() false)
                 "e")
                (make-mycircle 200 200 10 10 -20 -20 #true '() false))
  
  (check-equal? (circle-after-key-event
                 (make-mycircle 200 200 10 10 -20 -20 true '() false)
                 "u")
                (make-mycircle 200 200 10 10 -20 -20 #true '() false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Helper function for key event

;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
;; STRATEGY : combine simpler functions
(define (is-pause-key-event? ke)
  (key=? ke " "))

;; help function for key event
;; is-new-circle-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a create new circle instruction
;; STRATEGY : combine simpler functions
(define (is-new-circle-key-event? ke)
  (key=? ke "n"))

;; help function for key event
;; increase-xvelocity-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a increase-xvelocity
;; instruction.
;; STRATEGY : combine simpler functions
(define (is-increase-xvelocity-key-event? ke)
  (key=? ke "right"))

;; help function for key event
;; decrease-velocity-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a decrease-xvelocity
;; instruction.
;; STRATEGY : combine simpler functions
(define (is-decrease-xvelocity-key-event? ke)
  (key=? ke "left"))

;; help function for key event
;; increase-yvelocity-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a increase-yvelocity
;; instruction.
;; STRATEGY : combine simpler functions
(define (is-increase-yvelocity-key-event? ke)
  (key=? ke "up"))

;; help function for key event
;; decrease-yvelocity-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a decrease-yvelocity
;; instruction.
;; STRATEGY : combine simpler functions
(define (is-decrease-yvelocity-key-event? ke)
  (key=? ke "down"))

;; help function for key event
;; is-drop-pen-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a drop pen instruction.
(define (is-drop-pen-event? ke)
  (key=? ke "d"))

;; help function for key event
;; is-erase-pen-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a erase marks instruction.
;; STRATEGY : combine simpler functions
(define (is-erase-pen-event? ke)
  (key=? ke "e"))

;; help function for key event
;; is-pen-up-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pen up instruction.
;; STRATEGY : combine simpler functions
(define (is-pen-up-event? ke)
  (key=? ke "u"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-xVelocity-for-circle : LOC Integer -> LOC
;; GIVEN: a circle list and a value for increasing x velocity with.
;; RETURNS: the list of circles with new x velocity for selected circles.
;; examples:  See test cases below
;; strategy: Use template for LOC on lst
(define (adjust-xVelocity-for-circles lst adjust_value)
  (cond
    [(empty? lst) empty]
    [else  (cons (adjust-xVelocity-for-circle (first lst) adjust_value)
                 (adjust-xVelocity-for-circles (rest lst) adjust_value))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; adjust-xVelocity-for-circle : MyCircle Integer -> MyCircle
;; GIVEN: a circle and a value for increasing x velocity with.
;; RETURNS: the circle with new x velocity for selected circle.
;; examples:  See test cases below
;; STRATEGY : combine simpler functions
(define (adjust-xVelocity-for-circle c adjust_value)
  (if (circ-selected? c)
      (make-mycircle (circ-x c) (circ-y c)
                     (+ adjust_value (circ-vx c)) (circ-vy c)
                    (circ-mouse-x-diff c)
                     (circ-mouse-y-diff c) (circ-selected? c)
                     (mycircle-center-trail c) (circle-pen-down? c)) c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-yVelocity-for-circles : LOC Integer -> LOC
;; GIVEN: a circle list and a value for increasing y velocity with.
;; RETURNS: the list of circles with new y velocity for selected circles.
;; examples:  See test cases below
;; strategy: Use template for LOC on lst
(define (adjust-yVelocity-for-circles lst adjust_value)
  (cond
    [(empty? lst) empty]
    [else  (cons (adjust-yVelocity-for-circle (first lst) adjust_value)
                 (adjust-yVelocity-for-circles (rest lst) adjust_value))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-yVelocity-for-circle : MyCircle Integer -> MyCircle
;; GIVEN: a circle and a value for increasing y velocity with.
;; RETURNS: the circle with new y velocity for selected circle.
;; examples:  See test cases below
;; STRATEGY : combine simpler functions
(define (adjust-yVelocity-for-circle c adjust_value)
  (if (circ-selected? c)
      (make-mycircle (circ-x c)  (circ-y c)
                     (circ-vx c) (+ adjust_value (circ-vy c))
                    (circ-mouse-x-diff c)
                     (circ-mouse-y-diff c) (circ-selected? c)
                     (mycircle-center-trail c) (circle-pen-down? c)) c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for World on w
(define (world-after-mouse-event w x y event)
  (make-world
   x y
   (circles-after-mouse-event (world-circles w) x y event)
   (mouse-ptr-after-event w event)
   (world-paused? w)))

;;; tests:
 (begin-for-test
  (check-equal? (world-after-mouse-event
                 world-with-first-circle-selected-at-senario1 200 200
                 "button-down")
                (make-world 200 200
                 (list
                  (make-mycircle 230 100 6 -10 212 130 true 
                   (list (make-mycircle 224 110 0 0 0 0 #false '() #false)
                         (make-mycircle 218 120 0 0 0 0 #false '() #false)
                         (make-mycircle 212 130 0 0 0 0 #false '() #false))
                   true)
                  (make-mycircle 310 260 10 -10 0 0 #true '() #true))
                 true false))
  
   (check-equal? (world-after-mouse-event
                 world-with-first-circle-selected-at-senario1 200 200
                 "drag")
                 (make-world 200 200
                 (list
                  (make-mycircle 412 330 6 -10 212 130 true 
                   (list (make-mycircle 224 110 0 0 0 0 #false '() #false)
                         (make-mycircle 218 120 0 0 0 0 #false '() #false)
                         (make-mycircle 212 130 0 0 0 0 #false '() #false))
                   true)
                  (make-mycircle 200 200 10 -10 0 0 #true '() #true))
                 true false))

    (check-equal? (world-after-mouse-event
                 world-with-first-circle-selected-at-senario1 200 200
                 "button-up")
                   (make-world 200 200
                 (list
                  (make-mycircle 230 100 6 -10 212 130 false 
                   (list (make-mycircle 224 110 0 0 0 0 #false '() #false)
                         (make-mycircle 218 120 0 0 0 0 #false '() #false)
                         (make-mycircle 212 130 0 0 0 0 #false '() #false))
                   true)
                  (make-mycircle 310 260 10 -10 0 0 false '() true))
                 false false)))

;; world-after-button-up : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for World on w
(define (mouse-ptr-after-event w event)
  (cond
    [(mouse=? event "button-down") true]
    [(mouse=? event "drag") true]
    [(mouse=? event "button-up") false]))

;; circles-after-mouse-event : LOC Integer Integer MouseEvent -> LOC
;; GIVEN: a circle list and a description of a mouse event
;; RETURNS: the list of circles that should follow the given mouse event
;; examples:  See test cases below
;; strategy: Use template for LOC on lst
(define (circles-after-mouse-event lst mx my mev)
  (cond
    [(mouse=? mev "button-down") (circles-after-button-down lst mx my)]
    [(mouse=? mev "drag") (circles-after-drag lst mx my)]
    [(mouse=? mev "button-up") (circles-after-button-up lst mx my)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circles-after-button-down : LOC Integer Integer -> LOC
;; RETURNS: the cat following a button-down at the given location.
;; STRATEGY: Use template for LOC on lst
(define (circles-after-button-down lst x y)
  (cond
    [(empty? lst) empty]
    [else (cons (circle-after-button-down (first lst) x y)
                (circles-after-button-down (rest lst) x y))]))


;; circle-after-button-down : circle Integer Integer -> circle
;; RETURNS: the cat following a button-down at the given location.
;; STRATEGY: combine simpler functions.
(define (circle-after-button-down c x y)
  (if (in-circle? c x y)
      (make-mycircle (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
                     (- (circ-x c) x) (- (circ-y c) y)
                     true (mycircle-center-trail c) (circle-pen-down? c)) c))

;;; tests:
(begin-for-test
  (check-equal? (circle-after-button-down (new-circle 200 200 10 10) 250 220)
                (make-mycircle 200 200 10 10 0 0 false '() false))
  (check-equal? (circle-after-button-down (new-circle 200 200 10 10) 220 220)
                (make-mycircle 200 200 10 10 -20 -20 true '() false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circles-after-drag : LOC Integer Integer -> LOC
;; RETURNS: the LOC following a button-down at the given location.
;; STRATEGY: Use template for LOC on lst
(define (circles-after-drag lst x y)
  (cond
    [(empty? lst) empty]
    [else (cons (circle-after-drag (first lst) x y)
                (circles-after-drag (rest lst) x y))]))

;; circle-after-drag : circle Integer Integer -> circle
;; RETURNS: the Mycircle following a drag at the given location
;; STRATEGY: Use cases for Circle on c
(define (circle-after-drag c x y)
  (if (circ-selected? c)
      (make-mycircle  (+ x(circ-mouse-x-diff c))
                      (+ y (circ-mouse-y-diff c))
                      (circ-vx c) (circ-vy c)(circ-mouse-x-diff c)
                      (circ-mouse-y-diff c) true
                      (mycircle-center-trail c) (circle-pen-down? c)) c))

;;; tests:
(begin-for-test
  (check-equal? (circle-after-drag
                 (make-mycircle 200 200 10 10 0 0 false '() false) 200 230)
                (make-mycircle 200 200 10 10 0 0 false '() false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; circles-after-button-up : LOC Integer Integer -> LOC
;; RETURNS: the LOC following a button-up at the given location.
;; STRATEGY: Use template for LOC on lst
; function dup
(define (circles-after-button-up lst x y)
  (cond
    [(empty? lst) empty]
    [else (cons (circle-after-button-up (first lst) x y)
                (circles-after-button-up (rest lst) x y ))]))

;; circle-after-button-up : Mycircle Integer Integer -> circle 
;; RETURNS: the circle following a button-up at the given location
;; STRATEGY: Use template for circle on c
(define (circle-after-button-up c x y)
  (unselect-circle
   (check-x-crossing-for-buttonup
    (check-y-crossing-for-buttonup c))))

;; tests:
(begin-for-test

  (check-equal? 
   (circle-after-button-up
   (make-mycircle 200 270 20 20 10 10 true '() false) 210 280)
   (make-mycircle 200 260 20 -20 10 10 false '() false))

 (check-equal? 
   (circle-after-button-up
    (make-mycircle 370 200 20 20 10 10 true '() false) 360 190)
   (make-mycircle 360 200 -20 20 10 10 false '() false))

 (check-equal? 
   (circle-after-button-up
    (make-mycircle 30 200 -20 20 10 10 true '() false) 50 190)
   (make-mycircle 40 200 20 20 10 10 false '() false))

 (check-equal? 
   (circle-after-button-up
    (make-mycircle 200 30 20 -20 10 10 true '() false) 210 40)
   (make-mycircle 200 40 20 20 10 10 false '() false)))

;; check-x-crossing-for-buttonup : Mycircle -> Mycircle 
;; RETURNS: the mycircle with adjusted x coordinate if crossing widht borders
;; STRATEGY: combine simpler functions
(define (check-x-crossing-for-buttonup c)
  (if (is-crossing-x c) (adjust-x-distance-buttonup c) c))

;; check-y-crossing-for-buttonup : Mycircle -> Mycircle 
;; RETURNS: the mycircle with adjusted y coordinate if crossing height borders
;; STRATEGY: combine simpler functions
(define (check-y-crossing-for-buttonup c)
  (if (is-crossing-y c) (adjust-y-distance-buttonup c) c))

;; unselect-circle : Mycircle -> Mycircle 
;; RETURNS: unselect the circle
;; STRATEGY: combine simpler functions
(define (unselect-circle c)
  (make-mycircle (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
                (circ-mouse-x-diff c) (circ-mouse-y-diff c)
                 false (mycircle-center-trail c) (circle-pen-down? c)))


;; adjust-x-distance-buttonup : Mycircle -> Mycircle
;; RETURNS: a Mycircle that has adjusted the X-coordinate
;; according to the canvas width.
;; EXAMPLE: (adjust-x-distance-buttonup circle1-at-361-200) ->
;; circle-at-360-200 where 360 and 200 and x and y coordinates
;; STRATEGY: Divide into cases on mycircle-x
(define (adjust-x-distance-buttonup c)
  (cond
    [(>=  (+ (circ-x c) (circ-vx c)) (- CANVAS-WIDTH CIRCLE-RADIUS))
     (make-mycircle (- CANVAS-WIDTH CIRCLE-RADIUS) (circ-y c)          
                    (* (circ-vx c) -1) (circ-vy c)
                   (circ-mouse-x-diff c)(circ-mouse-y-diff c)
                    (circ-selected? c) (mycircle-center-trail c)
                    (circle-pen-down? c))]
    
    [(<= (+ (circ-x c) (circ-vx c)) CIRCLE-RADIUS)
     (make-mycircle  CIRCLE-RADIUS (circ-y c)
                     (* (circ-vx c) -1) (circ-vy c)
                    (circ-mouse-x-diff c)(circ-mouse-y-diff c)
                     (circ-selected? c) (mycircle-center-trail c)
                     (circle-pen-down? c))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; adjust-y-distance-buttonup : Mycircle -> Mycircle
;; RETURNS: a Mycircle that has adjusted the X-coordinate
;; according to the canvas width.
;; EXAMPLE: (adjust-y-distance-buttonup circle1-at-200-261) -> circle1-at-200-260
;; where 200 and 260 are the x and y coordinates
;; STRATEGY: Divide into cases on mycircle-y
(define (adjust-y-distance-buttonup c)
  (cond
    [(>=  (+ (circ-y c) (circ-vy c)) (- CANVAS-HEIGHT CIRCLE-RADIUS))
     (make-mycircle   (circ-x c) (- CANVAS-HEIGHT CIRCLE-RADIUS)         
                      (circ-vx c) (* (circ-vy c) -1)
                     (circ-mouse-x-diff c)(circ-mouse-y-diff c)
                      (circ-selected? c) (mycircle-center-trail c) (circle-pen-down? c))]
    
    [(<=  (+ (circ-y c) (circ-vy c)) CIRCLE-RADIUS)
     (make-mycircle (circ-x c) CIRCLE-RADIUS        
                    (circ-vx c)  (* (circ-vy c) -1)
                   (circ-mouse-x-diff c)(circ-mouse-y-diff c)
                    (circ-selected? c) (mycircle-center-trail c) (circle-pen-down? c))]))



;; in-circle? : Mycircle Integer Integer -> Mycircle
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given circle.
;; EXAMPLES: see tests below
;; STRATEGY: Use template for Circle on c
(define (in-circle? c x y)
  (<= (sqrt(+ (sqr (- (circ-x c) x))  (sqr (- (circ-y c) y)))) CIRCLE-RADIUS))



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
       (world-x w)
       (world-y w)
       (circles-after-tick (world-circles w))
       (world-mouse-button-down? w)
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
    "In paused world, circle should be unmoved"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; STRATEGY : combine simlper functions
(define (initial-world y)
  (make-world -1 -1 empty false true))

;;test
(begin-for-test
  (check-equal? (initial-world "any") (make-world -1 -1 '() false true)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-x-distance : Mycircle -> Mycircle
;; RETURNS: a Mycircle that has adjusted the X-coordinate
;; according to the canvas width.
;; EXAMPLE: (adjust-x-distance circle1-at-361-200) should return a Mycircle
;; with X-coordinate at 360 and remaining params remains unchanged.
;; STRATEGY: Divide into cases on mycircle-x
(define (adjust-x-distance c)
 (cond
  [(>=  (+ (circ-x c) (circ-vx c)) (- CANVAS-WIDTH CIRCLE-RADIUS))
   (make-mycircle (- CANVAS-WIDTH CIRCLE-RADIUS) (+ (circ-y c) (circ-vy c) )          
                  (* (circ-vx c) -1) (circ-vy c)(mycircle-diff-x-circle-mouse c)
                  (circ-mouse-y-diff c)
                  (circ-selected? c)
                  (add-black-circle (mycircle-center-trail c) c)
                  (circle-pen-down? c))]
    
  [(<=  (+ (circ-x c) (circ-vx c)) CIRCLE-RADIUS)
   (make-mycircle  CIRCLE-RADIUS (+ (circ-y c) (circ-vy c) )
                  (* (circ-vx c) -1) (circ-vy c)
                 (circ-mouse-x-diff c)
                  (circ-mouse-y-diff c) (circ-selected? c)
                  (add-black-circle (mycircle-center-trail c) c)
                  (circle-pen-down? c))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; adjust-y-distance : Mycircle -> Mycircle
;; RETURNS: a Mycircle that has adjusted the X-coordinate
;; according to the canvas width.
;; EXAMPLE: (adjust-x-distance circle1-at-200-261) -> circle1-at-200-260
;; a Mycircle with Y-coordinate at 260 and remaining params remains unchanged.
;; STRATEGY: Divide into cases on mycircle-y and mycircle-vy
(define (adjust-y-distance c)
  (cond
    [(>=  (+ (circ-y c) (circ-vy c)) (- CANVAS-HEIGHT CIRCLE-RADIUS))
     (make-mycircle  (+ (circ-x c) (circ-vx c)) (- CANVAS-HEIGHT CIRCLE-RADIUS)         
                     (circ-vx c) (* (circ-vy c) -1)
                    (circ-mouse-x-diff c)
                     (circ-mouse-y-diff c)
                     (circ-selected? c)
                     (add-black-circle (mycircle-center-trail c) c)
                     (circle-pen-down? c))]
    
    [(<=  (+ (circ-y c) (circ-vy c)) CIRCLE-RADIUS)
     (make-mycircle (+ (circ-x c) (circ-vx c)) CIRCLE-RADIUS        
                    (circ-vx c)  (* (circ-vy c) -1)
                   (circ-mouse-x-diff c)
                    (circ-mouse-y-diff c)
                    (circ-selected? c)
                    (add-black-circle (mycircle-center-trail c) c)
                    (circle-pen-down? c))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-crossing-x : Mycircle -> Boolean
;; RETURNS: a Boolean indicating whether the circle has breached
;; Canvas Width - Radius boundries
;; EXAMPLE: (is-crossing-x circle1-at-361-200) -> True
;; STRATEGY: Divide into cases on mycircle-x and mycircle-vx
(define (is-crossing-x c)
  (or  (>=  (+ (circ-x c) (circ-vx c)) (- CANVAS-WIDTH CIRCLE-RADIUS))
       (<=  (+ (circ-x c) (circ-vx c)) CIRCLE-RADIUS)))

;; tests
(begin-for-test
 (check-equal? (is-crossing-x (new-circle 30 186 10 10)) true)
 (check-equal? (is-crossing-x (new-circle 370 186 10 10)) true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-crossing-y : Mycircle -> Boolean
;; RETURNS: a Boolean indicating whether the circle has breached
;; Canvas Width - Radius boundries
;; EXAMPLE: (is-crossing-y circle1-at-100-280) -> True
;; STRATEGY: Divide into cases on mycircle-y and mycircle-vy
(define (is-crossing-y c)
  (or  (>=  (+ (circ-y c) (circ-vy c)) (- CANVAS-HEIGHT CIRCLE-RADIUS))
       (<=  (+ (circ-y c) (circ-vy c)) CIRCLE-RADIUS)))

;; tests
(begin-for-test
  (check-equal? (is-crossing-y  (new-circle 300 276 10 10)) true)
  (check-equal? (is-crossing-y  (new-circle 300 39 10 -10)) true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circles-after-tick : LOC  -> LOC
;; RETURNS: the LOC updating the x and y coordinates of the circles.
;; STRATEGY: Use template for LOC on lst
(define (circles-after-tick lst)
  (cond
    [(empty? lst) empty]
    [else (cons (circle-after-tick (first lst))
                (circles-after-tick (rest lst)))]))


;; circle-after-tick : Mycircle -> Mycircle
;; GIVEN: the state of a circle c
;; RETURNS: the state of the given circle after a tick if it were in an
;; unpaused world.
;; STRATEGY: Divide into cases on c
(define (circle-after-tick c)
  (if (circ-selected? c) c 
      (cond
        [(is-crossing-x c) (adjust-x-distance c)]
        [(is-crossing-y c) (adjust-y-distance c)]
        [else (make-mycircle
               (+ (circ-x c) (circ-vx c) )
               (+ (circ-y c) (circ-vy c) )          
               (circ-vx c) (circ-vy c)
              (circ-mouse-x-diff c)(circ-mouse-y-diff c)
               (circ-selected? c)
               (add-black-circle (mycircle-center-trail c) c)
               (circle-pen-down? c))])))

;; tests
(define SELECTED-CIRCLE
  (make-mycircle 310 260 10 -10 0 0 true '() true))
(begin-for-test
 
 (check-equal?
  (circle-after-tick SELECTED-CIRCLE)
   SELECTED-CIRCLE
   "test of circle-after-tick return wrong circle state")

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
  (circle-after-tick (new-circle 361 100 10 10))
   (new-circle 360 110 -10 10)
   "test of circle-after-tick return wrong circle state"))


;; STRATEGY : Combine simple functions
(define (add-black-circle lst c)
  (if (circle-pen-down? c)
      (cons (new-circle (circ-x c) (circ-y c) 0 0) lst) lst))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION
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
            (on-mouse world-after-mouse-event)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-red-circle : Int Int Scene -> Scene
;; GIVEN : x and y coordinate of Mycircle and a scene to print circle
;; RETURNS: a scene like the given one, but circle painted
;; on it.
;; STRATEGY : combine simpler functions
(define (place-red-circle x y s)
  (place-image 
   (circle 5 "solid" "red")
   x y 
   s))

;; place-circle : Mycircle Scene -> Scene
;; GIVEN : Mycircle structure and a scene to print circle
;; RETURNS: a scene like the given one, but with the given circle painted
;; on it.
;; STRATEGY : Use template for Mycircle on c
(define (place-circle c s)
  (if (circ-selected? c)
      (place-image 
       (circle CIRCLE-RADIUS "outline" "red")
       (circ-x c) (circ-y c) s)
      (place-image 
       (circle CIRCLE-RADIUS "outline" "blue")
       (circ-x c) (circ-y c)
       s)))

;; place-black-circle : Mycircle Scene -> Scene
;; GIVEN : Mycircle structure and a scene to print circle
;; RETURNS: a scene like the given one, but with the given circle painted
;; on it.
;; STRATEGY : combine simpler functions
(define (place-black-circle c s)
  (place-image 
   (circle 1 "outline" "black")
   (circ-x c) (circ-y c) s))


;; test
;; check this visually to make sure it's what you want
(define image-of-selected-circle-at-200
  (place-image  (circle CIRCLE-RADIUS "outline" "red") 200 200 EMPTY-CANVAS))

;;; note: these only test whether place-circle calls place-image properly.
;;; it doesn't check to see whether image-of-circle-at-200 is the right image.
(begin-for-test
 (check-equal? 
   (place-circle (make-mycircle 200 200 10 10 0 0  true '() true) EMPTY-CANVAS)
   image-of-selected-circle-at-200
   "place-circle circle1-at-200-200 EMPTY-CANVAS)
    returned unexpected image or value")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-text : Mycircle Scene -> Scene
;; GIVEN : a mycircle structure and a scene to print circle on it
;; RETURNS: a scene like the given one, but with the given text painted
;; on it.
;; STRATEGY : combine simpler function
(define (place-text c s)
  (place-image 
   (text (format "( ~a, ~a)" (circ-vx c) (circ-vy c)) TEXT_HEIGHT "blue")
   (circ-x c) (circ-y c)
   s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene paused-world-at-20) should return a canvas with
;; two circle, one at (200,100) and one at (200,200)
;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (if (world-mouse-button-down? w)      
      (place-red-circle (world-x w) (world-y w)
                        (place-circles-with-text (world-circles w)))
      (place-circles-with-text (world-circles w))))


;; place-circles-with-text : LOC -> Scene
;; GIVEN : a list of Mycircles
;; RETURNS: a scene placing all the circles with its velocity in center
;; STRATEGY : Use template for LOC on lst
(define (place-circles-with-text lst)
  (cond
    [(empty? lst) EMPTY-CANVAS]
    [else  (place-circle-with-text (first lst)
                                   (place-circles-with-text (rest lst)))]))

;; place-circle-with-text : LOC -> Scene
;; GIVEN : a list of Mycircles
;; RETURNS: a scene placing the circles with its velocity in center
;; STRATEGY : combine simpler functions
(define (place-circle-with-text c s)
  (if  (not (empty? (mycircle-center-trail c)))
       (place-text c (place-black-circles
                      (mycircle-center-trail c) (place-circle c s)))
       (place-text c (place-circle c s))))


;; place-black-circles : LOC -> Scene
;; GIVEN : a list of Mycircles
;; RETURNS: a scene placing all the circles with its velocity in center
;; STRATEGY : Use template for LOC on lst
(define (place-black-circles lst s)
  (cond
    [(empty? lst) s]
    [else  (place-black-circle (first lst) 
                               (place-black-circles (rest lst) s))]))

;; tests 
(define image-of-paused-buttondown-world-at-senario1
  (world-to-scene paused-world-at-senario1))
(define world-image-at-senario2
  (world-to-scene world-at-senario-2))

;; an image showing the circles at (200,200) and (200,100) with button down
;; check this visually to make sure it's what you want
(define image-of-unpaused-buttondown-world-at-senario1
 (place-red-circle 212 130
  (place-image 
    (text (format "( ~a, ~a)" 6 -10) TEXT_HEIGHT "blue")
     230 100
    (place-image SIMLPE_CIRCLE 230 100
     (place-image 
      (text (format "( ~a, ~a)" 10 -10) TEXT_HEIGHT "blue")
       310 260
      (place-image SIMLPE_CIRCLE 310 260
       EMPTY-CANVAS))))))


;; To test whether world->scene calls place-image properly.
;; Visual verification is needed.
(begin-for-test
  (check-equal? 
    (world-to-scene paused-world-at-senario1)
    image-of-paused-buttondown-world-at-senario1
    "test of world-to-scene for paused-world-at-senario1 failed")

    (check-equal? 
    (world-to-scene world-at-senario-2)
    world-image-at-senario2
    "test of world-to-scene for paused-world-at-senario1 failed")
  )


