;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-demo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;; two draggable cats.
;; like draggable cat, but there are TWO cats.  They are individually
;; draggable.  But space pauses or unpauses the entire system.

;; draggable cat.
;; like falling cat, but user can drag the cat with the mouse.
;; button-down to select, drag to move, button-up to release.

;; falling cat.  
;; A cat falls from the top of the scene.
;; The user can pause/unpause the cat with the space bar.

;; start with (main 0)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Integer -> World
;; GIVEN: the initial y-position of the cats
;; EFFECT: runs the simulation, starting with the cats falling
;; RETURNS: the final state of the world
(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 0.5)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CAT-IMAGE (circle  40 "outline" "blue"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 1)

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD 200)
(define CAT2-X-COORD 200)
(define CAT1-Y-COORD 100)
(define CAT2-Y-COORD 200)


;; dimensions of the cat
(define HALF-CAT-WIDTH  (/ (image-width  CAT-IMAGE) 2))
(define HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct world (cat1 cat2 paused?))
;; A World is a (make-world Cat Cat Boolean)
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-cat1 w) (world-cat2 w) (world-paused? w)))


(define-struct cat (x-pos y-pos selected?))
;; A Cat is a (make-cat Integer Integer Boolean)
;; Interpretation: 
;; x-pos, y-pos give the position of the cat. 
;; selected? describes whether or not the cat is selected.

;; template:
;; cat-fn : Cat -> ??
;(define (cat-fn c)
; (... (cat-x-pos w) (cat-y-pos w) (cat-selected? w)))

;; examples of cats, for testing
(define selected-cat1-at-20 (make-cat CAT1-X-COORD 20 true))
(define unselected-cat1-at-20 (make-cat CAT1-X-COORD 20 false))

(define selected-cat1-at-28 (make-cat CAT1-X-COORD 28 true))
(define unselected-cat1-at-28 (make-cat CAT1-X-COORD 28 false))

(define selected-cat2-at-35 (make-cat CAT2-X-COORD 35 true))
(define unselected-cat2-at-35 (make-cat CAT2-X-COORD 35 false))

;; examples of worlds, for testing

(define paused-world-at-20
  (make-world
    unselected-cat1-at-20
    selected-cat2-at-35
    true))

(define unpaused-world-at-20
  (make-world
    unselected-cat1-at-20
    selected-cat2-at-35
    false))

;; in an unpaused world, the unselected cat falls, but the selected
;; cat stays pinned to the mouse.
(define unpaused-world-at-20-after-tick
  (make-world
    unselected-cat1-at-28
    selected-cat2-at-35
    false))
  

;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   


;; example MouseEvents for testing:
(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow w after a tick.
;; STRATEGY: Use template for World on w
(define (world-after-tick w)
  (if (world-paused? w)
    w
    (make-world
      (cat-after-tick (world-cat1 w))
      (cat-after-tick (world-cat2 w))
      (world-paused? w))))



;; cat-after-tick : Cat -> Cat
;; GIVEN: the state of a cat c
;; RETURNS: the state of the given cat after a tick if it were in an
;; unpaused world.

;; examples: 
;; cat selected
;; (cat-after-tick selected-cat1-at-20) = selected-cat1-at-20
;; cat paused:
;; (cat-after-tick unselected-cat1-at-20) = unselected-cat-at-28

;; STRATEGY: use template for Cat on c

(define (cat-after-tick c)
  (if (cat-selected? c)
    c
    (make-cat
      (cat-x-pos c)
      (+ (cat-y-pos c) CATSPEED)          
      (cat-selected? c))))

;; tests:
#;(begin-for-test
  ;; cat selected
  (check-equal?
    (cat-after-tick selected-cat1-at-20)
    selected-cat1-at-20
    "selected cat shouldn't move")

  ;; cat unselected
  (check-equal? 
    (cat-after-tick unselected-cat1-at-20)
    unselected-cat1-at-28
    "unselected cat should fall CATSPEED pixels and remain unselected")

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene paused-world-at-20) should return a canvas with
;; two cats, one at (150,20) and one at (300,28)
;;          
;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (place-cat
    (world-cat1 w)
    (place-cat
      (world-cat2 w)
      EMPTY-CANVAS)))

(define image-of-paused-world-at-20
  (place-image CAT-IMAGE 150 20
    (place-image CAT-IMAGE 300 35
      EMPTY-CANVAS)))

;; place-cat : Cat Scene -> Scene
;; RETURNS: a scene like the given one, but with the given cat painted
;; on it.
(define (place-cat c s)
  (place-image
    CAT-IMAGE
    (cat-x-pos c) (cat-y-pos c)
    s))

;; tests

;;; check this visually to make sure it's what you want
(define image-at-20 (place-image CAT-IMAGE CAT1-X-COORD 20 EMPTY-CANVAS))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: use template for World on w
(define (world-with-paused-toggled w)
  (make-world
   (world-cat1 w)
   (world-cat2 w)
   (not (world-paused? w))))



;; in-cat? : Cat Integer Integer -> Cat
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given cat.
;; EXAMPLES: see tests below
;; STRATEGY: Use template for Cat on c
(define (in-cat? c x y)
  (and
    (<= 
      (- (cat-x-pos c) HALF-CAT-WIDTH)
      x
      (+ (cat-x-pos c) HALF-CAT-WIDTH))
    (<= 
      (- (cat-y-pos c) HALF-CAT-HEIGHT)
      y
      (+ (cat-y-pos c) HALF-CAT-HEIGHT))))



;; discussion question: are these tests sufficient to test in-cat?

;; initial-world : Integer -> World
;; RETURNS: a world with two unselected cats at the given y coordinate
(define (initial-world y)
  (make-world
    (make-cat CAT1-X-COORD y false)
    (make-cat CAT2-X-COORD y false)
    false))
