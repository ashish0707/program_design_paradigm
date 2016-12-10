#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "WidgetWorks.rkt")
(require "Model.rkt")
(require "World.rkt")
(require "Y-Controller.rkt")
(require "X-Controller.rkt")
(require "XY-Controller.rkt")
(require "Position-Controller.rkt")
(require "VelocityController.rkt")

(require rackunit)
(require "extras.rkt")

(provide ControllerFactory%)
;; The ControllerFactory% class is a
;; (new ControllerFactory% [world World<%>][mod Model<%>])
;; This class creates all the controllers, register it with model and
;; adds to the world.
(define ControllerFactory%
  (class* object% (SWidget<%>)

    (init-field world)  
    (init-field model)   

    (super-new) 

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN    : a key event
    ;; EFFECT   : adds a controller in the world
    ;; STRATEGY : Divide into cases on kev.
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "y") (add-controller Y-Controller%)]
        [(key=? kev "x") (add-controller X-Controller%)]
        [(key=? kev "z") (add-controller XY-Controller%)]
        [(key=? kev "v") (add-controller Velocity-Controller%)]
        [(key=? kev "p") (add-controller Position-Controller%)]))

    
    ;; add-controller : Controller% -> Void
    ;; GIVEN    : a contoller class reference.
    ;; EFFECT   : adds the given controller as a SWidget to world.
    ;; STRATEGY : combine simpler functions
    (define/public (add-controller cn-class)
      (send world add-widget (new cn-class [model model])))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN    : a scene
    ;; RETURNS  : same scene
    ;; STRATEGY : combine simpler functions
    (define/public (add-to-scene s) s)

    ;; controller factory ignores these functions
    (define/public (after-tick) this)
    (define/public (after-drag mx my) this)
    (define/public (after-button-down mx my) this) 
    (define/public (after-button-up mx my) this)
    (define/public (after-move mx my) this)

    ))



;;;;;;;;;;;;TESTING-FRAMEWORK;;;;;;;;;;;;;;;;;;

;; TESTS
(define model1 (new Model%))
(define world1 (new World% [model model1]))

(define cf1 (new ControllerFactory% [world world1] [model model1]))


(begin-for-test
  
  (send cf1 after-key-event "x")
 
  (check-true
   (is-a?
    (first (send world1 for-test:widgets))
    X-Controller% 
    )
   "X-Controller added incorrectly")

  (send cf1 after-key-event "y") 

  (check-true
   (is-a?
    (first (send world1 for-test:widgets))
    Y-Controller% 
    )
   "YController added incorrectly")
  
  (send cf1 after-key-event "z")

  (check-true
   (is-a?
    (first (send world1 for-test:widgets))
    XY-Controller%)
   "XY-Controller added incorrectly")

  (send cf1 after-key-event "p")

  (check-true
   (is-a?
    (first (send world1 for-test:widgets))
    Position-Controller%)
   "PositionController added incorrectly")

  (send cf1 after-key-event "v")

  (check-true
   (is-a?
    (first (send world1 for-test:widgets))
    Velocity-Controller%)
   " Velocity-Controller added incorrectly")
  
  (check-equal?
   (send cf1 add-to-scene
         (empty-scene 600 500))
         (empty-scene 600 500)
   "something went wrong while adding scene")
  
  (check-equal?
   (send cf1 after-tick)
   cf1
   "factory does not respond to after-tick ")

  (check-equal?
   (send cf1 after-button-down 5 6)
   cf1
   "factory does not respond to after-button-down ")

  (check-equal?
   (send cf1 after-button-up 4 5)
   cf1
   "factory does not respond to after-button-up ")

  (check-equal?
   (send cf1 after-move 4 5)
   cf1
   "factory does not respond to after-move ")

  (check-equal?
   (send cf1 after-drag 4 5)
   cf1
   "factory does not respond to after-drag "))
  
  



