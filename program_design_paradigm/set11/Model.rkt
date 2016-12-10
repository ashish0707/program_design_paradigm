#lang r t

(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")
(require "PerfectBounce.rkt")
(require "X-Controller.rkt")


(provide Model%)


;;***************************************************************

(define Model%
  (class* object% (Model<%>)
 
   
    (field [x-low 0])
    (field [x-high 150]) 
    (field [y-low 0])
    (field [y-high 100]) 
   
    (init-field [x (/ (+ x-low x-high) 2)])
    (init-field [y (/ (+ y-low y-high) 2)]) 
    (init-field [vx 0])
    (init-field [vy 0])

    ;; is the simulation paused?
    (init-field [paused? false]) 

    (init-field [controllers empty]) ;; List of controllers


    (super-new)

    ;; after-tick: -> Void
    ;; GIVEN    : no arguments   
    ;; EFFECT   : reports x and velocity at every tick
    ;; STRATEGY : Divide into cases on paused?
    (define/public (after-tick)
     (local
       ((define new-particle
          (particle-after-tick (make-particle x y vx vy)
                               (make-rect 0 150 0 100))))
       (if paused?
        1
        (begin
         (set! x (withing-bound? 1 (particle-x new-particle) 149))
         (publish-x)
         (set! y (withing-bound? 1 (particle-y new-particle) 99))
         (publish-y)
         (set! vx (particle-vx new-particle))
         (publish-vx)
         (set! vy (particle-vy new-particle))
         (publish-vy)))))

    ;; within-bounds? : PosInt PosInt PosInt -> PosInt
    ;; GIVEN          : low and hign value along with actual value.
    ;; RETURNS        : max value between low and min value between
    ;;                  actual and high value.
    ;; STRATEGY       : combine simpler functions 
    (define (withing-bound? lo val hi)
      (max lo (min val hi)))

    ;; register : Controller -> Void
    ;; GIVEN    : a controller
    ;; EFFECT   : register the new controller and send it some message
    ;; STRATEGY : combine simpler functions
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position x "x"))
        (send c receive-signal (make-report-velocity vx "x"))
        (send c receive-signal (make-report-position y "y"))
        (send c receive-signal (make-report-velocity vy "y"))))

    ;; execute-command : Command -> Void
    ;; GIVEN  : a command
    ;; EFFECT : Decodes the command, executes it, and sends 
    ;;          updates to all the controllers.
    ;; STRATEGY : Divide into cases on command - cmd.
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (if (string=? "x" (set-position-which cmd))
         (begin
           (set! x (set-position-pos cmd))
           (publish-x))
         (begin
           (set! y (set-position-pos cmd))
           (publish-y)))]
        [(set-velocity? cmd)
         (if (string=? "x" (set-velocity-which cmd))
         (begin
           (set! vx (set-velocity-dv cmd))
           (publish-vx))
         (begin
           (set! vy (set-velocity-dv cmd))
           (publish-vy)))]
        [(boolean? cmd)
         (if (equal? cmd true)
             (set! paused? true)
             (set! paused? false))]))

    
    ;; publish-x : -> Void
    ;; EFFECT   : publishes position the controllers in the world
    ;;            if it is changed
    ;; STRATEGY : combine simpler functions
    (define (publish-x)
      (local
        ((define msg (make-report-position x "x")))
        (for-each
          (lambda (c) (send c receive-signal msg))
          controllers)
        ))
    
    ;; publish-y : -> Void
    ;; EFFECT   : publishes y position to all controllers in the world
    ;;            if it is changed
    ;; STRATEGY : combine simpler functions
   (define (publish-y)
      (local
          ((define msg (make-report-position y "y")))
        (for-each
          (lambda (c) (send c receive-signal msg))
          controllers)
        ))

    ;; publish-vx : -> Void
    ;; EFFECT   : publishes x velocity to all controllers in the world
    ;;            if it is changed
    ;; STRATEGY : combine simpler functions
    (define (publish-vx)
      (local
          ((define msg (make-report-velocity vx "x")))
        (for-each
          (lambda (c) (send c receive-signal msg))
          controllers)))

    ;; publish-vy : -> Void
    ;; EFFECT   : publishes y velocity to all controllers in the world
    ;;            if it is changed
    ;; STRATEGY : combine simpler functions
    (define (publish-vy)
      (local
          ((define msg (make-report-velocity vy "y")))
        (for-each
          (lambda (c) (send c receive-signal msg))
          controllers)))

    ;; below funtions return the state of the model.
    (define/public (for-test:x) x)
    (define/public (for-test:y) y)
    (define/public (for-test:vx) vx)
    (define/public (for-test:vy) vy)
    (define/public (for-test:paused?) paused?)
    (define/public (for-test:contollers) controllers)

    ;; For Test cases
    (define/public (for-test:explain-state)
      (list x y vx vy))
 ))

;*********************Testing****************************

;; Constants for Test

(define m1 (new Model%))
(define m3 (new Model%))
(define m4 (new Model%))
(define m5 (new Model%))
(define m6 (new Model%))
(define m2 (new Model% [paused? true]))
(define xc (new X-Controller% [model m1]))
(define xc1 (new X-Controller% [model m3]))


;; TESTS
(begin-for-test
  
  (send m1 after-tick)
  (check-equal?
   (send m2 after-tick) 1
   "no change expected in after tick")
  
  (send m2 register xc)
  (check-equal?
   (send m2 for-test:contollers)
   (list xc)
   "x-controller is not added successfully")
  
  (send m1 execute-command (make-set-position 3 "x"))
  (check-equal?
   (send m1 for-test:x)
   3
   "value of x must be 3")
  (send m1 execute-command (make-set-position 3 "y"))
  (check-equal?
   (send m1 for-test:y)
   3
   "value of y must be 3")
  (send m1 execute-command (make-set-velocity 3 "y"))
  (check-equal?
   (send m1 for-test:vy)
   3
   "vy has to be set to 3")
  (send m1 execute-command (make-set-velocity 3 "x"))
  (check-equal?
   (send m1 for-test:vx)
   3
   "value of vx must be 3")
  (send m1 execute-command true)
  (check-equal?
   (send m1 for-test:paused?)
   true
   "paused should be true")
  (send m1 execute-command false)
  (check-equal?
   (send m1 for-test:paused?)
   false
   "paused should be false")

   (check-true (is-a? m1 Model%)
   "m1 is object of Model class"))




    

    