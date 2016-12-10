;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "07" "q2.rkt")

(provide
 probe-possible-outcome?
 make-turn-right
 make-turn-left
 make-move-forward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define NORTH 1)
(define WEST 2)
(define SOUTH 3)
(define EAST 4)
(define X-BACK-COUNT 0)
(define X-FORWARD-COUNT 0)
(define Y-BACK-COUNT 0)
(define Y-FORWARD-COUNT 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct probe(x y direction count-x-b count-x-f count-y-b count-y-f))

;; A probe is a (make-probe Integer Integer PosInteger
;;                          NonNegint NonNegint NonNegint NonNegint)
;; INTERP:
;; x and y are the coordinates of the center of the probe, facing north
;; direction is the way probe is facing between 1 and 4 inclusive.
;; the probe takes 1 step of 1 cm
;; count-x-b and count-y-b are the counter to check the negative
;; range of the probe
;; count-x-f and count-y-f are the counter to check the positive
;; range of the probe

;; TEMPLATE:
;; probe-fn : Probe -> ??
;; (define (probe-fn p)
;;   (...
;;     (probe-x p)
;;     (probe-y p)
;;     (probe-direction p)
;;     (probe-count-x-b p)
;;     (probe-count-x-f p)
;;     (probe-count-y-b p)
;;     (probe-count-y-f p)))

;; Direction is one of
;; -- 1
;; -- 2
;; -- 3
;; -- 4

;; INTERP:
;; 1 is North
;; 2 is West
;; 3 is South
;; 4 is East

(define-struct turn-left ())

;; A turn-left is a (make-turn-left )
;; INTERP:
;; The probe takes a left turn

;; TEMPLATE:
;; turn-left-fn : Turn-Left -> ??
;; (define (turn-left-fn p)
;;   (...))

(define-struct turn-right ())

;; A turn-righ is a (make-turn-right )
;; INTERP:
;; The probe takes a right turn

;; TEMPLATE:
;; turn-right-fn : Turn-Right -> ??
;; (define (turn-right-fn p)
;;   (...))


(define-struct move-forward (steps))

;; a move-forawrd is a (make-move-forward PosInt)
;; INTERP:
;; steps is the number of steps the probe takes
;; 1 step is of 1 cm

;; TEMPLATE:
;; move-forward-fn : Move-Forward -> ??
;; (define (move-forward-fn p)
;;   (...
;;      (move-forward-steps p)))


;; An Instruction is one of
;; -- (make-turn-left)            Interp: a turn-left instruction
;; -- (make-turn-right)           Interp: a turn-right instruction
;; -- (make-move-forward PosInt)  Interp: an instruction to move forward
;;                                        the given number of steps.

;; A List of Instructions (LOI) is one of:
;; -- empty
;; -- (cons Instruction LOI)

;; Template:
;; loi-fn : LOI -> ??
;; HALTING MEASURE: (length lst)
;; (define (loi-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (loi-fn (rest lst)))]))


;; A Program is a ListOfInstruction
;; INTERP:
;; A sequence of instructions, to be executed from left to right.

;; An Operator is a function with contract
;; Integer , Interger -> Integer
;; that is, it is a function that takes a two Interger values and 
;; produces a Interger value.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES FOR TESTING

(define probe1 (make-probe 20 100 1 0 0 0 0))

(define p1 (list (make-turn-right)
                 (make-move-forward 10)
                 (make-turn-right)
                 (make-move-forward 5)))


(define p2 (list (make-turn-right)
                 (make-turn-right)
                 (make-turn-right)
                 (make-move-forward 11)
                 (make-turn-left)))

(define p3 (list (make-turn-left)
                 (make-turn-left)
                 (make-turn-left)
                 (make-turn-left)
                 (make-move-forward 10)))

(define p4 (list (make-move-forward 1)))

(define p5 (list 
                 (make-move-forward 1)
                 (make-turn-right)
                 (make-turn-right)
                 (make-move-forward 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-possible-outcome? : Int Int Program Int Int -> Boolean
;; GIVEN    : starting coordinates x0, y0, a robot program p, and ending
;; coordinates x1, y1.
;; RETURNS  : true iff the robot, starting at (x0, y0) and facing north,
;; and executing program p according to the tolerances given above,
;; could end at (x1, y1).
;; EXAMPLES :
;; Let p1 = (list (make-turn-right)
;;                (make-move-forward 10)
;;                (make-turn-right)
;;                (make-move-forward 5))
;; then (probe-possible-outcome? 20 100 p1 x1 y1) = true iff
;; x1 is in the interval [28, 32] and y1 is in the interval
;; [103,107].
;; STRATEGY : combine simpler functions

(define (probe-possible-outcome? x1 y1 lst x2 y2)
  (check-status?(probe-mover (initial-probe x1 y1) lst)x2 y2))

;; initial-probe : Integer Integer -> Probe
;; GIVEN    : starting coordinate of the probe
;; RETURNS  : a probe centered at the given coordinates facing north
;; EXAMPLE  :
;; (initial-probe 20 100) = (make-probe 20 100 1 0 0 0 0)
;; STRATEGY : combine simpler functions

(define (initial-probe x y)
  (make-probe x y NORTH X-BACK-COUNT
              X-FORWARD-COUNT Y-BACK-COUNT Y-FORWARD-COUNT))

;; probe-mover : Probe LOI ->  Probe
;; GIVEN    : a probe and a list of instructions
;; RETURNS  : a probe after the execution of the program
;; EXAMPLE  :
;; (probe-mover probe1 p1) = (make-probe 30 105 3 2 2 2 2)
;; STRATEGY : use HOF foldl on lst
;; HALTING MEASURE : Length of lst.

(define (probe-mover prb lst)
  ;; Instruction PROBE ->  PROBE) PROBE LST -> PROBE
  (foldl
      ;; Instruction PROBE ->  PROBE
      ;; RETURNS : a Probe after the execution of the Instruction
      get-command prb
   lst))

;; get-command : Instruction Probe ->  Probe
;; GIVEN: an Instruction and a Probe
;; RETURNS: a Probe after the execution of the Instruction
;; EXAMPLE:
;; (get-command probe1 (make-turn-right) p2) = (make-probe 20 100 4 0 0 0 0)
;; STRATEGY: cases on Instruction

(define (get-command instruction prb)
  (cond
    [(equal? instruction (make-turn-right))
     (turn-probe prb -1)]
    [(equal? instruction (make-turn-left))
     (turn-probe prb 1)]
    [else (move-probe prb instruction)]))

;; turn-probe : Probe Integer -> Probe
;; GIVEN    : a Probe and the Direction to turn the probe to
;; RETURNS  : the given Probe facing in the new direction
;; EXAMPLE  :
;; (turn-probe probe1 -1) = (make-probe 20 100 4 0 0 0 0)
;; STRATEGY : us template of Probe on prb

(define (turn-probe prb turn)
  (make-probe (probe-x prb)
                (probe-y prb)
                (change-direction (probe-direction prb) turn)
                (probe-count-x-b prb) (probe-count-x-f prb)
                (probe-count-y-b prb) (probe-count-y-f prb)))

;; change-direction: Direction Integer -> PosInt
;; GIVEN : the current direction and the direction to turn the probe
;; RETURNS: the new direction of the probe
;; EXAMPLE:
;; (change-direction 1 -1) = 4
;; STRATEGY: cases on direction dir

(define (change-direction dir turn)
  (cond
    [(and (equal? dir NORTH) (equal? turn -1)) EAST]
    [(and (equal? dir EAST) (equal? turn 1)) NORTH]
    [else (+ dir turn)]))

;; move-probe : Probe Instruction -> Probe
;; GIVEN: a probe and a move forward instruction
;; RETURNS: a new probe after moving the given number of steps
;; EXAMPLE:
;; (move-probe probe1 (make-move-forward 10) p1) = (make-probe 20 90 1 0 0 2 2)
;; STRATEGY: combine simpler functions

(define (move-probe prb ins)
  (move-probe-forward prb (move-forward-steps ins)))

;; move-probe-forward : Probe PosInt -> Probe
;; GIVEN    : a Probe and given number of steps
;; RETURNS  : a new Probe after moving the given number of steps
;; EXAMPLE  :
;; (move-probe-forward probe1 10) = (make-probe 20 90 1 0 0 2 2)
;; STRATEGY : cases on Direction 

(define (move-probe-forward prb steps)
  (cond
    [(= (probe-direction prb) NORTH) 
     (create-probe-y prb NORTH steps -)]
    [(= (probe-direction prb) WEST)
     (create-probe-x prb WEST steps -)]
    [(= (probe-direction prb) SOUTH)
     (create-probe-y prb SOUTH steps +)]
    [(= (probe-direction prb) EAST)
     (create-probe-x prb EAST steps +)]))

;; create-probe-x : Probe Direction PosInt Operator -> Probe
;; GIVEN    : a probe, its direction, number of steps taken and an operator
;; depending upon the axis.
;; RETURNS  :  a probe that has moved the given number of steps on x axis.
;; EXAMPLE  :
;; (create-probe-x probe1 2 3 +) = (make-probe 23 100 2 2 2 0 0)
;; STRATEGY : use template of Probe on prb

(define (create-probe-x prb dir steps op)
  (make-probe (op (probe-x prb) steps) (probe-y prb) dir
              (get-backward-count (probe-count-x-b prb) steps dir)
              (get-forward-count (probe-count-x-f prb) steps dir)
              (probe-count-y-b prb) (probe-count-y-f prb)))

;; create-probe-y : Probe Direction PosInt Operator -> Probe
;; GIVEN    : a probe, its direction, number of steps taken and an operator
;; depending upon the axis.
;; RETURNS  :  a probe that has moved the given number of step on y axis.
;; EXAMPLE  :
;; (create-probe-y probe1 2 3 +) = (make-probe 20 103 2 0 0 2 2)
;; STRATEGY : combine simpler functions

(define (create-probe-y prb dir steps op)
  (make-probe (probe-x prb) (op (probe-y prb) steps) dir
              (probe-count-x-b prb)(probe-count-x-f prb)
              (get-backward-count (probe-count-y-b prb) steps dir )
              (get-forward-count (probe-count-y-f prb) steps dir )))


;; get-backward-count : (X->Y) PosInt Direction -> Integer
;; GIVEN: a function, number of steps and direction of probe.
;; RETURNS: an integer with the possible number of steps that probe can take
;; EXAMPLE:
;; (get-backward-count (probe-count-y-b probe1) 10 3) = 2
;; STRATEGY: combine simpler functions

(define (get-backward-count fn steps dir)
  (if (or (= dir 2) (= dir 1))
      (+ fn 2)
      (if (> steps 1)
          (+ fn 2)
          (+ fn 1))))

;; get-forward-count : (X->Y) PosInt Direction -> Integer
;; GIVEN: a function, number of steps and direction of probe.
;; RETURNS: an integer with the possible number of steps that probe can take
;; EXAMPLE:
;; (get-forward-count (probe-count-x-f probe1) 1 2) = 1
;; STRATEGY: combine simpler functions

(define (get-forward-count fn steps dir)
  (if (or (= dir 1) (= dir 2))
      (if (> steps 1)
          (+ fn 2)
          (+ fn 1))
      (+ fn 2)))

;; check-status? : Probe Integer Integer -> Boolean
;; GIVEN: a probe and the coordinates to check the status of the probe against
;; RETURNS: a boolean if the probe is at the same location as
;; the given  coordinates
;; EXAMPLE:
;; (check-status? probe1 10 100) = false
;; STRATEGY: use template of Probe on prb

(define (check-status? prb x y)
  (and (>= x (- (probe-x prb) (probe-count-x-b prb)))
       (<= x (+ (probe-x prb) (probe-count-x-f prb)))
       (>= y (- (probe-y prb) (probe-count-y-b prb)))
       (<= y (+ (probe-y prb) (probe-count-y-f prb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS
(begin-for-test
  (check-equal? (probe-possible-outcome? 0 0 empty 0 0) true
                "Position of probe when no instructions are given")
  (check-equal? (probe-possible-outcome? 20 100 p1 28 103) true
                "Probe is in the range of the given coordinates")
  (check-equal? (probe-possible-outcome? 13 4 p2 14 5) false
                "Probe is not in the range of the given coordinates")
  (check-equal? (probe-possible-outcome? 13 4 p3 13 -6) true
                "Probe is in the range of the given coordinates")
  (check-equal? (probe-possible-outcome? 20 40 p4 20 41) false
                "Probe is not in the range of the given coordinates")
  (check-equal? (probe-possible-outcome? 20 40 p5 20 40) true
                "Probe is in the range of the given coordinates")
  (check-equal? (probe-possible-outcome? 20 40 p5 20 44) false
                "Probe is not in the range of the given coordinates")
  (check-equal? (get-forward-count (probe-count-x-f probe1) 1 2) 1
                "Probe can take 1 step in this direction")
  (check-equal? (get-forward-count (probe-count-x-f probe1) 10 3) 2
                "Probe can take 2 step in this direction")
  (check-equal? (initial-probe 20 40) (make-probe 20 40 1 0 0 0 0)
                "Initial probe")
  (check-equal? (turn-probe probe1 -1) (make-probe 20 100 4 0 0 0 0)
                "Turn the intial probe right")
  (check-equal? (turn-probe probe1 1) (make-probe 20 100 2 0 0 0 0)
                "Turn the intial probe left")
  (check-equal? (change-direction NORTH -1) EAST
                "The new direction after turning the probe right")
  (check-equal? (change-direction EAST 1) NORTH
                "The new direction after turning the probe left")
  (check-equal? (move-probe probe1 (make-move-forward 2))
                (make-probe 20 98 NORTH 0 0 2 2)
                "Move the probe by 2 steps in the given direction")
  (check-equal? (move-probe probe1 (make-move-forward 1))
                (make-probe 20 99 NORTH 0 0 2 1)
                "Move the probe by 2 steps in the given direction")
  (check-equal? (move-probe-forward probe1 15) (make-probe 20 85 1 0 0 2 2)
                "Move the probe by 15 steps")
  (check-equal? (create-probe-y probe1 3 14 +) (make-probe 20 114 3 0 0 2 2)
                "Make a new probe after moving it in the direction")
  (check-equal? (create-probe-x probe1 2 8 -) (make-probe 12 100 2 2 2 0 0)
                "Make a new probe after moving it in the direction")
  (check-equal? (check-status? probe1 0 0) false
                "get the status of the probe after execution of instructions")
  (check-equal? (check-status? probe1 20 100) true
                "get the status of the probe after execution of instructions"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;