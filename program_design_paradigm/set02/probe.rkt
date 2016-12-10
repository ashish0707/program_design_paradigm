;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "probe.rkt")
(provide
  probe-at
  probe-turned-left
  probe-direction-equal?
  probe-location-equal?
  probe-forward-possible-outcome?
  ) 

(define-struct coordinate (x y))

; INTERP: A coordinate struct denotes a point in graphical co-ordinate system
; x is the X co-ordinate of the point 
; y is the Y co-ordinate of the point
; A coordinate is a (make-coordinate Int Int)
; EXAMPLE : (make-coordinate 0 -1)

; TEMPLATE
; coordinate-fn : coordinate -> ??
; (define (coordinate-fn c)
;  ...
;  (coordinate-x c)
;  (coordinate-y c))


(define-struct probe(coordinate direction))

; INTERP: A probe struct denotes a probe with co-ordinates and a direction
; coordinate denote exact location of a probe
; A probe is (make-probe coordinate "left")
; EXAMPLE : (make-coordinate 0 -1)

; TEMPLATE
; probe-fn : probe -> ??
; (define (direction-fn p)
;  ...
;  (probe-coordinate p)
;  (probe-direction p))


;; DATA DEFINITION:
;A Direction is one of
;-- "east"        interp: request the probe to turn left
;-- "west"       interp: request the probe to turn right
;-- "north"       interp: request the probe to turn north
;-- "south"       interp: request the probe to turn south

;; TEMPLATE
;; direction-fn : d -> ??
;(define (direction-fn d)
;  (cond
;    [(string=? td "east")    
;     ...]
;    [(string=? td "west")    
;     ...]
;    [(string=? td "north")    
;     ...]
;    [(string=? td "south")    
;     ...]))



;; DATA DEFINITION:
;A tdirection is one of
;-- "left"        interp: request the probe to turn left
;-- "right"       interp: request the probe to turn right


;; TEMPLATE
;; tdirection-fn : td -> ??
;(define (tdirection-fn td)
;  (cond
;    [(string=? td "left")    
;     ...]
;    [(string=? td "right")    
;     ...]))


(define ONEUNIT 1)
(define -ONEUNIT -1)

(define (get-direction cdirection tdirection)
  
  (cond
    [(string=? tdirection "left")
     (cond
       [(string=? cdirection "north") "west"]
       [(string=? cdirection "south") "east"]
       [(string=? cdirection "west") "south"]
       [(string=? cdirection "east") "north"])]
    
    [(string=? tdirection "right")
     (cond
       [(string=? cdirection "north") "east"]
       [(string=? cdirection "south") "west"]
       [(string=? cdirection "west") "north"]
       [(string=? cdirection "east") "south"])]
    
    
    )
  
  )

;probe-location-possible-equal? : Probe Probe -> Boolean
;GIVEN: two probles
;RETURNS: true iff the two probes may be at the same location
;EXAMPLE: (probe-location-possible-equal? (make-probe (make-coordinate 0 1) "left") (make-probe (make-coordinate 0 1) "right")) -> true
(define (probe-location-possible-equal? probe1 probe2)
  (if (and (and (>= (- (coordinate-x (probe-coordinate probe1)) (coordinate-x (probe-coordinate probe2))) -ONEUNIT)
                (<= (- (coordinate-x (probe-coordinate probe1)) (coordinate-x (probe-coordinate probe2))) ONEUNIT))
           (and (>= (- (coordinate-y (probe-coordinate probe1)) (coordinate-y (probe-coordinate probe2))) -ONEUNIT)
                (<= (- (coordinate-y (probe-coordinate probe1)) (coordinate-y (probe-coordinate probe2))) ONEUNIT))
           ) true false))


;probe-location-equal? : Probe Probe -> Boolean
;GIVEN: two probles
;RETURNS: true iff the two probes are at the same location
;EXAMPLE: (probe-location-equal? (make-probe (make-coordinate 0 1) "left") (make-probe (make-coordinate 0 1) "right")) -> true
(define (probe-location-equal? probe1 probe2)
  (if (and (= (- (coordinate-x (probe-coordinate probe1)) (coordinate-x (probe-coordinate probe2))) 0)
                (= (- (coordinate-y (probe-coordinate probe1)) (coordinate-y (probe-coordinate probe2))) 0))
            true false))

;probe-direction-equal : Probe Probe -> Boolean
;GIVEN: two probles
;RETURNS: true iff the two probes are at the same location
(define (probe-direction-equal? probe1 probe2)
  (if (string=? (probe-direction probe1) (probe-direction probe2)) true false))

;probe-turned-right : Probe -> Probe
;GIVEN: a probe
;RETURNS: a probe like the original, but turned 90 degrees to right
;EXAMPLE : (probe-turned-right probe) = probe
(define (probe-turned-right probe)
  (make-probe (probe-coordinate probe)(get-direction (probe-direction probe) "right")))

;probe-turned-left : Probe -> Probe
;GIVEN: a probe
;RETURNS: a probe like the original, but turned 90 degrees to left
;EXAMPLE : (probe-turned-right probe) = probe
(define (probe-turned-left probe)
  (make-probe (probe-coordinate probe)(get-direction (probe-direction probe) "left")))

;probe-at: Integer Integer -> Probe
;GIVEN: an x-coordinate and a y-coordinate
;RETURNS: a probe with its center at those coordinates, facing north.
;EXAMPLE: (probe-at 0 1)  -> probe
(define (probe-at x y)
  (make-probe (make-coordinate x y) "north"))


;probe-forward-possible-outcome? : Probe PosInt Probe -> Boolean
;GIVEN: two probes and a distance
;RETURNS: true iff the first probe, given a move-forward command with
;the specified number of steps, could wind up in the state described by
;the second probe.
(define (probe-forward-possible-outcome? probe1 distance probe2)
  (if (and (probe-location-possible-equal? (move-forward probe1 distance) probe2) (probe-direction-equal? (move-forward probe1 distance) probe2)) true false))


;move-forward : probe distance -> probe
;GIVEN: a probe and a distance in cm that prob wants to travel
;RETURNS: a probe at the near or exact distance from the current position
;Example: (move-forward (make-probe (make-coordinate 0 1) "left") 10) -> (make-probe (make-coordinate -10 1) "left")
(define (move-forward probe1 distance)
  
  (cond
    [(string=? (probe-direction probe1) "north")
     (make-probe (make-coordinate  (coordinate-x (probe-coordinate probe1))  (- (coordinate-y (probe-coordinate probe1)) distance)) (probe-direction probe1))]
    [(string=? (probe-direction probe1) "south")
     (make-probe (make-coordinate  (coordinate-x (probe-coordinate probe1))  (+ (coordinate-y (probe-coordinate probe1)) distance)) (probe-direction probe1))]
    [(string=? (probe-direction probe1) "west")
     (make-probe (make-coordinate (- (coordinate-x (probe-coordinate probe1)) distance) (coordinate-y (probe-coordinate probe1))) (probe-direction probe1))]
    [(string=? (probe-direction probe1) "east")
     (make-probe (make-coordinate (+ (coordinate-x (probe-coordinate probe1)) distance) (coordinate-y (probe-coordinate probe1))) (probe-direction probe1))]))




(begin-for-test
  (check-equal? (probe-forward-possible-outcome? (probe-at 3 3) 10 (probe-at 3 -8)) true
                "error in either calculation distance or checking direction")
  
 (check-equal? (probe-forward-possible-outcome? (probe-at 3 3) 0 (probe-turned-right (probe-at 3 3))) false
                "error in either calculation distance or checking direction")

  (check-equal? (probe-location-equal? (probe-at 3 3)  (probe-at 3 5)) false
                "error in either calculation distance or checking direction")

  (check-equal? (probe-location-equal? (probe-at 3 3)  (probe-at 3 3)) true
                "error in either calculation distance or checking direction")

  (check-equal? (probe-direction (probe-turned-left (probe-turned-left (probe-turned-left (probe-turned-left (probe-at 3 3)))))) "north"
                "probe turned in wrong direction")

  (check-equal? (probe-location-equal? (probe-turned-right (move-forward (probe-turned-right (move-forward (probe-turned-right (move-forward
                (probe-turned-right (move-forward (probe-at 3 3) 10)) 10)) 10)) 10)) (probe-at 3 3)) true
                "error in either calculation distance or checking direction")

    )


