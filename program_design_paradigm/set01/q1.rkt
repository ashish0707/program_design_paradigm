;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q1.rkt")


;; DATA DEFINITIONS: none

;; distance-to-origin Integer Integer -> PosInt          
;; GIVEN: Point in 2-d co-ordinate system,
;; x : Indicates x co-ordinate of the point, y : Indicate y co-ordinate of the point 
;; RETURNS: The distance of the point from origin.
;; EXAMPLES:
;; (distance-to-origin 3 4) = 5
;; (distance-to-origin 6 8) = 10
;; DESIGN STRATEGY: Combine simpler functions

(provide distance-to-origin)

(define (distance-to-origin x y)
  (sqrt (+ (* x x) (* y y))))

;; TESTS
(begin-for-test
  (check-equal? (distance-to-origin 3 4) 5
    "Distance from point (3,5) should be 4")
  (check-equal? (distance-to-origin -6 -8) 10 
   "Distance from point (10,26) should be 24"))
