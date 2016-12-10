;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q2.rkt")


;; DATA DEFINITIONS: none

;; string-last String -> 1String          
;; GIVEN: A String whose last element is to be extracted 
;; RETURNS: Provides the last character of the given string.
;; EXAMPLES:
;; (string-last "hello") = o
;; (string-last "testing") = g
;; DESIGN STRATEGY: Combine simpler functions

(provide string-last)

(define (string-last str)
  ( if (> (string-length str) 0)
  (substring str (- (string-length str) 1) (string-length str))
  "cannot extract the last character. Please check the input string"))

;; TESTS
(begin-for-test
  (check-equal? (string-last "hello") "o"
    "1string should be o ")
   (check-equal? (string-last "") "cannot extract the last character. Please check the input string"
    "1string should be o ")
 (check-equal? (string-last "testing") "g"
    "1string should be  g "))
