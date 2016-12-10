;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "q5.rkt")


;; DATA DEFINITIONS: none

;; string-delete String RealNumber -> String          
;; GIVEN: A String in which a character is to be eliminated.
;; A real number that indicates the position to delete the character.
;; RETURNS: A String with character deleted at ith position.
;; EXAMPLES:
;; (string-insert "ashish" 2) "asish"
;; (string-insert "hello" 3) "helo"
;; DESIGN STRATEGY: Combine simpler functions

(provide string-delete)

(define (string-delete str position)
  (if (or (<= (string-length str) 0) (<= (string-length str) position)) "Cannot delete character at given position" 
  (string-append (substring str 0 (- position 1)) (substring str position (string-length str))))
  
  )

;; TESTS
(begin-for-test
  (check-equal?  (string-delete "ashish" 10) "Cannot delete character at given position" 
     " _ inserted incorrectly")
 (check-equal?  (string-delete "hello" 3) "helo"
    " _ inserted incorrectly")
 (check-equal?  (string-delete "" 3) "Cannot delete character at given position" 
    " _ inserted incorrectly")
 )
