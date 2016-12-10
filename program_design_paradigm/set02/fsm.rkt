;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "fsm.rkt")
(provide
  initial-state
  next-state
  accepting-state?
  error-state?
  ) 

;; DATA DEFINITION:
;; a State (MachineState) is one of
;; -- "start"
;; -- state1
;; -- state2 
;; -- state3
;; -- final-state
;; -- error-state

;; Interp:
;; "start", "state1", "u" , "state3", "final-state", "final-state" are the six
;; states of a finite machine for the regualar expression
;; (q | x)* u* (a | b)* d (e | f)*
;; "u" is the state when the MachineInput is either "u".
;; On recipt of string "a" or "b", it jumps to state "state3"
;; else it jumps to error state.
;; "state3" is the state when the MachineInput is either "a" or "b".
;; On recipt of string "d", it jumps to state "d" else it jumps to error state.
;; "d" is the state when the MachineInput is either "d".
;; On recipt of string "e|f", it jumps to state "e|f" else it jumps to error state. 
;; "final-state" is the state when the MachineInput is either "e" or "f" or "d".
;; This is the final state On recipt of any other string the machine terminates.
;; "error-state" is the error state. An invalid input to any state leads to
;; error state.
;; Any of these states can be initial state.

;; TEMPLATE
;; ms-fn : ms -> ??
;(define (ms-fn MachineState)
;  (cond
;    [(string=? ms start)    
;     ...]
;    [(string=? ms state1)    
;     ...]
;    [(string=? ms state2)
;     ...]
;    [(string=? ms state3)  
;     ...]
;    [(string=? ms final-state)
;     ...]))


(define state1 "q|x")
(define state2 "u")
(define state3 "a|b")
(define final-state "d|e|f")
(define error-state "es")

;; next-state : ms MachineInput -> ms
;; GIVEN: a machine state and a single length string
;; RETURNS: the machinestate that follows the given state
;; (next-state state1 "y") = state2
;; (next-state state2 "a") = state3
;; (next-state state3 "d") = final-state
;; (next-state final-state "e") = final-state
;; STRATEGY: cases on machine input

(define (next-state ms MachineInput)
 (cond
  [(or (string=? ms state1) (string=? ms "start"))    
   (if (or (string=? MachineInput "q") (string=? MachineInput "x")) state1
       (if(string=? MachineInput "u") state2 (if (or (string=? MachineInput "a")
          (string=? MachineInput "b")) state3  (if(string=? MachineInput "d")
          final-state error-state))))]
    
  [(string=? ms state2)
   (if (string=? MachineInput "u") state2
       (if (or (string=? MachineInput "a") (string=? MachineInput "b")) state3
           (if(string=? MachineInput "d") final-state error-state)))]

  [(string=? ms state3)  
   (if (or (string=? MachineInput "a") (string=? MachineInput "b")) state3
       (if(string=? MachineInput "d") final-state error-state))]

  [(string=? ms final-state)
   (if (string=? MachineInput "d") error-state
       (if (or (string=? MachineInput "e") (string=? MachineInput "f"))
           final-state error-state))]))



;initial-state : Number -> State
;GIVEN   : a number
;RETURNS : a representation of the initial state
;EXAMPLE : (initial-state 2) => state2
;Interp  : It returns the state of your machine.  The given number is ignored.
;STRATEGY: cases on machine states
(define (initial-state ipnumber)
 state1)

;accepting-state? : State -> Boolean
;GIVEN: a state of the machine
;RETURNS: true iff the given state is a final (accepting) state
;EXAMPLES: (accepting-state? "u") -> false
;(accepting-state? "final-state") -> true
(define (accepting-state? state)
  (if(string=? state final-state) true false))


;error-state : State -> Boolean
;GIVEN: a state of the machine
;RETURNS: true iff there is no path (empty or non-empty) from the given state to
;an accepting state
;EXAMPLES: (error-state "error-state") -> true
;(error-state "u") -> false
;state to an accepting state
(define (error-state? state)
  (cond
    [(string=? state error-state) true]
    [else false]))



(begin-for-test
  (check-equal? (accepting-state? (next-state (next-state (next-state
                (next-state (next-state (next-state (next-state (next-state
                (next-state (initial-state 1)
                "q") "x") "u") "u") "a") "b") "d") "e") "f"))
                true "Not an accepting state.")
  
  (check-equal? (accepting-state? (next-state (initial-state 4) "d"))
                true "Not an accepting state.")
  
  (check-equal? (accepting-state? (next-state (initial-state 0) "e"))
                false "Not an accepting state.")
  
  (check-equal? (accepting-state? (next-state (initial-state 0) "a"))
                false "Not an accepting state.")

  (check-equal? (accepting-state? (next-state (next-state (next-state
                (initial-state 0) "u") "b") "d"))
                true "Not an accepting state.")

  (check-equal? (accepting-state? (next-state (next-state
                (initial-state 0) "u") "d"))
                true "Not an accepting state.")
 
  (check-equal? (accepting-state? (next-state (next-state
                (initial-state 0) "u") "e"))
                false "Not an accepting state.")

  (check-equal? (error-state? (next-state (next-state (next-state
                (initial-state 0) "u") "a") "b"))
                false "Not an accepting state.")
  
  (check-equal? (error-state? (next-state (initial-state 3) "f"))
                true "Not an error state.")
  
  (check-equal? (error-state? (next-state (next-state
                (initial-state 0) "a") "e")) true
                "It is an error state. ")
  
  (check-equal? (error-state? (next-state (next-state
                (initial-state 0) "d") "d")) true
                "It is an error state. ")
  
  (check-equal? (error-state? (next-state (next-state
                (initial-state 0) "d") "q")) true
                "It is an error state. ")

(error-state?
    (next-state
     (next-state (next-state (next-state (initial-state 0) "q") "u") "e")
     "f"))

  
 )



