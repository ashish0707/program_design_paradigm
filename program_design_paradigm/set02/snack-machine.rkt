;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "snack-machine.rkt")
(provide
  initial-machine
  machine-next-state
  machine-output
  machine-remaining-kale
  machine-remaining-carrots
  machine-bank
  ) 

;; DATA DEFINITION:
;A CustomerInput is one of
;-- a PosInt        interp: insert the specified number of quarters
;-- "kale"          interp: request a bag of kale chips
;-- "carrots"       interp: request a bag of carrots
;-- "change"        interp: return all the unspent money that the

;A MachineOutput is one of
;-- "kale"           interp: machine dispenses a bag of kale chips
;-- "carrots"        interp: machine dispenses a bag of carrot sticks
;-- "Out of Item"    interp: machine displays "Out of Item"
;-- a PosInt         interp: machine releases the specified number of quarters
;-- "Nothing"        interp: the machine does Nothing


;; TEMPLATE
;; custInput-fn : ci -> ??
;(define (custInput-fn ci)
;  (cond
;    [(number=? ci)    
;     ...]
;    [(string=? ci "kale")    
;     ...]
;    [(string=? ci "carrots")    
;     ...]
;    [(string=? ci "change")
;     ...]
;))

;; TEMPLATE
;; machOutput-fn : customerInput -> ??
;(define (custInput-fn ci)
;  (cond
;    [(string=? customerInput "change")    
;     ...PosInt ]
;    [(string=? customerInput "kale")    
;     ..."kale" or ... "Nothing ... or ... "Out of Item"]
;    [(string=? customerInput "carrots")    
;     . ..."carrots" or ... "Nothing ... or ... "Out of Item"]))



(define-struct MachineState (kalebags carrotbags amount unspecified))


;; INTERP:
;; A MachineState is a (make-MachineState Integer Integer Struct)
;; CarrotBags and KaleBags are the number of the bags of carrot and kale respectively currently present in the machine. 

;; TEMPLATE:
;; (define (ms-fn MachineState)
;;   (...
;;     (ms-fn CarrotBags) (ms-fn KaleBags) (ms-fn bank) ))


;initial-machine : NonNegInt NonNegInt -> MachineState
;GIVEN: a number of bags of kale chips and carrot sticks
;RETURNS: the state of a machine loaded with the given numbers of bags
;of kale chips and carrot sticks, with an empty bank.
(define (initial-machine kalebags carrotbags )
  (make-MachineState kalebags carrotbags 0 0))


;machine-next-state : MachineState CustomerInput -> MachineState
;GIVEN: a machine state and a customer input
;RETURNS: the state of the machine that should follow the customer's input
;STRATEGY : cases on customer input
(define (machine-next-state ms ci)
  
  (cond
    [(number? ci)    
     (make-MachineState  (MachineState-kalebags ms)(MachineState-carrotbags ms) (MachineState-amount ms) (+  (MachineState-unspecified ms) ci))]
    
    [(string=? ci "kale") (if  (> (machine-remaining-kale ms) 0) (if (>= (MachineState-unspecified ms) 3)   
     (make-MachineState (- (MachineState-kalebags ms) 1) (MachineState-carrotbags ms) (+  (MachineState-amount ms) 3) (-  (MachineState-unspecified ms) 3)) ms) ms)]
    
    [(string=? ci "carrots")    (if (> (machine-remaining-carrots ms) 0)  (if (>= (MachineState-unspecified ms) 2) 
     (make-MachineState  (MachineState-kalebags ms) (- (MachineState-carrotbags ms) 1)  (+  (MachineState-amount ms) 2) (-  (MachineState-unspecified ms) 2)) ms) ms)]
    
    [(string=? ci "change")
     (make-MachineState  (MachineState-kalebags ms) (MachineState-carrotbags ms) (MachineState-amount ms) 0)]))




;machine-output : MachineState CustomerInput -> MachineOutput
;GIVEN: a machine state and a customer input
;RETURNS: a MachineOutput that describes the machine's response to the
;customer input
;STRATEGY: cases on customer input 
(define (machine-output ms customerInput)
  
  (cond
    [(string=? customerInput "kale")    
     (if  (> (machine-remaining-kale ms) 0) (if (>= (MachineState-unspecified ms) 3) "kale" "Nothing") "Out of Item")]
    [(string=? customerInput "carrots")    
     (if (> (machine-remaining-kale ms) 0) (if (>= (MachineState-unspecified ms) 2)  "carrots" "Nothing") "Out of Item")]
    [(string=? customerInput "change")    
     (if(> (MachineState-unspecified ms) 0) (MachineState-unspecified ms) "Nothing")]))


;machine-remaining-kale : MachineState -> NonNegInt
;GIVEN: a machine state
;RETURNS: the number of bags of kale chips left in the machine
(define (machine-remaining-kale ms)
  (MachineState-kalebags ms))

;
;machine-remaining-carrots : MachineState -> NonNegInt
;GIVEN: a machine state
;RETURNS: the number of bags of carrots left in the machine
(define (machine-remaining-carrots ms)
  (MachineState-carrotbags ms))

;machine-bank : MachineState -> NonNegInt
;GIVEN: a machine state
;RETURNS: the amount of money in the machine's bank, in cents
(define (machine-bank ms)
  (* (MachineState-amount ms) 0.25))




(begin-for-test
  (check-equal? (machine-output (machine-next-state (initial-machine 5 5) 2) "change") 2
                "Not the correct amount of cash getting dispensend ")
  
  (check-equal?  (machine-output (machine-next-state (initial-machine 5 5) 2) "kale") "Nothing"
                 "Machine output incorrect")
  
  (check-equal?  (machine-output (machine-next-state (initial-machine 5 5) 1) "carrots") "Nothing"
                 "Machine output incorrect")
  
  (check-equal?  (machine-output (machine-next-state (initial-machine 5 5) 0) "change") "Nothing"
                 "Machine output incorrect")
  
  (check-equal?  (machine-output (machine-next-state (initial-machine 5 5) 3) "kale") "kale"
                 "Machine output incorrect")
  
  (check-equal?  (machine-output (machine-next-state (initial-machine 5 5) 3) "carrots") "carrots"
                 "Machine output incorrect")
  
  (check-equal?  (machine-output (machine-next-state (initial-machine 0 5) 3) "kale") "Out of Item"
                 "Machine output incorrect")
  
  (check-equal?  (machine-output (machine-next-state (initial-machine 0 0) 10) "carrots") "Out of Item"
                 "Machine output incorrect")
  
  (check-equal?  (machine-remaining-kale (machine-next-state (machine-next-state (initial-machine 5 5) 3) "kale")) 4
                 "Machine output incorrect")
  
  (check-equal?  (machine-remaining-carrots (machine-next-state (machine-next-state (initial-machine 5 5) 2) "carrots")) 4
                 "Machine output incorrect")
  
  (check-equal?  (machine-bank (machine-next-state (machine-next-state (initial-machine 5 5) 3) "kale")) 0.75
                 "Machine output incorrect")
  
  (check-equal?  (machine-bank (machine-next-state (machine-next-state (initial-machine 5 5) 3) "kale")) 0.75
                 "Machine output incorrect")
  
  (check-equal?  (machine-bank (machine-next-state (machine-next-state (initial-machine 0 0) 3) "change")) 0
                 "Machine output incorrect")
  
  (check-equal?  (machine-bank (machine-next-state (machine-next-state (initial-machine 0 0) 3) "kale")) 0
                 "Machine output incorrect")
  
  (check-equal?  (machine-bank (machine-next-state (machine-next-state (initial-machine 5 6) 2) "kale")) 0
                 "Machine output incorrect")
  
  (check-equal?  (machine-bank (machine-next-state (machine-next-state (initial-machine 0 0) 3) "carrots")) 0
                 "Machine output incorrect")
  
  (check-equal?  (machine-bank (machine-next-state (machine-next-state (initial-machine 5 6) 1) "carrots")) 0
                 "Machine output incorrect")
  
  
  
  )


