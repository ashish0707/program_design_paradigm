#lang racket

(require "WidgetWorks.rkt")
(provide Metatoy<%>
         Toy<%>)

         
 

;;;;;;;;;;;;;;;;;;;;INTERFACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;; A Metatoy is an object of any class that implements Metatoy<%>.
 (define Metatoy<%>
   (interface (SWidget<%>) ;include all methods in WorldState<%>
    
    ;; -> ListOfToy
    get-toys
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;; A Toy is an object of any class that implements Toy<%>

 (define Toy<%> 
  (interface
      
    ;; The interface Toy<%> inherits from the interface SWidget<%>.
    ;; This means that any class that implements Toy<%> must implement
    ;; all the methods from SWidget<%> plus all the methods defined here.
    (SWidget<%>)
    
    ;; toy-x : -> Int
    ;; RETURNS: the x position of the center of the toy
    toy-x

    ;; toy-y : -> Int
    ;; RETURNS: the y position of the center of the toy
    toy-y
    
    ;; toy-data : -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data
    
    ))
