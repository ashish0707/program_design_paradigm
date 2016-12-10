#lang racket
(provide World<%>
         Metatoy<%>
         Widget<%>
         Toy<%>)
         
 

;;;;;;;;;;;;;;;;;;;;INTERFACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define World<%>
  (interface ()
    
    ;; -> World
    ;; GIVEN   : no arguments
    ;; RETURNS : the state of the world at the next tick
    after-tick          
    
    ;; Integer Integer MouseEvent-> World
    ;; GIVEN   : a location
    ;; RETURNS : the state of the world that should follow the
    ;;           given mouse event at the given location.
    after-mouse-event
    
    
    ;; KeyEvent -> World
    ;; GIVEN   : a key event
    ;; RETURNS : the state of the world that should follow the
    ;;           given key event
    after-key-event     
    
    ; -> Scene
    ; GIVEN   : a scene
    ; RETURNS : a scene that depicts this World
    to-scene
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;; A Metatoy is an object of any class that implements Metatoy<%>.
 (define Metatoy<%>
   (interface (World<%>) ;include all methods in WorldState<%>
    
    ;; -> ListOfToy
    get-toys
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;; A Widget is an object whose class
 ;; implements the Widget<%> interface.

 (define Widget<%>
  (interface ()
    
    ;; -> Widget
    ;; GIVEN   : no arguments
    ;; RETURNS : the state of this object
    ;;           that should follow after a tick
    after-tick          
    
    ;; Integer Integer -> Widget
    ;; GIVEN   : a location 
    ;; RETURNS : the state of this object that should follow the
    ;;           specified mouse event at the given location.
    
    after-button-down
    after-button-up
    after-drag
    
    ;; KeyEvent -> Widget
    ;; GIVEN   : a key event 
    ;; RETURNS : the state of this object that should
    ;;           follow after the given key event
    
    after-key-event     
    
    ;; Scene -> Scene
    ;; GIVEN   : a scene
    ;; RETURNS : a scene like the given one, but with
    ;;           this object painted on it.
    
    add-to-scene
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;; A Toy is an object of any class that implements Toy<%>

 (define Toy<%> 
  (interface
      
    ;; The interface Toy<%> inherits from the interface Widget<%>.
    ;; This means that any class that implements Toy<%> must implement
    ;; all the methods from Widget<%> plus all the methods defined here.
    (Widget<%>)
    
    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move
    
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data
    
    ))
