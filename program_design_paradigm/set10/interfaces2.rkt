#lang racket

(require "WidgetWorks.rkt")
(provide
 Sblock<%>)
         
 

;;;;;;;;;;;;;;;;;;;;INTERFACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 ;; A Sblock is an object of any class that implements Sblock<%>

 (define Sblock<%> 
  (interface
      
    ;; The interface Sblock<%> inherits from the interface SWidget<%>.
    ;; This means that any class that implements Sblock<%> must implement
    ;; all the methods from SWidget<%> plus all the methods defined here.
    (SWidget<%>)

    
    ;: -> ListOfSBlock
    ;; RETURNS: the teammates of this sblock
    get-team

    ;; ListOfSBlock -> Void
    ;;EFFECT: adds the given list of sblock to this block's team
    add-team

    ;; SBlock -> Void
    ;;EFFECT: adds the given sblock to this block's playground sblocks
    add-to-playground-sblocks

    ;add-teammate: SBlock -> Void
    ;EFFECT: adds the given sblock to this block's team
    add-teammate

    ;; -> Integer
    ;; RETURNS: the x or y coordinates of this sblock
    sblock-x 
    sblock-y

    ;; update-x-y-diff : NonNegInt NonNegInt -> Void
    ;; GIVEN    : the x and y coordinate of a point to update
    ;;            difference between center of block and given point
    ;; EFFECT   : Updates the difference between point and sblock center
    ;;            coordinates 
    update-x-y-diff

    ;; update-x-y-of-teammate : NonNegInt NonNegInt -> Void
    ;; GIVEN    : the x and y coordinate of a leader sblock 
    ;; EFFECT   : Updates the x and y coordinates
    ;;            of this sblock according to leader sblock.
    update-x-y-of-teammate
    
    ))
