#lang racket
(require "interfaces2.rkt")
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(provide
 Sblock%:
 make-block
 )

;;; CONSTANTS

;; CANVAS
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define CANVAS-WIDTH-HALF (/ CANVAS-WIDTH 2))
(define CANVAS-HEIGHT-HALF (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sblock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-block: PosInt PosInt -> SBlock
;; GIVEN    : an x and a y position of a throbber
;; RETURNS  : an object representing a throbber at the given position.
;; EXAMPLES : see test cases
;; STRATEGY : combine simpler functions.
(define (make-block x y listOfSblock)
  (new Sblock%: [x x][y y]
       [playground-sblocks listOfSblock]))


;; Constructor template for Sblock%:
;; A SBlock is a (new Sblock%: [x Integer][y Integer]
;;                             [playground-sblocks ListOfSblock])
;; Interpretation : An object of class Sblock%: represents
;;                  an SBlock.
(define Sblock%:
  (class* object% (Sblock<%>)
    (init-field x y)                 ; x and y coordinate of sblock
    
    (init-field playground-sblocks)  ; number of sblocks in the playground not
    ; with teammates
    
    ; difference between sblock's x and mouse pointer's x coordinate.
    (init-field [sblock-mouse-x-diff -1])
    
    ; difference between sblock's y and mouse pointer's y coordinate.
    (init-field [sblock-mouse-y-diff -1]) 
    
    (field [side 20])                 ; the side of the sblock.
    (init-field [selected? false])   ; the states whether the sblock's is
    ; selected.
    (field [teammates empty])        ; all the sblocks that are teammates of
    ; this block
    
    
    ; (field  background [send world register this])
    
    ;; image for displaying the sblock when unselected
    (field [SBLOCK-UNSELECTED (square side "outline" "green")])
    
    ;; image for displaying the sblock when selected
    (field [SBLOCK-SELECTED (square side "outline" "red")])
    
    
    (super-new)
    
    ;; after-tick : -> Void
    ;; DETAILS : a sblock ignores after-tick
    (define/public (after-tick)
      1)
    
    ;; get-team : -> ListOfBlock
    ;; GIVEN   : no arguments.
    ;; RETURNS : list of teammates of this block.
    (define/public (get-team)
      teammates) 
    
    ;; remove-block-from-playground-blocks : Sblock -> void
    ;; EFFECT : remove the given sblock from the list of playground nodes
    ;; maintined by this node.
    (define/public (remove-block-from-playground-blocks sb)
      (begin
        (set! playground-sblocks 
              (filter 
               (lambda (b)
                 (not (equal? b sb))) playground-sblocks))
       ))
    
    ;; blocks-instersect? : Sblock -> Boolean
    ;; GIVEN    : a sblock
    ;; RETURNS  : true iff the current sblock intersects the given sblock.
    ;; STRATEGY : combine simpler functions.
    (define (blocks-instersect? sblock)
      (begin
        (and
         (<=  (abs (- x (send sblock sblock-x))) side)
         (<=  (abs (- y (send sblock sblock-y))) side))))
    
    
    ;; add-to-playground-sblocks : Sblock -> void
    ;; GIVEN    : a sblock
    ;; EFFECT   : adds the given sblock to playground-sblock list.
    (define/public (add-to-playground-sblocks sblock)
      (begin 
        (println "adding new sblock to background list")
        (set! playground-sblocks (cons sblock playground-sblocks))))
    
    
    ;; to-scene : Scene -> Scene
    ;; GIVEN    : a scene to paint the throbber.
    ;; RETURNS  : a scene like the given one, but with
    ;;            this thobber painted on it.
    ;; STRATEGY : Cases on whether the throbber is selected.
    (define/public (add-to-scene scene)
      (if selected?
          (place-image SBLOCK-SELECTED x y scene)
          (place-image SBLOCK-UNSELECTED x y scene)))
    
    
    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; GIVEN    : the location of a button-down event
    ;; EFFECT   : If the mouse pointer is inside SBlock,
    ;;            make the block selected.
    ;; STRATEGY : Cases on whether the mouse is within the sblock.
    (define/public (after-button-down mx my)
      (if (inside-sblock? x y mx my)
          (begin
            (set! sblock-mouse-x-diff (- x mx)) 
            (set! sblock-mouse-y-diff (- y my))
            (set! selected? true)
            (for-each 
             (lambda(b)
               (send b update-x-y-diff x y))
             teammates)
            this)
          this)) 
    
    
    ;; update-x-y-diff : NonNegInt NonNegInt -> Void
    ;; GIVEN    : the x and y coordinate of a point to update
    ;;            difference between center of block and given point
    ;; EFFECT   : Updates the difference between point and sblock center
    ;;            coordinates 
    (define/public (update-x-y-diff mx my)
      (begin
        
        (set! sblock-mouse-x-diff (- x mx)) 
        (set! sblock-mouse-y-diff (- y my)))
      
      ) 
    
    ;; after-drag : NonNegInt NonNegInt -> void
    ;; GIVEN    : the x and y coordinates of the mouse pointer.
    ;; EFFECT   : updates the location of slock and its teammate according to
    ;;            mouse pointer. Also checks for intersection between this block
    ;;            and its playground nodes.
    ;; STRATEGY : Divide into Cases on whether the throbber is selected.
    (define/public (after-drag mx my)
      (if selected? 
          (begin    
            
            (set! x (+ mx sblock-mouse-x-diff))
            (set! y (+ my sblock-mouse-y-diff))
            
            (for-each
             ;update the x,y of the teammates to move the team.
             (lambda (sblock)
               (send sblock update-x-y-of-teammate x y)) teammates)
            
            (check-playground-sblock-for-intersection) 

            (for-each
             ;update the mouse-x,y diff of the teammates.
              (lambda(b)
               (send b update-x-y-diff x y))
             
             teammates)
            
            )
          1))     
    
    
    ;; update-x-y-of-teammate : NonNegInt NonNegInt -> Void
    ;; GIVEN    : the x and y coordinate of a leader sblock 
    ;; EFFECT   : Updates the x and y coordinates
    ;;            of this sblock according to leader sblock.
    (define/public (update-x-y-of-teammate mx my)
      (set! x (+ mx sblock-mouse-x-diff))
      (set! y (+ my sblock-mouse-y-diff)))
    
    
    
    ;; add-team : ListOfSblock -> void
    ;; GIVEN  : no arguments
    ;; EFFECT : add the given list of sblock to the teammates without
    ;;          duplicates.
    (define/public (add-team lst)
      (set! teammates (remove this (set-union lst teammates))))

     ;; add-teammate : Sblock -> void
    ;; GIVEN  : no arguments
    ;; EFFECT : add the given  sblock to the teammates without
    ;;          duplicates.
    (define/public (add-teammate blk)
      (set! teammates (set-cons blk teammates)))

    
    
    ;; check-playground-sblock-for-intersection : -> void
    ;; GIVEN  : no arguments
    ;; EFFECT : checks if the current block intersects with any playground
    ;;          node. If true, swap their teams and removes each other from
    ;;          their playground nodes.
    (define (check-playground-sblock-for-intersection)
      (for-each
       (lambda (sblock)
         (if (blocks-instersect? sblock)
             (begin
               (swap-and-add-team sblock)
               (remove-block-from-playground-blocks sblock)
               (send sblock remove-block-from-playground-blocks this))
             "do-nothing")) 
       playground-sblocks))
    
    
    ;; swap-and-add-team : Sblock -> void
    ;; GIVEN  : a Sblock to swap team with.
    ;; EFFECT : take team of given sblock and add to this sblock
    ;; and each of its team members and vice-versa.
    (define (swap-and-add-team sblock)
      (begin
        (for-each
         (lambda (block)
           (send block add-team (set-cons sblock (send sblock get-team)))) 
         (cons this teammates))
        
        (for-each
         (lambda (block)
           (send block add-team (set-cons this teammates)))
         (cons sblock (send sblock get-team)))))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> void
    ;; GIVEN    : the x and y coordinates of mouse pointer.
    ;; Effect   : unselect the sblock.
    ;; STRATEGY : combine simpler functions
    (define/public (after-button-up mx my)
      (set! selected? false)) 
    
    
    ;; inside-sblock? : Integer Integer Integer Integer -> Boolean
    ;; GIVEN    : x and y coordinates of two points respectively.
    ;; RETURNS  : true iff the given coordinate is inside the given sblock.
    ;; EXAMPLES : see tests below
    ;; STRATEGY : Combine simpler function
    (define (inside-sblock? x y mx my)
      (and  
       (and (< mx (+ x (/ side 2))) (> mx (- x (/ side 2))))
       (and (< my (+ y (/ side 2))) (> my (- y (/ side 2))))))
    
    
    ;; after-key-event : KeyEvent -> void
    ;; DETAILS : a sblock ignores key events
    (define/public (after-key-event kev) this)
    
    
    ;; after-drag : NonNegInt NonNegInt -> Throbber<%>
    ;; GIVEN: the location of a move event
    ;; DETAILS: a sblock ignores after-move
    (define/public (after-move  mx my)
      this)
    
    
    ;; -> Int
    ;; RETURNS: the x position of the center of the sblock
    (define/public (sblock-x)
      x)
    
    ;; -> Int
    ;; RETURNS: the y position of the center of the sblock
    (define/public (sblock-y)
      y)
    
    ;; -> Int
    ;; RETURNS: the sblock-mouse-x-diff of the sblock
    (define/public (sblock-sblock-mouse-x-diff)
      sblock-mouse-x-diff)
    
    ;; -> Int
    ;; RETURNS: the sblock-mouse-y-diff of the sblock
    (define/public (sblock-sblock-mouse-y-diff)
      sblock-mouse-y-diff)
    
    ;; -> Boolean
    ;; RETURNS: the whether the block is selected 
    (define/public (sblock-selected)
      selected?)
    
    ;; For Test cases
    (define/public (for-test:explain-state)
      (list x y sblock-mouse-x-diff sblock-mouse-y-diff
            (get-playground-list playground-sblocks) selected?))
    
    ;; for test cases
    (define/public (get-playground-list playground-sblocks)
      (map  
       (lambda (blk)
         (list (send blk sblock-x)
               (send blk sblock-x)
               (send blk sblock-sblock-mouse-x-diff)
               (send blk sblock-sblock-mouse-y-diff)
               '()
               (send blk sblock-selected)))
       
       playground-sblocks))
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING FRAMEWORK - Sblock ;;;;;;;;;;;;;;;;

(begin-for-test
  (local
    ((define INITIAL-SBLOCK (make-block 100 100 empty))
     (define INITIAL-SBLOCK-1 (make-block 100 100 empty))
     (define SELECTED-BLOCK (new Sblock%: [x 20][y 30]
                                 [sblock-mouse-x-diff -1]
                                 [sblock-mouse-y-diff -1]
                                 [playground-sblocks empty] [selected? true]))
     (define UNSELECTED-BLOCK (new Sblock%: [x 20][y 30]
                                   [sblock-mouse-x-diff -1]
                                   [sblock-mouse-y-diff -1]
                                   [playground-sblocks empty]
                                   [selected? false]))
     (define DRAG_BLOCK (new Sblock%: [x 105][y 105]
                             [sblock-mouse-x-diff -1]
                             [sblock-mouse-y-diff -1]
                             [playground-sblocks (list INITIAL-SBLOCK-1)]
                             [selected? true]))
     (define ADDED-BLOCK (new Sblock%: [x 20][y 30]
                              [sblock-mouse-x-diff -1]
                              [sblock-mouse-y-diff -1]
                              [playground-sblocks (list INITIAL-SBLOCK-1)]
                              [selected? true]))
     
     (define drag-test-block (make-block 100 100 empty))
     
     (define block-with-playground-block
       (make-block 100 100 (list (make-block 300 300 empty))))
     
     
     )
    (send INITIAL-SBLOCK remove-block-from-playground-blocks SELECTED-BLOCK)
    (send SELECTED-BLOCK add-to-scene EMPTY-CANVAS)
    (send UNSELECTED-BLOCK add-to-scene EMPTY-CANVAS)
    
    
    (send block-with-playground-block after-button-down 100 100)
    (send block-with-playground-block after-drag 200 200)
    (check-equal? (send block-with-playground-block for-test:explain-state)
                  (list 200 200 0 0 (list (list 300 300 -1 -1 '() false)) true)
                  "returns incorrect config of sblock")
    
    
    (send ADDED-BLOCK add-to-playground-sblocks INITIAL-SBLOCK)
    
    (check-equal? (send INITIAL-SBLOCK sblock-x)
                  100
                  "returns incorrect x-coordinate of sblock")
    
    (check-equal? (send INITIAL-SBLOCK sblock-y)
                  100
                  "returns incorrect y-coordinate of sblock")
    
    
    (send drag-test-block add-team (list (make-block 100 100 empty)))
    (send drag-test-block after-button-down 100 100)
    (send drag-test-block after-drag 200 200)
    (check-equal? (send SELECTED-BLOCK sblock-selected)
                  true
                  "sblock must be selected")
    
    (send INITIAL-SBLOCK after-tick)
    (check-equal? (send INITIAL-SBLOCK for-test:explain-state)
                  (list 100 100 -1 -1 empty #f)
                  "returns parameters of INITIAL-SBLOCK after one tick")
    
    
    (check-equal? (send INITIAL-SBLOCK get-team)
                  empty
                  "returns teammates of sblock")
    
    (send INITIAL-SBLOCK after-button-down 100 100)
    (check-equal? (send INITIAL-SBLOCK for-test:explain-state)
                  (list 100 100 0 0 empty #true)
                  "returns parameters of INITIAL-SBLOCK after
                   mouse button-down event")
    
    (check-equal? (send INITIAL-SBLOCK sblock-sblock-mouse-x-diff)
                  0
                  "returns incorrecty mouse-block x difference")
    
    (check-equal? (send INITIAL-SBLOCK sblock-sblock-mouse-y-diff)
                  0
                  "returns incorrecty mouse-block y difference")
    
    
    (send UNSELECTED-BLOCK after-button-down 200 200)
    (check-equal? (send UNSELECTED-BLOCK for-test:explain-state)
                  (list 20 30 -1 -1 empty #f)
                  "returns parameters of UNSELECTED-BLOCK after mouse
                   button-down event")
    
    (send SELECTED-BLOCK after-drag 200 200)
    (check-equal? (send SELECTED-BLOCK for-test:explain-state)
                  (list 199 199 -1 -1 empty #t)
                  "returns parameters of SELECTED-BLOCK after mouse drag
                   event")
    
    (send UNSELECTED-BLOCK after-drag 200 200)
    (check-equal? (send UNSELECTED-BLOCK for-test:explain-state)
                  (list 20 30 -1 -1 empty #f)
                  "returns parameters of UNSELECTED-BLOCK after mouse drag
                   event")
    
    (send DRAG_BLOCK after-drag 110 110)
    (check-equal? (send DRAG_BLOCK get-team)
                  (list INITIAL-SBLOCK-1)
                  "returns parameters of DRAG_BLOCK after mouse drag event")
    
    (send SELECTED-BLOCK after-move 200 200)
    (check-equal? (send SELECTED-BLOCK for-test:explain-state)
                  (list 199 199 -1 -1 empty #t)
                  "returns parameters of SELECTED-BLOCK after mouse move
                    event")
    
    (send INITIAL-SBLOCK after-button-up 100 100)
    (check-equal? (send INITIAL-SBLOCK for-test:explain-state)
                  (list 100 100 0 0 empty #f)
                  "returns parameters of INITIAL-BLOCK after mouse move event")
    
    (send INITIAL-SBLOCK after-key-event "p")
    (check-equal? (send INITIAL-SBLOCK for-test:explain-state)
                  (list 100 100 0 0 empty #f)
                  "returns parameters of INITIAL-BLOCK after key event")
    
    
    
    
    ))





