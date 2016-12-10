#lang racket

(require rackunit)
(require "interfaces.rkt")
(require 2htdp/image)
(require "extras.rkt")
(require 2htdp/universe)
(require "X-Controller.rkt")
(require "Model.rkt")
(provide World% make-world)


;; A World is a (make-world model) 

;; make-world : Model -> World
;; GIVEN  : a Model Object
;; Return : a world with given model and empty widget list.
;; Strategy : combine simpler functions.
(define (make-world m)
  (new World% [model m]))

;; A World% consists of a model and stateful widgets.  
;; It sends after-tick,mouse events and keyboard events,add-to-scene
;; to the model and to all controllers

(define World%
  (class* object%
    (SWidget<%>)

    (init-field model) ; model object. Refer class model for more details 
    (init-field [widgets empty])   ; list of SWidgets initialized with empty.

    (super-new)

    ;; after-tick : -> void
    ;; GIVEN  : no arguments
    ;; EFFECT : updates this widget to the
    ;;          state it should have following a tick.
     (define/public (after-tick)
      (send model after-tick)
      (for-each (lambda (wid) (send wid after-tick)) widgets))

    
    ;; to-scene : Scene -> Scene
    ;; GIVEN    : a scene.
    ;; RETURNS  : the same scene with all the widgets in this World
    ;;            painted on it.
    ;; STRATEGY : Use HOFC foldr on the SWidgets in this World
    (define/public (add-to-scene myscene)
      (foldr
        (lambda (wid base)
          (send wid add-to-scene base)) 
        myscene
        widgets))
    

    ;; after-mouse-event : NonNegInt NonNegInt MouseEvent -> Void
    ;; GIVEN   : two non negative integers indicating x and y position of 
    ;;           the mouse and a MouseEvent.
    ;; EFFECT  : distributes the mouse event to all the widgets.
    ;; STRATGY : Cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (after-button-down mx my)] 
        [(mouse=? mev "drag")
         (after-drag mx my)]
        [(mouse=? mev "button-up")
         (after-button-up mx my)]
        [else (after-move mx my)]))

    
    ;; after-button-down : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : distributes the after-button-down to all widgets in this.
    ;; STRATEGY : Use For-Each for distribution.
    (define/public (after-button-down mx my)
     (for-each
        ;;  SWidget -> SWidget
        ;;  Effect : the send the after after-button-down event to widget-wid.
        (lambda (wid)
          (send wid after-button-down mx my))
        widgets))
       
    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : distributes the after-button-up to all widgets in this.
    ;; STRATEGY : Use For-Each for distribution.
    (define/public (after-button-up mx my)
       (for-each
        ;; SWidget -> Void
        ;; Effect : the send the after-button-up to widget
        (lambda (wid) 
          (send wid after-button-up mx my))
        widgets))
     
    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : distributes the after-drag to all widgets in this.
    ;; STRATEGY : Use For-Each for distribution.
    (define/public (after-drag mx my)
      (for-each
        ;; SWidget -> Void
        ;; Effect : the send the after-drag to widget
        (lambda (wid) 
          (send wid after-drag mx my))
        widgets))
        
    
    ;; after-move : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : distributes the after-move to all widgets in this.
    ;; STRATEGY : Use For-Each for distribution.
    (define/public (after-move mx my)
      (for-each
        ;; SWidget -> Void
        ;; Effect : the send the after-move to widget
        (lambda (wid) 
          (send wid after-move mx my)) 
        widgets))

        
    ;; after-key-event : KeyEvent -> Void
    ;; EFFECT   : distributes the after-key-event to all widgets in this.
    ;; STRATEGY : Use For-Each for distribution.
    (define/public (after-key-event kev)
     (for-each
        ;; SWidget -> Void
        ;; Effect : the send the after-key-event to widget
        (lambda (wid) 
          (send wid after-key-event kev)) 
        widgets))

    ;; add-widget : SWidget -> Void
    ;; EFFECT   : adds the given widget to list of widgets.
    (define/public (add-widget c)
      (set! widgets (cons c widgets)))

    ; for testing 
    (define/public (for-test:widgets)
      widgets)
    
 
    ))

;;*************************TESTING *******************************

(define m1 (new Model%))
(define xc (new X-Controller% [model m1]))
(define w (new World% [model m1]))
;; TESTS

(begin-for-test
(check-equal?
 (begin
   (send w add-widget xc)
   (length (send w for-test:widgets)))
  1
  "Widget is not added properly"))


