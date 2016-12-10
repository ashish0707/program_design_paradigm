;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname probset06q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
        initial-world
        run
        world-after-mouse-event
        world-after-key-event
        world-to-trees
        tree-to-root
        tree-to-sons
        node-to-center
        node-to-selected?)
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; dimensions of the canvas
(define CANVAS-WIDTH 500)
(define INITIAL-VEL 0)
(define INITIAL-MOUSE-X -1)
(define INITIAL-MOUSE-Y -1)
(define CANVAS-HEIGHT 400)
(define TEXT_HEIGHT 10)
(define CIRCLE-RADIUS 20)
(define SIMLPE-CIRCLE (circle CIRCLE-RADIUS "outline" "green"))
(define SELECTED-CIRCLE (circle CIRCLE-RADIUS "solid" "green"))
(define SQUARE-RADIUS 20)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define SIMPLE-SQUARE (square SQUARE-RADIUS "outline" "green"))
(define SELECTED-CIRCLE (circle CIRCLE-RADIUS "solid" "green"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFINITIONS FOR KEY EVENTS
(define delete-node-event "d")
(define new-cicle-key-event "c")
(define new-square-key-event "s")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct leaf (x y))
(define-struct node position (lson rson))

;; A Tree is either
;; -- (make-leaf Number)
;; -- (make-node Tree Tree) 

;; Template:
;; tree-fn : Tree -> ???
;; (define (tree-fn t)
;;   (cond
;;     [(leaf? t) (... (leaf-datum t))]
;;     [else (...
;;             (tree-fn (node-lson t))
;;             (tree-fn (node-rson t)))]))

;; tree-fold  
;;  : (X X -> X) (Number -> X) Tree -> X
;; Strategy: Use template for Tree on t
(define (tree-fold combiner base t)
  (cond
    [(leaf? t) (base (leaf-x t) (leaf-y t))]
    [else (combiner
            (tree-fold combiner base 
              (node-lson t))
            (tree-fold combiner base 
              (node-rson t)))]))



;; number-of-nodes : Tree -> Number
;; RETURNS: the number of nodes in the given tree.
;; Strategy: Use HOF tree-fold on t
(define (number-of-nodes t)
  (tree-fold
    ;; Number Number -> Number
    (lambda (ans-for-lson ans-for-rson)
      (+ 1 ans-for-lson ans-for-rson))
    (lambda (num) 1)
    t))

;; increment-all : Tree -> Tree
;; RETURNS: a tree just like the original, but in which all of the
;; leaves have contents one more  than in the original.
;; STRATEGY: Use HOF tree-fold on t
(define (increment-all t newx newy)
  (tree-fold
    ;; Number Number -> Number
    (lambda (ans-for-lson ans-for-rson)
      (make-node ans-for-lson ans-for-rson))
    (lambda (x y) (make-leaf (+ x newx) (+ y newy)))
    t))

(define tree1 
  (make-node
    (make-node 
      (make-leaf 3 5)
      (make-leaf 14 16))
    (make-node
      (make-leaf 15 17)
      (make-node 
        (make-leaf 9 11)
        (make-leaf 26 28)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
