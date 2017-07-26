#lang racket
;=======================================
; TETRIS
;=======================================
; A full-fledged tetris with colors.
;
; Author: Roman Kunin (mosceo@gmail.com)
; Source: https://github.com/mosceo/tetris

 (require 2htdp/image)




; size of one block in pixels
(define W 50)

; how oftet a tick event fires (in sec)
(define RATE 0.25)

(define (border image)
  (define width (image-width image))
  (define height (image-height image))
  (define rect (rectangle width height 'outline "black"))
  (overlay rect image))



(define BLOCK (border (square W "solid" "yellow")))


(define (place-block x y scene)
  (place-image/align BLOCK (* x W) (* y W) "left" "top" scene))



;=======================================
; Data definitions
;=======================================

;--------
; Block ;
;--------

; A Block is a Cons
; a block on the board with certain board coordinates
; example: (cons 0 4) represents a block at (0, 4)

; this wrapper will help create blocks easier
(define (b x y)
  (cons x y))


;--------
; Board ;
;--------

; A Row is a List-of-Numbers
; a list of x-coordinates that are occupied by blocks at that row
; example: (list 3 1 7)

; A Board is a List-of-Rows
; a list of rows represents a board, with row 0 being the topmost
; example: (list (list)
;                (list 0 3)
;                (list 5 2 1))
; this data structure represents a board of height 3, with no blocks
; at row 0, two blocks at row 1 and three blocks at row 2


;-------
; Game ;
;-------

(define-struct game [board piece next-piece score active?])
; A Game is a structure:
;   (make-game Board GamePiece Boolean)
; represents a state of the game with a given board, a moving piece
; and a boolean which defines whether the game is over




(define IMAGE-1 'dummy)
(define IMAGE-2 'dummy)
(define IMAGE-3 'dummy)
(define IMAGE-4 'dummy)

;=======================================
; Pieces
;=======================================

; □ ■ □  □ ■ □  □ □ □  □ ■ □
; ■ ■ ■  □ ■ ■  ■ ■ ■  ■ ■ □
; □ □ □  □ ■ □  □ ■ □  □ ■ □
(define PIECE1 (list (list (list (b 1 0) (b 0 1) (b 1 1) (b 2 1)) IMAGE-1)
                     (list (list (b 1 0) (b 1 1) (b 2 1) (b 1 2)) IMAGE-2)
                     (list (list (b 0 1) (b 1 1) (b 2 1) (b 1 2)) IMAGE-3)
                     (list (list (b 1 0) (b 0 1) (b 1 1) (b 0 2)) IMAGE-4)))
; □ □ ■ □  □ □ □ □
; □ □ ■ □  ■ ■ ■ ■
; □ □ ■ □  □ □ □ □
; □ □ ■ □  □ □ □ □



; ■ ■
; ■ ■



; □ ■ ■  ■ □ □
; ■ ■ □  ■ ■ □
; □ □ □  □ ■ □


(define PIECES (list PIECE1 PIECE1 PIECE1 PIECE1 PIECE1 PIECE1 PIECE1))








;(empty-scene 300 300 (make-color 255 0 255 100))

(define SCENE1 (square 150 150 (make-color 255 0 255 0)))
(define SCENE2 (place-image BLOCK 75 25 SCENE1))
(define SCENE3 (place-image BLOCK 75 75 SCENE2))
(define SCENE4 (place-image BLOCK 75 125 SCENE3))
(define SCENE5 (place-image BLOCK 25 125 SCENE4))

(define PIECE SCENE5)




(define CANVAS (rectangle 500 300 "solid" "pink"))


(define CANVAS2 (place-block 0 0 CANVAS))
(define CANVAS3 (place-block 0 1 CANVAS2))
(define CANVAS4 (place-block 1 1 CANVAS3))
(define CANVAS5 (place-block 2 1 CANVAS4))


CANVAS5


(beside BLOCK BLOCK BLOCK)









