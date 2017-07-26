#lang racket
;=======================================
;
;    TETRIS
;
;=======================================
; A full-fledged tetris with colors.
;
; Author: Roman Kunin (mosceo@gmail.com)
; Source: https://github.com/mosceo/tetris
;=======================================

 (require 2htdp/image)




; size of one block in pixels
(define PIX 50)
; how oftet a tick event fires (in sec)
(define RATE 0.25)




(define COLORS (list "pink" "gray"))


(define P1-COLOR "pink")
(define P2-COLOR "gray")



(define-struct block [x y])




(define (block-image color)
  (define outline (rectangle PIX PIX 'outline "black"))
  (define b-im (square PIX "solid" color))
  (overlay outline b-im))



(define (place-block b color scene)
  (define x (block-x b))
  (define y (block-y b))
  (define b-im (block-image color))
  (place-image/align b-im (* x PIX) (* y PIX) "left" "top" scene))




(define CANVAS (square 300 "solid" "yellow"))



;(define CANVAS2 (place-block (make-block 2 1) "pink" CANVAS))
;(place-block (make-block 3 1) "pink" CANVAS2)










;=======================================
; Game
;=======================================

;--------
; Block ;
;--------

; A Block is a Cons
; a block on the board with certain board coordinates
; example: (cons 0 4) represents a block at (0, 4)

; this wrapper will help create blocks easier
(define (b x y)
  (make-block x y))


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

(define-struct game [width height board piece next-piece score active?])
; A Game is a structure:
;   (make-game Board GamePiece Boolean)
; represents a state of the game with a given board, a moving piece
; and a boolean which defines whether the game is over





;=======================================
; Pieces
;=======================================



(define (image-for-piece blocks size color)
  (define pixels (+ 2 (* PIX size))) ;; make it a bit wider to contain the borders
  (define scene (rectangle pixels pixels "solid" (make-color 100 100 100 0)))
  (define im1 (place-block (list-ref blocks 0) color scene))
  (define im2 (place-block (list-ref blocks 1) color im1))
  (define im3 (place-block (list-ref blocks 2) color im2))
  (define im4 (place-block (list-ref blocks 3) color im3))
  im4)
  



; □ ■ □  □ ■ □  □ □ □  □ ■ □
; ■ ■ ■  □ ■ ■  ■ ■ ■  ■ ■ □
; □ □ □  □ ■ □  □ ■ □  □ ■ □
(define P1-SIZE 3)

(define P1-BLOCKS (list (list (b 1 0) (b 0 1) (b 1 1) (b 2 1))
                        (list (b 1 0) (b 1 1) (b 2 1) (b 1 2))
                        (list (b 0 1) (b 1 1) (b 2 1) (b 1 2))
                        (list (b 1 0) (b 0 1) (b 1 1) (b 0 2))))


(image-for-piece (first P1-BLOCKS) 3 "pink")


;(define P1-IMAGES (list (image-for-piece (list-ref P1-BLOCKS 0) P1-SIZE P1-COLOR)
;                        (image-for-piece (list-ref P1-BLOCKS 1) P1-SIZE P1-COLOR)
;                        (image-for-piece (list-ref P1-BLOCKS 2) P1-SIZE P1-COLOR)
;                        (image-for-piece (list-ref P1-BLOCKS 3) P1-SIZE P1-COLOR)))

;(define PIECE1 (make-piece P1-BLOCKS P1-IMAGES))


; ■ ■
; ■ ■





;(define PIECES (list PIECE1 PIECE1 PIECE1 PIECE1 PIECE1 PIECE1 PIECE1))




