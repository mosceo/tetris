;===========================================
;
;   TETRIS
;
;===========================================
;
; A full-fledged colorful tetris.
;
; Author:  Roman Kunin (mosceo@gmail.com)
; Source:  https://github.com/mosceo/tetris
; License: MIT
;
;==========================================

#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)


;=======================================
; Constants and definitions
;=======================================

(define W 5)          ; board width (# of blocks)
(define H 6)          ; board height (# of blocks)
(define PIX 50)       ; block size (pixels)
(define RATE (/ 1 4)) ; tick event inrerval (s)

(define-struct b [x y])
; A Block is (b Number Number)
; represents a block on the board by its coordinates
; example: (b 1 4) represents a block at (1, 4)


;=======================================
; PIECE: colors
;=======================================

(define P0-COLOR "pink")
(define P1-COLOR "gray")
(define PIECE-COLOR (list P0-COLOR P1-COLOR))


;=======================================
; PIECE: size, blocks
;=======================================

;; □ ■ □  □ ■ □  □ □ □  □ ■ □
;; ■ ■ ■  □ ■ ■  ■ ■ ■  ■ ■ □
;; □ □ □  □ ■ □  □ ■ □  □ ■ □

(define P0-SIZE 3)
(define P0-BLOCK (list (list (b 1 0) (b 0 1) (b 1 1) (b 2 1))
                       (list (b 1 0) (b 1 1) (b 2 1) (b 1 2))
                       (list (b 0 1) (b 1 1) (b 2 1) (b 1 2))
                       (list (b 1 0) (b 0 1) (b 1 1) (b 1 2))))

;; ■ ■
;; ■ ■

(define P1-SIZE 2)
(define P1-BLOCK (list (list (b 0 0) (b 1 0) (b 0 1) (b 1 1))))

;; ------------

(define PIECE-BLOCK (list P0-BLOCK P1-BLOCK))
(define PIECE-TYPE (list 4 1))


;=======================================
; Creating images
;=======================================

(define (block-image color)
  (define outline (square PIX 'outline "black"))
  (define sq (square PIX "solid" color))
  (overlay outline sq))


(define (board-place-image im x y scene)
  (underlay/xy scene (* x PIX) (* y PIX) im))


(define (place-block-color x y color scene)
  (define im (block-image color))
  (board-place-image im x y scene))


(define (place-block x y id scene)
  (define color (list-ref PIECE-COLOR id))
  (place-block-color x y color scene))


(define (place-blocks bls id scene)
  (foldl (lambda (bl im) (place-block (b-x bl) (b-y bl) id im))
         scene bls))


(define (place-block-b-color bl color scene)
  (define x (b-x bl))
  (define y (b-y bl))
  (place-block-color x y color scene))


;=======================================
; Images for pieces
;=======================================

;; [List-of B] ID Number -> Image
;; create a transparent square board and render given blocks on it
(define (blocks-on-square bls id size)
  (place-blocks bls id (square size "solid" "transparent")))


(define P0-IMAGE (list (blocks-on-square (list-ref P0-BLOCK 0) 0 P0-SIZE)
                       (blocks-on-square (list-ref P0-BLOCK 1) 0 P0-SIZE)
                       (blocks-on-square (list-ref P0-BLOCK 2) 0 P0-SIZE)
                       (blocks-on-square (list-ref P0-BLOCK 3) 0 P0-SIZE)))

(define P1-IMAGE (list (blocks-on-square (list-ref P1-BLOCK 0) 1 P1-SIZE)))

;; ------------

(define PIECE-IMAGE (list P0-IMAGE P1-IMAGE))


;=======================================
; Background
;=======================================

;; None -> Image 
;; create background image
(define (background-image)
  (define col1 (rectangle PIX (* PIX H) "solid" "LightGoldenrodYellow"))
  (define col2 (rectangle PIX (* PIX H) "solid" "PaleGoldenrod"))
  (define cols (build-list W (lambda (n) (if (= (modulo n 2) 0) col1 col2))))
  (apply beside cols))


(define BACKGROUND (background-image))


;=======================================
; Piece
;=======================================

(define-struct piece [id type x y])


(define (place-piece p scene)
  (board-place-image (piece-to-image p) (piece-x p) (piece-y p) scene))


(define (list-list-ref ll n m)
  (list-ref (list-ref ll n) m))


(define (piece-to-image p)
  (list-list-ref PIECE-IMAGE (piece-id p) (piece-type p)))



;; DEBUG
;;(place-piece (piece 0 3 2 3) BACKGROUND)



;=======================================
; Matrix
;=======================================

; color is either #f or a number in 0..5
(define-struct me [id] #:mutable #:transparent)

(define (e color)
  (make-me color))

(define F #f)


;; an example of a matrix
;; □ □ □ □ □
;; □ □ □ □ □
;; □ ■ ■ □ □
;; □ ■ □ □ □
;; □ ■ □ □ ■
(define matrix-ex-1 (list (list (e F) (e F) (e F) (e F) (e F))
                          (list (e F) (e F) (e F) (e F) (e F))
                          (list (e F) (e F) (e F) (e F) (e F))
                          (list (e F) (e 1) (e 1) (e F) (e F))
                          (list (e F) (e 1) (e F) (e F) (e F))
                          (list (e F) (e 1) (e F) (e F) (e 0))))





;; Any -> Boolean
;; check if it is an id
(define (id? x)
  (and (number? x) (>= x 0) (<= x 5)))

(check-equal? (id? #f) #f)
(check-equal? (id? -1) #f)
(check-equal? (id? 0) #t)
(check-equal? (id? 2) #t)
(check-equal? (id? 5) #t)


;; Matrix -> Image
;; render a matrix
(define (matrix-image m)
  (define im BACKGROUND)
  ;; [List-of [Maybe ID]] Number Image -> Image 
  (define (matrix-row-image row j)
    (for ([i W] [e row])
      (define id (me-id e))
      (when (id? id) (set! im (place-block i j id im)))))
  (for ([j H] [row m])
    (matrix-row-image row j))
  im)




;; Number Number -> Matrix
;; create an empty matrix
(define (create-matrix w h)
  (define (row _) (build-list w (lambda (_) (e #f))))
  (build-list h row))

;; Matrix Number Number -> Item
;; extract an item from matrix
(define (matrix-get m x y)
  (list-ref (list-ref m y) x))

;; Matrix Number Number ID
;; set a matrix entry
(define (matrix-set m x y id)
  (define item (matrix-get m x y))
  (set-me-id! item #t))


;=======================================
; Block and Board
;=======================================

;; check if a block can be added to a board
(define (block-put-matrix? bl m)
  (define entry (matrix-get m (b-x bl) (b-y bl)))
  (false? (me-id entry)))


;; check if a list of blocks can be added to a board
(define (block-put-matrix*? bls m)
  (andmap (lambda (bl) (block-put-matrix? bl m)) bls))



(define (piece-put-matrix? p m)
  (define bls (piece-to-blocks p))
  (block-put-matrix*? bls m))



(define (piece-blocks p)
  (define id (piece-id p))
  (define type (piece-type p))
  (list-list-ref PIECE-BLOCK id type))



(define (piece-to-blocks p)
  (define raw-bls (piece-blocks p))
  (define dx (piece-x p))
  (define dy (piece-y p))
  (define (shift bl) (b-shift bl dx dy))
  (map shift raw-bls))



(define (b-shift bl dx dy)
  (b (+ (b-x bl) dx)
     (+ (b-y bl) dy)))
  






(define p1 (piece 0 0 2 3))

(piece-put-matrix? p1 matrix-ex-1)

(place-piece p1 (matrix-image matrix-ex-1))








;=======================================
; Game
;=======================================



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


(define (game0)
  (make-game 8 12 (create-matrix 8 12) (random 6) 0 (random 6) 0 #t))






















