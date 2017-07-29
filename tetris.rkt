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
(require 2htdp/universe)
(require rackunit)


;=======================================
; Constants and definitions
;=======================================

(define PIX 50)         ; size of one block in pixels
(define W 5)
(define H 6)
(define RATE (/ 1 4))   ; how often tick events fire


(define-struct b [x y])
; A B (Block) is ...
; a block on the board with certain board coordinates
; example: (cons 0 4) represents a block at (0, 4)


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
(define PIECE-TYPE# (list 4 1))


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


(define (place-block x y colid scene)
  (define color (list-ref PIECE-COLOR colid))
  (place-block-color x y color scene))


(define (place-block-b-color bl color scene)
  (define x (b-x bl))
  (define y (b-y bl))
  (place-block-color x y color scene))


;=======================================
; Images for pieces
;=======================================






(define (image-for-piece blocks size color)
  (define pixels (+ 2 (* PIX size))) ;; make it a bit wider to contain the borders
  (define scene (rectangle pixels pixels "solid" (make-color 100 100 100 0)))
  (define im1 (place-block-b-color (list-ref blocks 0) color scene))
  (define im2 (place-block-b-color (list-ref blocks 1) color im1))
  (define im3 (place-block-b-color (list-ref blocks 2) color im2))
  (define im4 (place-block-b-color (list-ref blocks 3) color im3))
  im4)







(define P0-IMAGE (map (lambda (bs) (image-for-piece bs P0-SIZE P0-COLOR))
                       P0-BLOCK))


(define P1-IMAGE (map (lambda (bs) (image-for-piece bs P1-SIZE P1-COLOR))
                       P1-BLOCK))


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

;(define-struct posn [x y])

(define-struct piece [id type x y])

(define (place-piece p scene)
  (board-place-image (piece-to-image p) (piece-x p) (piece-y p) scene))



(define (piece-to-image p)
  (define id (piece-id p))
  (define type (piece-type p))
  (list-ref (list-ref PIECE-IMAGE id) type))


(place-piece (piece 0 0 2 1) BACKGROUND)




;=======================================
; Matrix
;=======================================

; color is either #f or a number in 0..5
(define-struct me [id] #:mutable #:transparent)

(define (e color)
  (make-me color))

(define F #f)

;; an example of a matrix
(define matrix (list (list (e 0) (e F) (e F) (e F) (e F))
                     (list (e 0) (e 1) (e F) (e F) (e F))
                     (list (e 0) (e 1) (e 0) (e F) (e F))
                     (list (e 0) (e 1) (e 0) (e 1) (e F))
                     (list (e F) (e F) (e F) (e F) (e 0))
                     (list (e F) (e F) (e F) (e 1) (e 0))))




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






















