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
; Helpers
;=======================================

(define (list-list-ref ll n m)
  (list-ref (list-ref ll n) m))


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
  (define bls (piece-to-valid-blocks p))
  (place-blocks bls (piece-id p) scene))



;; DEBUG
;;(place-piece (piece 0 3 2 3) BACKGROUND)



;=======================================
; Board
;=======================================

;;
;; Board is a matrix of [Maybe Number 0..5]
;;

; color is either #f or a number in 0..5
(define-struct entry [id] #:mutable #:transparent)

(define (e color)
  (make-entry color))

(define F #f)


;; an example of a board
;; □ □ □ □ □
;; □ □ □ □ □
;; □ ■ ■ □ □
;; □ ■ □ □ □
;; □ ■ □ □ ■
(define board-ex-1 (list (list (e F) (e F) (e F) (e F) (e F))
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


;; Board -> Image
;; render a board
(define (board-image m)
  (define im BACKGROUND)
  ;; [List-of [Maybe ID]] Number Image -> Image 
  (define (board-row-image row j)
    (for ([i W] [e row])
      (define id (entry-id e))
      (when (id? id) (set! im (place-block i j id im)))))
  (for ([j H] [row m])
    (board-row-image row j))
  im)




;; Number Number -> board
;; create an empty board
(define (create-board w h)
  (define (row _) (build-list w (lambda (_) (e #f))))
  (build-list h row))

;; board Number Number -> Item
;; extract an item from board
(define (board-get m x y)
  (list-ref (list-ref m y) x))

;; board Number Number ID
;; set a board entry
(define (board-set m x y id)
  (define item (board-get m x y))
  (set-entry-id! item #t))


;=======================================
; Block and Board
;=======================================

(define (board-taken? brd x y)
  (define entry (board-get brd x y))
  (false? (entry-id entry)))



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


(define (piece-to-valid-blocks p)
  (define bls (piece-to-blocks p))
  (filter (lambda (bl) (>= (b-y bl) 0)) bls))



(define (b-shift bl dx dy)
  (b (+ (b-x bl) dx)
     (+ (b-y bl) dy)))


;=======================================
; Piece moving
;=======================================

(define (piece-down p)
  (define new-y (add1 (piece-y p)))
  (struct-copy piece p
               [y new-y]))


(define (piece-left p)
  (define new-x (sub1 (piece-x p)))
  (struct-copy piece p
               [x new-x]))


(define (piece-right p)
  (define new-x (add1 (piece-x p)))
  (struct-copy piece p
               [x new-x]))


(define (piece-rotate p)
  (define id (piece-id p))
  (define type (piece-type p))
  (define type# (list-ref PIECE-TYPE id))
  (define new-type (modulo (add1 type) type#))
  (struct-copy piece p
               [type new-type]))


;=======================================
; Block, piece and board interaction
;=======================================

(define (block-inside? bl)
  (define x (b-x bl))
  (define y (b-y bl))
  (and (>= x 0) (< x W)
       (< y H)))


(define (block-above? bl)
  (define y (b-y bl))
  (< y 0))


(define (block-collision? bl brd)
  (define x (b-x bl))
  (define y (b-y bl))
  (not (board-taken? brd x y)))

;;--------------

(define (block-inside*? bls)
  (andmap block-inside? bls))


(define (block-above*? bls)
  (ormap block-above? bls))


(define (block-collision*? bls brd)
  (ormap (lambda (bl) (block-collision? bl brd))
          bls))

;;--------------

(define (piece-inside? p)
  (define bls (piece-to-blocks p))
  (block-inside*? bls))


(define (piece-above? p)
  (define bls (piece-to-blocks p))
  (block-above*? bls))


(define (piece-collision? p brd)
  (define bls (piece-to-blocks p))
  (block-collision*? bls brd))


(define (piece-valid? p brd)
  (and (piece-inside? p)
       (not (piece-collision? p brd))))


;;--------------



;; DEBUGGING
;(define p1 (piece 0 0 2 3))
;(piece-collision? p1 board-ex-1)
;(place-piece p1 (board-image board-ex-1))



;=======================================
; Game
;=======================================

(define-struct game [board piece next-piece
                     score active?])
; A Game is a structure:
;   (make-game Board Piece Piece
;              Number Boolean)
; represents a state of the game with a given board, a moving piece
; and a boolean which defines whether the game is over


;=======================================
; Game, low-level functions
;=======================================



(define (game-new-piece g)
  (define new-p (game-next-piece g))
  (define new-next-piece (random-piece))
  ;; - IN -
  (struct-copy game g
               [piece new-p]
               [next-piece new-next-p]))



(define (game-active-false g)
  (struct-copy game g
               [active #f]))



;=======================================
;=======================================
;=======================================
;=======================================
;=======================================
;=======================================


;=====
; API
;=====
;
; Window works with a Game object using this API
;
; (game-new)
; (game-active? g)
; (game-score   g)
;
; (game-left    g)
; (game-right   g)
; (game-rotate  g)
;
; (game-down-tick g)
; (game-down-key  g)
; (game-fall    g)    *


;; API
(define (game-new)
  (game [(board-new) (piece-new) (piece-new) 0 #t]))






(define (change-piece g fun)
  (define p (game-piece g))
  (define new-p (fun p))
  (define brd (game-board p))
  (if (piece-valid? new-p brd)
      (struct-copy game g [piece new-p]) g))

;; API
(define (game-left g)
  (change-piece g piece-left))

;; API
(define (game-right g)
  (change-piece g piece-right))

;; API
(define (game-rotate g)
  (change-piece g piece-rotate))









(define (game-move-down? g)
  (piece-valid? (piece-down (game-piece g))
                (game-board g)))













(define (game-down g rew)
  (cond [(game-move-down? g) (game-move-down g rew)]
        [else (game-land g)]))


  
  (define p (game-piece g))
  (define new-p (piece-down p))
  (define sc (game-score g))
  (define new-sc (+ sc rew))
  ;; - IN -
  (struct-copy game g
               [piece new-p]
               [score new-sc]))










(define (game-piece-above? g)
  (piece-above? (game-piece g)))







(define (game-land-piece g)
  (define p (game-piece g))
  (define brd (game-board g))
  (define new-brd (board-land-piece brd p))
  (define new-p (game-next-piece g))
  (define new-next-p (piece-new g))
  ;; - IN -
  (struct-copy game g
               [board new-brd]
               [piece new-p]
               [next-piece new-next-p]))



(define (game-over g)
  (struct-copy game g
               [active #f]))



(define (game-land g)
  (cond [(game-piece-above? g) (game-over g)]
        [else (game-land-piece g)]))



  


;; API
(define (game-down-tick g)
  (game-down g 1))


;; API
(define (game-down-key g)
  (game-down g 2))

  




;=======================================
; Window
;=======================================

(define-struct window [game])



























;=======================================
; Big-bang
;=======================================

(big-bang (window (game-new))
 [on-tick window-tick (/ 1 10)]
 [on-key  window-key]
 [to-draw window-draw])








