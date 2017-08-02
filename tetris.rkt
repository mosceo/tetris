;;===========================================
;;
;;   TETRIS
;;
;;===========================================
;;
;; A full-fledged colorful tetris.
;;
;; Author:  Roman Kunin (mosceo@gmail.com)
;; Source:  https://github.com/mosceo/tetris
;; License: MIT
;;
;;==========================================

#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)


;=======================================
; Global constants
;=======================================

(define W 5)          ; board width (# of blocks)
(define H 6)          ; board height (# of blocks)
(define PIX 50)       ; block size (pixels)
(define RATE 1.0)     ; tick event inrerval (s)
(define PIECE# 2)     ; number of pieces


;;=======================================
;; Helpers
;;=======================================

(define F #f)

(define (matrix-ref mat row col)
  (list-ref (list-ref mat row) col))

(define (id? x)
  (and (number? x) (>= x 0) (< x PIECE#)))

(check-false (id? #f))
(check-false (id? 1000))
(check-true (id? 0))
(check-true (id? 1))


;;=======================================
;; Block
;;=======================================

(define-struct block [x y] #:transparent)

(define (b x y)
  (block x y))


;;
;; API
;;
;; block-shift
;; block-shift*
;; block-inside?
;; block-inside*?
;; block-above?
;; block-above*?
;; block-visible?
;; block-visible*?
;;

;; NOTE: the meaning of function that work with lists differ,
;;       block-inside*? needs all blocks be inside,
;;       when block-above*? needs only one block be above

(define (block-shift b dx dy)
  (block (+ (block-x b) dx)
         (+ (block-y b) dy)))


(define (block-shift* bs dx dy)
  (map (lambda (b) (block-shift b dx dy)) bs))


(define (block-inside? b)
  (define x (block-x b))
  (define y (block-y b))
  (and (>= x 0) (< x W)
       (< y H)))


(define (block-inside*? bs)
  (andmap block-inside? bs))


(define (block-above? b)
  (< (block-y b) 0))


(define (block-above*? bs)
  (ormap block-above? bs))


(define (block-visible? b)
  (and (block-inside? b)
       (not (block-above? b))))


(define (block-visible*? bs)
  (andmap block-visible? bs))


;;
;; Unit tests
;;

(check-equal? (block-shift (b 1 2) 3 -2) (b 4 0))

(check-equal? (block-shift* (list (b 0 1) (b 2 5)) 1 -2) (list (b 1 -1) (b 3 3)))

(check-true (block-inside? (b 0 0)))
(check-true (block-inside? (b 0 -5)))
(check-false (block-inside? (b -1 0)))
(check-false (block-inside? (b 0 1000)))

(check-true (block-inside*? (list (b 0 0) (b 0 -1) (b 1 2))))
(check-false (block-inside*? (list (b 0 0) (b 0 -1) (b -1 2))))

(check-true (block-above? (b 0 -1)))
(check-false (block-above? (b 0 0)))

(check-true (block-above*? (list (b 0 0) (b 0 1) (b 0 -1))))
(check-false (block-above*? (list (b 0 0) (b 0 1) (b 1 2))))

(check-true (block-visible? (b 0 0)))
(check-false (block-visible? (b 0 -1)))
(check-false (block-visible? (b -1 0)))

(check-true (block-visible*? (list (b 0 0) (b 0 1) (b 1 2))))
(check-false (block-visible*? (list (b 0 0) (b 0 1) (b 0 1000))))


;;=======================================
;; Game pieces
;;=======================================

;; Piece 0
;;
;; □ ■ □  □ ■ □  □ □ □  □ ■ □
;; ■ ■ ■  □ ■ ■  ■ ■ ■  ■ ■ □
;; □ □ □  □ ■ □  □ ■ □  □ ■ □

(define P0-COLOR "pink")
(define P0-SIZE 3)
(define P0-TYPE# 4)
(define P0-BLOCK (list (list (b 1 0) (b 0 1) (b 1 1) (b 2 1))
                       (list (b 1 0) (b 1 1) (b 2 1) (b 1 2))
                       (list (b 0 1) (b 1 1) (b 2 1) (b 1 2))
                       (list (b 1 0) (b 0 1) (b 1 1) (b 1 2))))

;; Piece 1
;;
;; ■ ■
;; ■ ■

(define P1-COLOR "gray")
(define P1-SIZE 2)
(define P1-TYPE# 1)
(define P1-BLOCK (list (list (b 0 0) (b 1 0) (b 0 1) (b 1 1))))


;;
;; All data in one place
;;

(define PIECE-COLOR (list P0-COLOR P1-COLOR))
(define PIECE-BLOCK (list P0-BLOCK P1-BLOCK))
(define PIECE-TYPE# (list P0-TYPE# P1-TYPE#))


;;
;; API
;;
;; global-piece-color
;; global-piece-blocks
;; global-piece-type#
;;

(define (global-piece-color id)
  (list-ref PIECE-COLOR id))


(define (global-piece-blocks id type)
  (matrix-ref PIECE-BLOCK id type))


(define (global-piece-type# id)
  (list-ref PIECE-TYPE# id))


;;
;; Unit tests
;;

(check-equal? (global-piece-color 1) P1-COLOR)

(check-equal? (global-piece-blocks 0 2) (list (b 0 1) (b 1 1) (b 2 1) (b 1 2)))

(check-equal? (global-piece-type# 1) 1)


;;=======================================
;; Piece
;;=======================================

(define-struct piece [id type x y] #:transparent)


;;
;; API
;;
;; piece-new
;; piece-left
;; piece-right
;; piece-down
;; piece-rotate
;;
;; piece-inside?
;; piece-above?
;;
;; piece->blocks
;; piece->visible-blocks
;;

(define (piece-new)
  (define id (random PIECE#))
  (define type# (global-piece-type# id))
  (define type (random type#))
  (piece id type 0 0))


(define (piece-left p)
  (define new-x (sub1 (piece-x p)))
  (struct-copy piece p [x new-x]))


(define (piece-right p)
  (define new-x (add1 (piece-x p)))
  (struct-copy piece p [x new-x]))


(define (piece-down p)
  (define new-y (add1 (piece-y p)))
  (struct-copy piece p [y new-y]))


(define (piece-rotate p)
  (define id (piece-id p))
  (define type (piece-type p))
  (define type# (global-piece-type# id))
  (define new-type (modulo (add1 type) type#))
  (struct-copy piece p [type new-type]))


(define (piece-inside? p)
  (block-inside*? (piece->blocks p)))


(define (piece-above? p)
  (block-above*? (piece->blocks p)))


(define (piece->blocks p)
  (define raw-bs (piece->raw-blocks p))
  (block-shift* raw-bs (piece-x p) (piece-y p)))


(define (piece->visible-blocks p)
  (define bs (piece->blocks p))
  (filter block-visible? bs))


;;
;; Lower-level routines
;;

(define (piece->raw-blocks p)
  (global-piece-blocks (piece-id p) (piece-type p)))


;;
;; Unit tests
;;

(check-pred piece? (piece-new))

(check-equal? (piece-left (piece 0 0 2 5)) (piece 0 0 1 5))

(check-equal? (piece-right (piece 0 0 2 5)) (piece 0 0 3 5))

(check-equal? (piece-down (piece 0 0 2 5)) (piece 0 0 2 6))

(check-equal? (piece-rotate (piece 0 0 2 5)) (piece 0 1 2 5))
(check-equal? (piece-rotate (piece 0 3 2 5)) (piece 0 0 2 5))
(check-equal? (piece-rotate (piece 1 0 2 5)) (piece 1 0 2 5))

(check-true (piece-inside? (piece 0 0 0 0)))
(check-true (piece-inside? (piece 0 1 -1 -2)))
(check-false (piece-inside? (piece 0 1 -2 -2)))

(check-false (piece-above? (piece 0 2 0 -1)))
(check-true (piece-above? (piece 0 2 0 -2)))

(check-equal? (piece->blocks (piece 0 2 3 5)) (list (b 3 6) (b 4 6) (b 5 6) (b 4 7)))
(check-equal? (piece->blocks (piece 1 0 3 5)) (list (b 3 5) (b 4 5) (b 3 6) (b 4 6)))


;;=======================================
;; Entry
;;=======================================

(define-struct entry [id] #:mutable #:transparent)


;;
;; API
;;
;; e
;; ef
;; entry-taken?
;; entry-set
;;

(define (e id)
  (entry id))


(define (ef)
  (e #f))


(define (entry-taken? e)
  (false? (entry-id entry)))


(define (entry-set e id)
  (set-entry-id! e id))


;;
;; Unit tests
;;

(check-pred entry? (e 1))
(check-pred entry? (ef))
(check-equal? (entry-id (e 1)) 1)
(check-equal? (entry-id (ef)) #f)


;;;=======================================
;;; Row
;;;=======================================
;
;(define-struct entry [id] #:mutable #:transparent)
;
;(define (e id)
;  (entry id))
;
;(define (ef)
;  (e #f))
;
;;; Example: (cons 0 (list (e F) (e 1) (e 2) (e F) (e F)))
;
;
;;;
;;; API
;;;
;;; row-new
;;; rows-new
;;; rows-replenish
;;; rows-remove-full
;;;
;
;
;(define (row-new)
;  (build-list W ef))
;
;
;(define (rows-new n)
;  (build-list n row-new))
;
;
;(define (rows-replenish rows)
;  (define size (length rows))
;  (define lack (- H size))
;  (append (rows-new lack) rows))
;
;
;(define (rows-remove-full rows)
;  (filter rows row-not-full?))
;
;
;(define (row-full? row)
;  (= (car row) W))
;
;
;(define (row-not-full? row)
;  (not (row-full? row))
;
;
;;;=======================================
;;; Board
;;;=======================================
;
;;; EXAMPLE #1
;;;
;;; □ □ □ □ □
;;; □ □ □ □ □
;;; □ ■ ■ □ □
;;; □ ■ □ □ □
;;; □ ■ □ □ ■
;
;(define board-ex-1 (list (cons 0 (list (e F) (e F) (e F) (e F) (e F)))
;                         (cons 0 (list (e F) (e F) (e F) (e F) (e F)))
;                         (cons 0 (list (e F) (e F) (e F) (e F) (e F)))
;                         (cons 2 (list (e F) (e 1) (e 1) (e F) (e F)))
;                         (cons 1 (list (e F) (e 1) (e F) (e F) (e F)))
;                         (cons 2 (list (e F) (e 1) (e F) (e F) (e 0)))))
;
;
;;;
;;; API
;;;
;;; board-new
;;; board-piece?
;;; board-land
;;; board-remove-full
;;;
;
;
;(define (board-new)
;  (rows-new H))
;
;
;(define (board-piece? brd p)
;  (and (piece-inside? p)
;       (not (board-piece-collision? brd p))))
;
;
;(define (board-land brd p)
;  (define id (piece-id p))
;  (define bs (piece->visible-blocks p))
;  (for ([b bs])
;    (board-land-block brd b id))
;  brd)
;
;
;(define (board-remove-full brd)
;  (rows-replenish (rows-remove-full brd)))
;
;
;;;
;;; Lower-level routines
;;;
;  
;
;(define (board-piece-collision? brd p)
;  (define bs (piece->visible-blocks p))
;  (board-block-collision*? brd bs))
;
;
;(define (board-block-collision*? brd bs)
;  (ormap (lambda (b) (board-block-collision? b brd)) bs))
;
;
;(define (board-block-collision? brd b)
;  (not (board-taken? brd (block-x b) (block-y b))))
;
;
;(define (board-taken? brd x y)
;  (define e (board-get brd x y))
;  (entry-taken? e))
;
;
;(define (board-land-block brd b id)
;  (board-set brd (block-x b) (block-y b) id))
;
;
;(define (board-get brd x y)
;  (define row (list-ref brd y))
;  (define entries (cdr row))
;  (list-ref entries x))
;
;
;(define (board-set brd x y id)
;  (define e (board-get brd x y))
;  (entry-set e id))
;
;
;;;=======================================
;;; Game
;;;=======================================
;
;(define-struct game [board piece next-piece
;                     score active?])
;
;
;;;
;;; API
;;;
;;; game-new
;;; game-active?
;;; game-score
;;;
;;; game-left
;;; game-right
;;; game-rotate
;;;
;;; game-down-tick
;;; game-down-key
;;; game-fall *
;;;
;
;
;(define (game-new)
;  (game [(board-new) (piece-new) (piece-new) 0 #t]))
;
;
;(define (game-left g)
;  (change-piece g piece-left))
;
;
;(define (game-right g)
;  (change-piece g piece-right))
;
;
;(define (game-rotate g)
;  (change-piece g piece-rotate))
;
;
;(define (game-down-tick g)
;  (game-down g 1))
;
;
;(define (game-down-key g)
;  (game-down g 2))
;
;
;;;
;;; Lower-level routines
;;;
;
;
;;; Game Function -> Game
;(define (change-piece g fn)
;  (define p (game-piece g))
;  (define new-p (fn p))
;  (define brd (game-board p))
;  (if (piece-valid? new-p brd)
;      (struct-copy game g [piece new-p]) g))
;
;
;(define (game-down g rew)
;  (cond [(game-move-down? g) (game-move-down g rew)]
;        [else (game-land g)]))
;
;
;(define (game-move-down? g rew)
;  (board-piece-valid? (piece-down (game-piece g))
;                      (game-board g)))
;
;
;(define (game-move-down g rew)
;  (define p (game-piece g))
;  (define new-p (piece-down p))
;  (define sc (game-score g))
;  (define new-sc (+ sc rew))
;  ;; - IN -
;  (struct-copy game g
;               [piece new-p]
;               [score new-sc]))
;
;
;(define (game-land g)
;  (cond [(game-piece-above? g) (game-over g)]
;        [else (game-land-piece g)]))
;
;
;(define (game-land-piece g)
;  (define p (game-piece g))
;  (define brd (game-board g))
;  (define new-brd (board-land brd p))
;  (define new-p (game-next-piece g))
;  (define new-next-p (piece-new g))
;  ;; - IN -
;  (struct-copy game g
;               [board new-brd]
;               [piece new-p]
;               [next-piece new-next-p]))
;
;
;(define (game-piece-above? g)
;  (piece-above? (game-piece g)))
;
;
;(define (game-over g)
;  (struct-copy game g [active #f]))
;
;
;;=======================================
;; Window
;;=======================================
;
;(define-struct window [game main-counter counter th ])
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;;=======================================
;; Big-bang
;;=======================================
;
;(big-bang (window (game-new))
; [on-tick window-tick (/ 1 10)]
; [on-key  window-key]
; [to-draw window-draw])








