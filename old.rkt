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
; Global constants
;=======================================

(define W 5)          ; board width (# of blocks)
(define H 6)          ; board height (# of blocks)
(define PIX 50)       ; block size (pixels)
(define RATE 1.0)     ; tick event inrerval (s)
(define PIECE# 2)     ; number of pieces


;=======================================
; Helpers
;=======================================

(define F #f)

(define (matrix-ref mat row col)
  (list-ref (list-ref mat row ) col))


;; Any -> Boolean
;; check if it is an id
(define (id? x)
  (and (number? x) (>= x 0) (< x PIECE#)))

(check-equal? (id? #f) #f)
(check-equal? (id? -1) #f)
(check-equal? (id? 0) #t)
(check-equal? (id? 1) #t)




;;=======================================
;; Block
;;=======================================

(define-struct block [x y])

(define (b x y)
  (block x y))


;;
;; API
;;
;; block-shift
;; block-shift*
;; block-inside?
;; block-above?
;; block-inside*?
;; block-above*?
;;


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


(define (block-above? b)
  (< (block-y b) 0))


(define (block-visible? b)
  (and (block-inside? b)
       (not (block-above? b))))


(define (block-inside*? bls)
  (andmap block-inside? bls))


(define (block-above*? bls)
  (ormap block-above? bls))



;;=======================================
;; Piece
;;=======================================

(define-struct piece [id type x y])

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
  (define type# (list-ref PIECE-TYPE id))
  (define type (random type#))
  (piece id type 0 0))


(define (piece-left p)
  (define new-x (sub1 (piece-x p)))
  (struct-copy piece p
               [x new-x]))


(define (piece-right p)
  (define new-x (add1 (piece-x p)))
  (struct-copy piece p
               [x new-x]))


(define (piece-down p)
  (define new-y (add1 (piece-y p)))
  (struct-copy piece p
               [y new-y]))


(define (piece-rotate p)
  (define id (piece-id p))
  (define type (piece-type p))
  (define type# (list-ref PIECE-TYPE id))
  (define new-type (modulo (add1 type) type#))
  (struct-copy piece p
               [type new-type]))


(define (piece-inside? p)
  (define bls (piece-to-blocks p))
  (block-inside*? bls))


(define (piece-above? p)
  (define bls (piece-to-blocks p))
  (block-above*? bls))


(define (piece->raw-blocks p)
  (define id (piece-id p))
  (define type (piece-type p))
  (matrix-ref PIECE-BLOCK id type))


(define (piece->blocks p)
  (define raw-bs (piece->raw-blocks p))
  (block-shift* raw-bs (piece-x p) (piece-y p)))


(define (piece->visible-blocks p)
  (define bs (piece->blocks p))
  (filter block-visible? bs))


;;=======================================
;; Entry
;;=======================================

(define-struct entry [id] #:mutable #:transparent)


;;
;; API
;;
;; entry-new    (ef)
;; entry-new-id (e)
;; entry-taken?
;;


(define (entry-new)
  (entry #f))


(define (ef)
  (e #f))


(define (entry-new-id id)
  (entry id))


(define (e id)
  (entry-new-id id))


(define (entry-taken? e)
  (false? (e-id entry)))


(define (entry-set e id)
  (set-entry-id! e id))


;;=======================================
;; Row
;;=======================================

(define-struct entry [id] #:mutable #:transparent)

(define (e id)
  (entry id))

(define (ef)
  (e #f))

;; Example: (cons 0 (list (e F) (e 1) (e 2) (e F) (e F)))


;;
;; API
;;
;; row-new
;; rows-new
;; rows-replenish
;; rows-remove-full
;;


(define (row-new)
  (build-list W ef))


(define (rows-new n)
  (build-list n row-new))


(define (rows-replenish rows)
  (define size (length rows))
  (define lack (- H size))
  (append (rows-new lack) rows))


(define (rows-remove-full rows)
  (filter rows row-not-full?))


(define (row-full? row)
  (= (car row) W))


(define (row-not-full? row)
  (not (row-full? row))


;;=======================================
;; Board
;;=======================================

;; EXAMPLE #1
;;
;; □ □ □ □ □
;; □ □ □ □ □
;; □ ■ ■ □ □
;; □ ■ □ □ □
;; □ ■ □ □ ■

(define board-ex-1 (list (cons 0 (list (e F) (e F) (e F) (e F) (e F)))
                         (cons 0 (list (e F) (e F) (e F) (e F) (e F)))
                         (cons 0 (list (e F) (e F) (e F) (e F) (e F)))
                         (cons 2 (list (e F) (e 1) (e 1) (e F) (e F)))
                         (cons 1 (list (e F) (e 1) (e F) (e F) (e F)))
                         (cons 2 (list (e F) (e 1) (e F) (e F) (e 0)))))


;;
;; API
;;
;; board-new
;; board-piece?
;; board-land
;; board-remove-full
;;


(define (board-new)
  (rows-new H))


(define (board-piece? brd p)
  (and (piece-inside? p)
       (not (board-piece-collision? brd p))))


(define (board-land brd p)
  (define id (piece-id p))
  (define bs (piece->visible-blocks p))
  (for ([b bs])
    (board-land-block brd b id))
  brd)


(define (board-remove-full brd)
  (rows-replenish (rows-remove-full brd)))


;;
;; Lower-level routines
;;
  

(define (board-piece-collision? brd p)
  (define bs (piece->visible-blocks p))
  (board-block-collision*? brd bs))


(define (board-block-collision*? brd bs)
  (ormap (lambda (b) (board-block-collision? b brd)) bs))


(define (board-block-collision? brd b)
  (not (board-taken? brd (block-x b) (block-y b))))


(define (board-taken? brd x y)
  (define e (board-get brd x y))
  (entry-taken? e))


(define (board-land-block brd b id)
  (board-set brd (block-x b) (block-y b) id))


(define (board-get brd x y)
  (define row (list-ref brd y))
  (define entries (cdr row))
  (list-ref entries x))


(define (board-set brd x y id)
  (define e (board-get brd x y))
  (entry-set e id))


;;=======================================
;; Game
;;=======================================

(define-struct game [board piece next-piece
                     score active?])


;;
;; API
;;
;; game-new
;; game-active?
;; game-score
;;
;; game-left
;; game-right
;; game-rotate
;;
;; game-down-tick
;; game-down-key
;; game-fall *
;;


(define (game-new)
  (game [(board-new) (piece-new) (piece-new) 0 #t]))


(define (game-left g)
  (change-piece g piece-left))


(define (game-right g)
  (change-piece g piece-right))


(define (game-rotate g)
  (change-piece g piece-rotate))


(define (game-down-tick g)
  (game-down g 1))


(define (game-down-key g)
  (game-down g 2))


;;
;; Lower-level routines
;;


;; Game Function -> Game
(define (change-piece g fn)
  (define p (game-piece g))
  (define new-p (fn p))
  (define brd (game-board p))
  (if (piece-valid? new-p brd)
      (struct-copy game g [piece new-p]) g))


(define (game-down g rew)
  (cond [(game-move-down? g) (game-move-down g rew)]
        [else (game-land g)]))


(define (game-move-down? g rew)
  (board-piece-valid? (piece-down (game-piece g))
                      (game-board g)))


(define (game-move-down g rew)
  (define p (game-piece g))
  (define new-p (piece-down p))
  (define sc (game-score g))
  (define new-sc (+ sc rew))
  ;; - IN -
  (struct-copy game g
               [piece new-p]
               [score new-sc]))


(define (game-land g)
  (cond [(game-piece-above? g) (game-over g)]
        [else (game-land-piece g)]))


(define (game-land-piece g)
  (define p (game-piece g))
  (define brd (game-board g))
  (define new-brd (board-land brd p))
  (define new-p (game-next-piece g))
  (define new-next-p (piece-new g))
  ;; - IN -
  (struct-copy game g
               [board new-brd]
               [piece new-p]
               [next-piece new-next-p]))


(define (game-piece-above? g)
  (piece-above? (game-piece g)))


(define (game-over g)
  (struct-copy game g [active #f]))


;=======================================
; Window
;=======================================

(define-struct window [game main-counter counter th ])



























;=======================================
; Big-bang
;=======================================

(big-bang (window (game-new))
 [on-tick window-tick (/ 1 10)]
 [on-key  window-key]
 [to-draw window-draw])








