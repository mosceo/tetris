;;===========================================
;;
;;   TETRIS
;;
;;===========================================
;;
;; A colorful version of tetris.
;;
;; Author:  Roman Kunin (mosceo@gmail.com)
;; Source:  https://github.com/mosceo/tetris
;; License: MIT
;;
;;===========================================

#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)


;;=======================================
;; Global constants
;;=======================================

(define W 5)          ;; board width (# of blocks)
(define H 6)          ;; board height (# of blocks)
(define PIX 50)       ;; block size (pixels)
(define RATE 1.0)     ;; tick event inrerval (s)
(define PIECE# 2)     ;; number of pieces


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
;; API:
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
(define P0-SHIFT (list (b 0 0) (b -1 0) (b 0 -1) (b 0 0)))
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
(define P1-SHIFT (list (b 0 0)))
(define P1-BLOCK (list (list (b 0 0) (b 1 0) (b 0 1) (b 1 1))))


;;
;; All data in one place
;;

(define PIECE-COLOR (list P0-COLOR P1-COLOR))
(define PIECE-BLOCK (list P0-BLOCK P1-BLOCK))
(define PIECE-TYPE# (list P0-TYPE# P1-TYPE#))
(define PIECE-SHIFT (list P0-SHIFT P1-SHIFT))
(define PIECE-SIZE  (list P0-SIZE  P1-SIZE))


;;
;; API:
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


(define (global-piece-shift id type)
  (matrix-ref PIECE-SHIFT id type))


(define (global-piece-size id)
  (list-ref PIECE-SIZE id))


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
;; API:
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
  (piece id type 0 -4))


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
;; API:
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
  (number? (entry-id e)))


(define (entry-set e id)
  (set-entry-id! e id))


;;
;; Unit tests
;;

(check-pred entry? (e 1))
(check-pred entry? (ef))
(check-equal? (entry-id (e 1)) 1)
(check-equal? (entry-id (ef)) #f)

(check-true (entry-taken? (e 1)))
(check-false (entry-taken? (e #f)))

(local [(define e1 (e 1))]
  (entry-set e1 2)
  (check-equal? (entry-id e1) 2))


;;=======================================
;; Row
;;=======================================

(define-struct row [count entries] #:mutable #:transparent)


;; Example: (row 2 (list (ef) (e 0) (e 1) (ef) (ef)))

;;
;; API:
;;
;; row-set
;; rows-new
;; rows-entry
;; rows-remove-full
;;

(define (row-set r x id)
  (define new-count (add1 (row-count r)))
  (define entry (list-ref (row-entries r) x))
  (entry-set entry id)
  (set-row-count! r new-count))


(define (rows-new n)
  (for/list ([i n]) (row-new)))


(define (rows-entry rs x y)
  (define r (list-ref rs y))
  (list-ref (row-entries r) x))


(define (rows-remove-full rs)
  (filter row-not-full? rs))


;;
;; Lower-level routines
;;

(define (row-new)
  (row 0 (for/list ([i W]) (ef))))


(define (row-full? r)
  (= (row-count r) W))


(define (row-not-full? row)
  (not (row-full? row)))


(define (rows-replenish rs)
  (define lack (- H (length rs)))
  (append (rows-new lack) rs))


;;
;; Unit tests
;;

(local [(define r1 (row 1 (list (e 0) (ef) (ef))))
        (define r2 (row 2 (list (e 0) (ef) (e 1))))]
  (row-set r1 2 1)
  (check-equal? r1 r2))

(check-equal? (rows-new 0) (list))
(check-equal? (rows-new 1) (list (row-new)))
(check-equal? (rows-new 2) (list (row-new) (row-new)))

(check-equal? (rows-entry (list (row 0 '(a b c)) (row 0 '(d e g))) 0 0) 'a)
(check-equal? (rows-entry (list (row 0 '(a b c)) (row 0 '(d e g))) 2 1) 'g)

(check-equal? (rows-remove-full (list (row 1 'd) (row W 'd)  (row W 'd) (row 2 'd)))
              (list (row 1 'd) (row 2 'd)))

(check-equal? (row-new) (row 0 (list (ef) (ef) (ef) (ef) (ef))))

(check-true (row-full? (row W 'd)))
(check-false (row-full? (row 1 'd)))

(check-true (row-not-full? (row 1 'd)))
(check-false (row-not-full? (row W 'd)))

(check-equal? (rows-replenish (list (row 1 'd) (row 2 'd)))
              (list (row-new) (row-new) (row-new) (row-new) (row 1 'd) (row 2 'd)))


;;=======================================
;; Board
;;=======================================

(define (cr count vals)
  (row count (map (lambda (val) (entry val)) vals)))

(check-equal? (cr 2 (list 0 F F 1)) (row 2 (list (e 0) (ef) (ef) (e 1))))

 
;; EXAMPLE #1
;;
;; □ □ □ □ □
;; □ □ □ □ ■
;; ■ ■ ■ □ ■
;; ■ ■ □ □ □
;; ■ ■ ■ □ ■
;; □ ■ □ □ □

(define board-ex-1 (list (cr 0 (list F F F F F))
                         (cr 1 (list F F F F 1))
                         (cr 4 (list 0 0 1 F 1))
                         (cr 2 (list 0 0 F F F))
                         (cr 4 (list 1 1 1 F 0))
                         (cr 1 (list F 1 F F F))))

;; EXAMPLE #2
;;
;; □ □ □ □ □
;; □ □ □ □ □
;; □ □ □ □ □
;; □ □ □ □ ■
;; ■ ■ □ ■ ■
;; □ ■ □ □ □

(define board-ex-2 (list (cr 0 (list F F F F F))
                         (cr 0 (list F F F F F))
                         (cr 0 (list F F F F F))
                         (cr 1 (list F F F F 1))
                         (cr 4 (list 0 0 F 0 0))
                         (cr 1 (list F 1 F F F))))


;;
;; API:
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
  (define bs (piece->visible-blocks p))
  (board-land-block* brd bs (piece-id p))
  (board-remove-full brd))


(define (board-remove-full brd)
  (rows-replenish (rows-remove-full brd)))


;;
;; Lower-level routines
;;

(define (board-piece-collision? brd p)
  (define bs (piece->visible-blocks p))
  (board-block-collision*? brd bs))


(define (board-block-collision*? brd bs)
  (ormap (lambda (b) (board-block-collision? brd b)) bs))


(define (board-block-collision? brd b)
  (board-taken? brd (block-x b) (block-y b)))


(define (board-land-block* brd bs id)
  (for ([b bs]) (board-land-block brd b id))
  brd)


(define (board-land-block brd b id)
  (define row (list-ref brd (block-y b)))
  (row-set row (block-x b) id))


(define (board-taken? brd x y)
  (define e (board-get brd x y))
  (entry-taken? e))


(define (board-get brd x y)
  (rows-entry brd x y))


(define (board-set brd x y id)
  (define row (list-ref brd y))
  (row-set row x id))


;;
;; Unit tests
;;

(check-pred list? (board-new))

(check-true (board-piece? board-ex-1 (piece 1 0 0 0)))
(check-true (board-piece? board-ex-1 (piece 0 1 2 2)))
(check-false (board-piece? board-ex-1 (piece 1 0 -1 0)))
(check-false (board-piece? board-ex-1 (piece 0 1 2 3)))

(local [(define b1 (list (cr 0 (list F F F F F))
                         (cr 1 (list F F F F 1))
                         (cr 4 (list 0 0 1 F 1))
                         (cr 2 (list 0 0 F F F))
                         (cr 4 (list 1 1 1 F 0))
                         (cr 1 (list F 1 F F F))))
        (define b2 (board-land b1 (piece 0 1 2 2)))]
  (check-equal? b2 board-ex-2))

;; board-remove-full will be implicitely tested while testing board-land

(check-false (board-piece-collision? board-ex-1 (piece 0 1 2 2)))
(check-true (board-piece-collision? board-ex-1 (piece 0 1 2 3)))
(check-false (board-piece-collision? board-ex-1 (piece 1 0 2 0)))
(check-true (board-piece-collision? board-ex-1 (piece 1 0 3 0)))

(check-true (board-block-collision*? board-ex-1 (list (b 3 2) (b 4 2) (b 3 3))))
(check-false (board-block-collision*? board-ex-1 (list (b 3 2) (b 3 3))))

(check-true (board-block-collision? board-ex-1 (b 4 2)))
(check-false (board-block-collision? board-ex-1 (b 3 2)))

(local [(define b1 (list (cr 1 (list F 1 F))
                         (cr 1 (list 0 F F))))
        (define b2 (list (cr 2 (list F 1 0))
                         (cr 2 (list 0 0 F))))]
  (board-land-block* b1 (list (b 2 0) (b 1 1)) 0)
  (check-equal? b1 b2))


(local [(define b1 (list (cr 1 (list F 1 F))
                         (cr 1 (list 0 F F))))
        (define b2 (list (cr 1 (list F 1 F))
                         (cr 2 (list 0 F 0))))]
  (board-land-block b1 (b 2 1) 0)
  (check-equal? b1 b2))

(check-true (board-taken? board-ex-1 4 1))
(check-false (board-taken? board-ex-1 3 1))

(check-equal? (board-get board-ex-1 4 1) (entry 1))
(check-equal? (board-get board-ex-1 3 1) (entry #f))

(local [(define b1 (list (cr 1 (list F 1 F))
                         (cr 1 (list 0 F F))))
        (define b2 (list (cr 1 (list F 1 F))
                         (cr 2 (list 0 F 0))))]
  (board-set b1 2 1 0)
  (check-equal? b1 b2))


;;=======================================
;; Game
;;=======================================

(define-struct game [board piece next-piece
                     score active?])


;; NOTE: a very strong layer of abstraction, from now on we
;;       use only this API and don't use the code above at all


;;
;; API:
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
  (game (board-new) (piece-new) (piece-new) 0 #t))


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

(define (game-down g s)
  (if (game-move-down? g) (game-score+ (game-move-down g) s)
      (game-land g)))


(define (game-move-down? g)
  (board-piece? (game-board g) (piece-down (game-piece g))))


(define (game-move-down g)
  (define new-p (piece-down (game-piece g)))
  (struct-copy game g [piece new-p]))


(define (game-land g)
  (if (game-piece-above? g) (game-stop g) (game-land-piece g)))


(define (game-land-piece g)
  (struct-copy game g
               [board (board-land (game-board g) (game-piece g))]
               [piece (game-next-piece g)]
               [next-piece (piece-new)]))


(define (game-score+ g ds)
  (define new-sc (+ (game-score g) ds))
  (struct-copy game g [score new-sc]))


(define (game-piece-above? g)
  (piece-above? (game-piece g)))


(define (game-stop g)
  (struct-copy game g [active? #f]))


(define (change-piece g fn)
  (define new-p (fn (game-piece g)))
  (if (board-piece? (game-board g) new-p)
      (struct-copy game g [piece new-p]) g))


;;
;; Unit tests
;;


;;=======================================
;; Window
;;=======================================

(define-struct window [game counter])


(define (window-new)
  (window (game-new) 0))


(define (window-tick w)
  (define new-g (game-down-tick (window-game w)))
  (define new-c (add1 (window-counter w)))
  (window new-g new-c))


(define (window-key w k)
  (cond [(key=? k "left") (window-key-left w)]
        [(key=? k "right") (window-key-right w)]
        [(key=? k "up") (window-key-up w)]
        [(key=? k "down") (window-key-down w)]
        [else w]))


(define (window-key-left w)
  (struct-copy window w [game (game-left (window-game w))]))


(define (window-key-right w)
  (struct-copy window w [game (game-right (window-game w))]))


(define (window-key-up w)
  (struct-copy window w [game (game-rotate (window-game w))]))


(define (window-key-down w)
  (struct-copy window w [game (game-down-key (window-game w))]))


(define (window-draw w)
  (image-render-window w))


;;
;; Unit tests
;;

(check-pred window? (window-new))



;;=======================================
;; Big-bang
;;=======================================

(define (start-game)
  (big-bang (window-new)
            [on-tick window-tick 1]
            [on-key  window-key]
            [to-draw window-draw]))


;;=======================================
;; Drawing images
;;=======================================

(define (image-board-piece g)
  (define p (game-piece g))
  (define brd (game-board g))
  (define brd-image (board->image brd))
  (image-piece/scene p brd-image))


(define (background-image)
  (define col1 (rectangle PIX (* PIX H) "solid" "LightGoldenrodYellow"))
  (define col2 (rectangle PIX (* PIX H) "solid" "PaleGoldenrod"))
  (define cols (build-list W (lambda (n) (if (= (modulo n 2) 0) col1 col2))))
  (apply beside cols))


(define BACKGROUND (background-image))


(define (image-block color)
  (define outline (square PIX 'outline "black"))
  (define sq (square PIX "solid" color))
  (overlay outline sq))


(define (image-image/scene im x y scene)
  (underlay/xy scene (* x PIX) (* y PIX) im))


(define (image-block/scene x y id scene)
  (define color (global-piece-color id))
  (define b-im (image-block color))
  (image-image/scene b-im x y scene))


(define (image-block*/scene bs id scene)
  (foldl (lambda (b im) (image-block/scene (block-x b) (block-y b) id im))
         scene bs))


(define (image-piece/scene p scene)
  (define bs (piece->visible-blocks p))
  (image-block*/scene bs (piece-id p) scene))


(define (board->image brd)
  (define im BACKGROUND)
  (define (row->image es y)
    (for ([x W] [e es])
      (when (entry-taken? e) (set! im (image-block/scene x y (entry-id e) im)))))
  ;; --IN--
  (for ([y H] [row brd])
    (row->image (row-entries row) y))
  im)


;;=======================================
;; Drawing Window
;;=======================================

(define (image-render-window w)
  (define g (window-game w))
  (define board-piece (image-board-piece g))
  (define score-display (image-render-score (game-score g)))
  (beside board-piece score-display))


(define (image-render-score sc)
  (define rect1 (rectangle 110 50 'solid 'black))
  (define rect2 (rectangle 100 40 'solid 'white))
  (define txt (text (number->string sc) 20 "black"))
  (overlay txt rect2 rect1))


;(define (image-render-next-piece g)
;  ...)


(define (image-render-piece p)
  (define shift (global-piece-shift p))


(start-game)


















