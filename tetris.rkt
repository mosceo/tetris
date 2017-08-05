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

;; for testing
;(define W 5)
;(define H 6)

(define W 10)          ;; board width (# of blocks)
(define H 22)          ;; board height (# of blocks)

(define PIX 30)       ;; block size (pixels)


;;=======================================
;; Helpers
;;=======================================

(define F #f)

(define (matrix-ref mat row col)
  (list-ref (list-ref mat row) col))

(define (id? x)
  (and (number? x) (>= x 0) (< x PIECE#)))


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


(define (min-x bs)
  (apply min (map block-x bs)))
(define (min-y bs)
  (apply min (map block-y bs)))
(define (max-x bs)
  (apply max (map block-x bs)))
(define (max-y bs)
  (apply max (map block-y bs)))


(define (shift-top-left bs)
  (block-shift* bs (- (min-x bs)) (- (min-y bs))))


;;=======================================
;; Game pieces
;;=======================================

(define PIECE# 7)


;; Piece 0
;;
;; □ ■ □  □ ■ □  □ □ □  □ ■ □
;; ■ ■ ■  □ ■ ■  ■ ■ ■  ■ ■ □
;; □ □ □  □ ■ □  □ ■ □  □ ■ □

(define P0-COLOR "pink")
(define P0-BLOCK (list (list (b 1 0) (b 0 1) (b 1 1) (b 2 1))
                       (list (b 1 0) (b 1 1) (b 2 1) (b 1 2))
                       (list (b 0 1) (b 1 1) (b 2 1) (b 1 2))
                       (list (b 1 0) (b 0 1) (b 1 1) (b 1 2))))

;; Piece 1
;;
;; ■ ■
;; ■ ■

(define P1-COLOR "gray")
(define P1-BLOCK (list (list (b 0 0) (b 1 0) (b 0 1) (b 1 1))))

;; Piece 2
;; □ □ □ □  □ ■ □ □
;; ■ ■ ■ ■  □ ■ □ □
;; □ □ □ □  □ ■ □ □
;; □ □ □ □  □ ■ □ □

(define P2-COLOR "cyan")
(define P2-BLOCK (list (list (b 0 1) (b 1 1) (b 2 1) (b 3 1))
                       (list (b 1 0) (b 1 1) (b 1 2) (b 1 3))))

;; Piece 3
;;
;; □ □ □  □ ■ □  ■ □ □  □ ■ ■
;; ■ ■ ■  □ ■ □  ■ ■ ■  □ ■ □
;; □ □ ■  ■ ■ □  □ □ □  □ ■ □

(define P3-COLOR "blue")
(define P3-BLOCK (list (list (b 0 1) (b 1 1) (b 2 1) (b 2 2))
                       (list (b 1 0) (b 1 1) (b 1 2) (b 0 2))
                       (list (b 0 1) (b 1 1) (b 2 1) (b 0 0))
                       (list (b 1 0) (b 1 1) (b 1 2) (b 2 0))))

;; Piece 4
;;
;; □ □ □  ■ ■ □  □ □ ■  □ ■ □
;; ■ ■ ■  □ ■ □  ■ ■ ■  □ ■ □
;; ■ □ □  □ ■ □  □ □ □  □ ■ ■

(define P4-COLOR "orange")
(define P4-BLOCK (list (list (b 0 1) (b 1 1) (b 2 1) (b 0 2))
                       (list (b 1 0) (b 1 1) (b 1 2) (b 0 0))
                       (list (b 0 1) (b 1 1) (b 2 1) (b 2 0))
                       (list (b 1 0) (b 1 1) (b 1 2) (b 2 2))))

;; Piece 5
;;
;; □ ■ ■  ■ □ □
;; ■ ■ □  ■ ■ □
;; □ □ □  □ ■ □

(define P5-COLOR "darkgreen")
(define P5-BLOCK (list (list (b 1 0) (b 2 0) (b 0 1) (b 1 1))
                       (list (b 0 0) (b 0 1) (b 1 1) (b 1 2))))

;; Piece 6
;;
;; ■ ■ □  □ □ ■
;; □ ■ ■  □ ■ ■
;; □ □ □  □ ■ □

(define P6-COLOR "red")
(define P6-BLOCK (list (list (b 0 0) (b 1 0) (b 1 1) (b 2 1))
                       (list (b 2 0) (b 2 1) (b 1 1) (b 1 2))))


;;;;
;; All data in one place
;;;;

(define PIECE-COLOR (list P0-COLOR P1-COLOR P2-COLOR P3-COLOR P4-COLOR P5-COLOR P6-COLOR))
(define PIECE-BLOCK (list P0-BLOCK P1-BLOCK P2-BLOCK P3-BLOCK P4-BLOCK P5-BLOCK P6-BLOCK))
(define PIECE-TYPE# (list (length P0-BLOCK) (length P1-BLOCK) (length P2-BLOCK)
                          (length P3-BLOCK) (length P4-BLOCK) (length P5-BLOCK)
                          (length P6-BLOCK)))

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
;; piece-down-n
;; piece-down
;; piece-rotate
;;
;; piece-inside?
;; piece-above?
;;
;; piece->blocks
;; piece->visible-blocks
;;
;; piece-width
;; piece-height
;;
;;

(define (piece-new)
  (define id (random PIECE#))
  (define type 0)
  (piece id type (piece-start-x id) (piece-start-y id)))


(define (piece-left p)
  (define new-x (sub1 (piece-x p)))
  (struct-copy piece p [x new-x]))


(define (piece-right p)
  (define new-x (add1 (piece-x p)))
  (struct-copy piece p [x new-x]))


(define (piece-down-n p dy)
  (define new-y (+ (piece-y p) dy))
  (struct-copy piece p [y new-y]))


(define (piece-down p)
  (piece-down-n p 1))


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


(define (piece-width id type)
  (define raw-bs (global-piece-blocks id type))
  (define bs (shift-top-left raw-bs))
  (add1 (max-x bs)))


(define (piece-height id type)
  (define raw-bs (global-piece-blocks id type))
  (define bs (shift-top-left raw-bs))
  (add1 (max-y bs)))


;;
;; Routines
;;

(define (piece->raw-blocks p)
  (global-piece-blocks (piece-id p) (piece-type p)))


(define (piece-start-x id)
  (define w (piece-width id 0))
  (quotient (- W w) 2))


(define (piece-start-y id)
  (define h (piece-height id 0))
  (- h))


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


;;=======================================
;; Board
;;=======================================

;;
;; API:
;;
;; board-new
;; board-piece?
;; board-land
;; board-remove-full
;; board-altitude
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


(define (board-altitude brd p)
  (define pp (piece-down p))
  (if (not (board-piece? brd pp)) 0
      (add1 (board-altitude brd pp))))


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


;;=======================================
;; Game
;;=======================================

(define-struct game [board piece next-piece
                     score active?])


;; NOTE: a very strong layer of abstraction, from now on we
;;       use only this API and don't use the code above at all


;;------;;
;; API: ;;
;;------;;
;;
;; game-new
;; game-active?
;; game-over?
;; game-score
;;
;; game-left
;; game-right
;; game-rotate
;;
;; game-down-tick
;; game-down-key
;; game-fall
;;
;;;;

(define (game-new)
  (game (board-new) (piece-new) (piece-new) 0 #t))


(define (game-over? g)
  (not (game-active? g)))


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


(define (game-fall g)
  (define alt (board-altitude (game-board g) (game-piece g)))
  (define g1 (change-piece g (lambda (p) (piece-down-n p alt))))
  (define g2 (game-score+ g1 (* 3 alt)))
  (define g3 (game-down g2 0))
  g3)


;;----------;;
;; Routines ;;
;;----------;;

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


;;=======================================
;; Constants
;;=======================================

(define INIT-THRESHOLD 15)
(define INIT-LEVEL-TICKS 100)
(define RATE (/ 1 20))


;;=======================================
;; Window
;;=======================================

(define-struct window [game cnt cnt-act th level])


(define (window-new)
  (window (game-new) 1 0 INIT-THRESHOLD 1))


;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


(define (window-tick w)
  (cond [(window-stopped? w) w]
        [else
         (define w1 (window-update-counters w))
         (if (window-action? w1)
             (struct-copy window w1 [game (game-down-tick (window-game w1))]) w1)]))


(define (window-action? w)
  (= (window-cnt-act w) 0))


(define (window-update-counters w)
  (define cn (window-cnt w))
  (define ca (window-cnt-act w))
  (define th (window-th w))
  (define lv (window-level w))

  (set! cn (add1 cn))
  (set! ca (add1 ca))

  (when (= ca th) (set! ca 0))
  (when (and (= ca 0) (>= cn INIT-LEVEL-TICKS))
      (set! cn 0)
      (when (> th 1) (set! th (sub1 th)))
      (set! lv (add1 lv)))

  (struct-copy window w [cnt cn] [cnt-act ca] [th th] [level lv]))


;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



(define (window-key w k)
  (cond [(and (window-stopped? w) (key=? k " ")) (window-new)]
        [(window-stopped? w) w]
        [(key=? k "left") (window-key-left w)]
        [(key=? k "right") (window-key-right w)]
        [(key=? k "up") (window-key-up w)]
        [(key=? k "down") (window-key-down w)]
        [(key=? k " ") (window-key-space w)]
        [else w]))


(define (window-key-left w)
  (struct-copy window w [game (game-left (window-game w))]))


(define (window-key-right w)
  (struct-copy window w [game (game-right (window-game w))]))


(define (window-key-up w)
  (struct-copy window w [game (game-rotate (window-game w))]))


(define (window-key-down w)
  (struct-copy window w [game (game-down-key (window-game w))]))


(define (window-key-space w)
  (struct-copy window w [game (game-fall (window-game w))]))


(define (window-draw w)
  (image-render-window w))


;;----------;;
;; Routines ;;
;;----------;;

(define (window-stopped? w)
  (game-over? (window-game w)))


;;=======================================
;; Big-bang
;;=======================================

(define (start-game)
  (big-bang (window-new)
            [on-tick window-tick RATE]
            [on-key  window-key]
            [to-draw window-draw]))


;;=======================================
;; Drawing images
;;=======================================

(define (image-borpic g)
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


(define (image-next-piece id type)
  (define raw-bs (global-piece-blocks id type))
  (define bs (shift-top-left raw-bs))
  (define w (add1 (max-x bs)))
  (define h (add1 (max-y bs)))
  (define scene (rectangle (* PIX w) (* PIX h) "solid" "transparent"))
  (image-block*/scene bs id scene))




;;=======================================
;; Drawing Window
;;=======================================





(define GAME-OVER-TEXT (above (text "Press \"space\"" 18 "black")
                              (text "to start a new game" 18 "black")))



(define (image-render-window w)
  (define rpanel (image-rpanel w))
  (define borpic (image-borpic (window-game w)))
  (beside/align "top" borpic rpanel))


(define (image-rpanel w)
  (define g (window-game w))
  (define np (game-next-piece g))
  (define score (image-score (game-score g)))
  (define level (image-level (window-level w)))
  (define next-piece (image-next-piece (piece-id np) (piece-type np)))

  (define p_ (rectangle 1 40 "solid" "transparent"))
  
  (if (window-stopped? w)
      (above score level p_ next-piece p_ GAME-OVER-TEXT)
      (above score level p_ next-piece)))

(define (image-score sc)
  (define rect (rectangle 200 100 'solid "WhiteSmoke"))
  (define txt (text (number->string sc) 30 "black"))
  (overlay txt rect))


(define (image-level n)
  (define rect (rectangle 200 50 'solid "LightGray"))
  (define str (string-append "Level " (number->string n)))
  (define txt (text str 20 "black"))
  (overlay txt rect))


;;=======================================
;; Run
;;=======================================

;(include "test.rkt")

(start-game)






