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

#lang racket/base

(require 2htdp/image
         2htdp/universe)

(provide (all-defined-out))

;;=======================================
;; Global constants
;;=======================================

;; FOR PLAYING
(define W 10)         ;; board width  (# of blocks)
(define H 22)         ;; board height (# of blocks)

(define PIX 30)       ;; block size (pixels)

(define INIT-THRESHOLD 15)        ;; initial speed of blocks
(define INIT-LEVEL-TICKS 1000)    ;; how fast speed changes
(define RATE (/ 1 20))            ;; how much time one tick takes

(define (set-W! new-W)
  (set! W new-W))

(define (set-H! new-H)
  (set! H new-H))



;;=======================================
;; Helpers
;;=======================================

;; Matrix Number Number -> Any
;; get a matrix entry M[row][col] 
(define (matrix-ref mat row col)
  (list-ref (list-ref mat row) col))


;;=======================================
;; Block
;;=======================================
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
;; min-x
;; min-y
;; max-x
;; max-y
;;
;; shift-top-left
;;

;; A Block is a structure:
;;   (block Number Number)
;; represents a block in a game of tetris by its coordinates
;;
;; Ex.: (block 0 6)
;;
(define-struct block [x y] #:transparent)


;; Number Number -> Block
;; a shorthand to create a block
(define (b x y)
  (block x y))

;; Block Number Number -> Block
;; shift a block
(define (block-shift b dx dy)
  (block (+ (block-x b) dx)
         (+ (block-y b) dy)))

;; [List-of Block] Number Number -> [List-of Block]
;; shift each block in a list
(define (block-shift* bs dx dy)
  (map (lambda (b) (block-shift b dx dy)) bs))

;; DEFINITION:
;;   We say that a block is "inside" a board, if it is visually
;;   on the board or will appear on the board when dragged down.

;; Block -> Boolean
;; check if a block is inside the board
(define (block-inside? b)
  (define x (block-x b))
  (define y (block-y b))
  (and (>= x 0) (< x W) (< y H)))

;; [List-of Block] -> Boolean
;; check if ALL blocks are inside a board
(define (block-inside*? bs)
  (andmap block-inside? bs))

;; Block -> Boolean
;; check if a block is above a board
(define (block-above? b)
  (< (block-y b) 0))

;; [List-of Block] -> Boolean
;; check if AT LEAST ONE block is above a board
(define (block-above*? bs)
  (ormap block-above? bs))

;; Block -> Boolean
;; check if a block is visible (right on the board)
(define (block-visible? b)
  (and (block-inside? b)
       (not (block-above? b))))

;; [List-of Block] -> Boolean
;; check if ALL blocks are visible
(define (block-visible*? bs)
  (andmap block-visible? bs))

;; [List-of Block] -> Number
;; find the minimun x-coordinate among a list of blocks
(define (min-x bs)
  (apply min (map block-x bs)))

;; [List-of Block] -> Number
;; find the minimum y-coordinate among a list of blocks
(define (min-y bs)
  (apply min (map block-y bs)))

;; [List-of Block] -> Number
;; find the maximum x-coordinate among a list of blocks
(define (max-x bs)
  (apply max (map block-x bs)))

;; [List-of Block] -> Number
;; find the maximum y-coordinate among a list of blocks
(define (max-y bs)
  (apply max (map block-y bs)))

;; [List-of Block] -> [List-of Block]
;; shift a set of blocks (as one piece) to the top-left corner
(define (shift-top-left bs)
  (block-shift* bs (- (min-x bs)) (- (min-y bs))))


;;=======================================
;; Game pieces
;;=======================================
;;
;; API:
;;
;; global-piece-color
;; global-piece-blocks
;; global-piece-type#
;;

;; the number of pieces
(define PIECE# 7)

;; An ID is a Number 0..6
;; represents the ID of a piece
;;
;; Ex.: 6
;;

;; Any -> Boolean
;; check if it is a proper piece id
(define (id? x)
  (and (number? x) (>= x 0) (< x PIECE#)))


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

;; All data in one place
(define PIECE-COLOR (list P0-COLOR P1-COLOR P2-COLOR P3-COLOR P4-COLOR P5-COLOR P6-COLOR))
(define PIECE-BLOCK (list P0-BLOCK P1-BLOCK P2-BLOCK P3-BLOCK P4-BLOCK P5-BLOCK P6-BLOCK))
(define PIECE-TYPE# (list (length P0-BLOCK) (length P1-BLOCK) (length P2-BLOCK)
                          (length P3-BLOCK) (length P4-BLOCK) (length P5-BLOCK)
                          (length P6-BLOCK)))

;; ID -> Color
;; get the color of a piece
(define (global-piece-color id)
  (list-ref PIECE-COLOR id))

;; ID Number -> [List-of Block]
;; get raw blocks for a piece in a certain configuration
(define (global-piece-blocks id type)
  (matrix-ref PIECE-BLOCK id type))

;; ID -> Number
;; get the number of configurations for a piece
(define (global-piece-type# id)
  (list-ref PIECE-TYPE# id))


;;=======================================
;; Piece
;;=======================================
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

;; A Piece is a structure:
;;   (piece ID Number Number Number)
;; represents a piece in a game of tetris where id and type specify the piece,
;;   and (x, y) is its position on the board
;;
;; Ex.: (piece 3 2 0 -3)
;;
(define-struct piece [id type x y] #:transparent)


;; Void -> Piece
;; create a random piece
(define (piece-new)
  (define id (random PIECE#))
  (define type 0)
  (piece id type (piece-start-x id) (piece-start-y id)))

;; Piece -> Piece
;; move a piece to the left
(define (piece-left p)
  (define new-x (sub1 (piece-x p)))
  (struct-copy piece p [x new-x]))

;; Piece -> Piece
;; move a piece to the right
(define (piece-right p)
  (define new-x (add1 (piece-x p)))
  (struct-copy piece p [x new-x]))

;; Piece -> Piece
;; move a piece down a specified number of times
(define (piece-down-n p dy)
  (define new-y (+ (piece-y p) dy))
  (struct-copy piece p [y new-y]))

;; Piece -> Piece
;; move a piece down
(define (piece-down p)
  (piece-down-n p 1))

;; Piece -> Piece
;; rotate a piece
(define (piece-rotate p)
  (define id (piece-id p))
  (define type (piece-type p))
  (define type# (global-piece-type# id))
  (define new-type (modulo (add1 type) type#))
  (struct-copy piece p [type new-type]))

;; Piece -> Boolean
;; check if a piece is completely inside a board
;; (read the definition of "inside" for a block)
(define (piece-inside? p)
  (block-inside*? (piece->blocks p)))

;; Piece -> Boolean
;; check if a piece is at least partially above the board
(define (piece-above? p)
  (block-above*? (piece->blocks p)))

;; Piece -> [List-of Block]
;; convert a piece to a list of blocks
(define (piece->blocks p)
  (define raw-bs (global-piece-blocks (piece-id p) (piece-type p)))
  (block-shift* raw-bs (piece-x p) (piece-y p)))

;; Piece -> [List-of Block]
;; convert a piece to a list of blocks, that are visible on the page
(define (piece->visible-blocks p)
  (define bs (piece->blocks p))
  (filter block-visible? bs))

;; DEFINITION:
;;   The piece width is defined to be the minimum width of a board that can
;;   contain the piece. The piece height is defined in a similar fashion.

;; ID Number -> Number
;; compute a piece width
(define (piece-width id type)
  (define raw-bs (global-piece-blocks id type))
  (define bs (shift-top-left raw-bs))
  (add1 (max-x bs)))

;; ID Number -> Number
;; compute a piece height
(define (piece-height id type)
  (define raw-bs (global-piece-blocks id type))
  (define bs (shift-top-left raw-bs))
  (add1 (max-y bs)))


;;-----------
;; Routines
;;-----------

;; ID -> Number
;; compute the initial x-coordinate for a piece
(define (piece-start-x id)
  (define w (piece-width id 0))
  (quotient (- W w) 2))

;; ID -> Number
;; compute the initial y-coordinate for a piece
(define (piece-start-y id)
  (define h (piece-height id 0))
  (- h))


;;=======================================
;; Entry
;;=======================================
;;
;; API:
;;
;; e
;; ef
;; entry-taken?
;; entry-set
;;

;; An Entry is a structure:
;;   (entry ID|#f)
;; represents one entry (cell) of a board,
;;   a cell is considered empty if it contains #f, otherwise it contains
;;   an ID and is considred taken by a block with this ID
;;   entry is mutable so that you don't need to copy
;;   the whole board to change one entry
;;
;; Ex.: (entry 0)
;;
(define-struct entry [id] #:mutable #:transparent)


;; ID -> Entry
;; a shorthand to create an entry
(define (e id)
  (entry id))

;; Void -> Entry
;; create an empty entry
(define (ef)
  (e #f))

;; Entry -> Boolean
;; check if an entry is taken by a block
(define (entry-taken? e)
  (number? (entry-id e)))

;; Entry ID -> Entry
;; make an entry taken by a block with a given ID
(define (entry-set e id)
  (set-entry-id! e id))


;;=======================================
;; Row
;;=======================================
;;
;; API:
;;
;; row-set
;; rows-new
;; rows-entry
;; rows-remove-full
;; rows-replenish
;;

;; An Row is a structure:
;;   (row Number [List-of Entry])
;; represents a row of cells in a board,
;;   the number says how many cells contain blocks
;;
;; Ex.: (row 2 (list (e #f) (e 1) (e #f) (e #f) (e 2)))
;;
(define-struct row [count entries] #:mutable #:transparent)


;; Row Index ID -> Void
;; make a certain entry in a row taken by a block with a given ID
(define (row-set r x id)
  (define new-count (add1 (row-count r)))
  (define entry (list-ref (row-entries r) x))
  (entry-set entry id)
  (set-row-count! r new-count))

;; Number -> [List-of Row]
;; create a list of rows
(define (rows-new n)
  (for/list ([i n]) (row-new)))

;; [List-of Row] Number Number
;; get an entry with given coordinates
;; (as if a list of rows was a matirx)
(define (rows-entry rs x y)
  (define r (list-ref rs y))
  (list-ref (row-entries r) x))

;; [List-of Row] -> [List-of Row]
;; remove all full rows
(define (rows-remove-full rs)
  (filter row-not-full? rs))

;; [List-of Row] -> [List-of Row]
;; add free rows to the top of the list until its size
;; is equal the height of a board (global constant)
(define (rows-replenish rs)
  (define lack (- H (length rs)))
  (append (rows-new lack) rs))


;;-----------
;; Routines
;;-----------

;; Number -> Row
;; create an empty row
(define (row-new)
  (row 0 (for/list ([i W]) (ef))))

;; Row -> Boolean
;; check if a row has no free cells
(define (row-full? r)
  (= (row-count r) W))

;; Row -> Boolean
;; check if a row has a free cell
(define (row-not-full? row)
  (not (row-full? row)))


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

;; An Board is a [List-of Row] of size H
;; represents a board in a game of tetris
;;
;; Ex.: (rows-new H)
;;


;; Void -> Board
;; create an empty board
(define (board-new)
  (rows-new H))

;; Board Piece -> Boolean
;; check if a piece can be put on a board
(define (board-piece? brd p)
  (and (piece-inside? p)
       (not (board-piece-collision? brd p))))

;; Board Piece -> Board
;; put a piece on a board
(define (board-land brd p)
  (define bs (piece->visible-blocks p))
  (board-land-block* brd bs (piece-id p))
  (board-remove-full brd))

;; Board -> Board
;; remove full rows
(define (board-remove-full brd)
  (rows-replenish (rows-remove-full brd)))

;; DEFINITION:
;;   The altitude is the maxumum number of times a valid
;;   piece can be moved down on a given board.

;; Board Piece -> Board
;; compute the altitude of a piece
(define (board-altitude brd p)
  (define pp (piece-down p))
  (if (not (board-piece? brd pp)) 0
      (add1 (board-altitude brd pp))))


;;-----------
;; Routines
;;-----------

;; Board Piece -> Boolean
;; check if a piece collides with other blocks on a board
(define (board-piece-collision? brd p)
  (define bs (piece->visible-blocks p))
  (board-block-collision*? brd bs))

;; Board [List-of Block] -> Boolean
;; check if a set of blocks collide with other blocks on a board
(define (board-block-collision*? brd bs)
  (ormap (lambda (b) (board-block-collision? brd b)) bs))

;; Board Block -> Boolean
;; check if a block collides with other blocks on a board
(define (board-block-collision? brd b)
  (board-taken? brd (block-x b) (block-y b)))

;; Board [List-of Block] ID -> Board
;; land a set of blocks on a board as blocks of a given ID
(define (board-land-block* brd bs id)
  (for ([b bs]) (board-land-block brd b id))
  brd)

;; Board Block ID -> Board
;; land a block on a board as a block of a given ID
(define (board-land-block brd b id)
  (define row (list-ref brd (block-y b)))
  (row-set row (block-x b) id))

;; Board Number Number -> Boolean
;; check if a board entry is taken
(define (board-taken? brd x y)
  (define e (board-get brd x y))
  (entry-taken? e))

;; Board Number Number -> ID|#f
;; get a board entry
(define (board-get brd x y)
  (rows-entry brd x y))

;; Board Number Number ID -> Void
;; set a board entry
(define (board-set brd x y id)
  (define row (list-ref brd y))
  (row-set row x id))


;;=======================================
;; Game
;;=======================================
;;
;; API:
;;
;; game-new
;; game-active?
;; game-over?
;; game-score
;; game-landed?
;; game-piece-onland
;;
;; game-left
;; game-right
;; game-rotate
;;
;; game-down-tick
;; game-down-key
;; game-fall
;;

;; A Game is a structure:
;;   (game Board Piece Piece Number Boolean)
;; represents a state of a game of tetris
;;
;; Ex.: (game (board-new) (piece-new) (piece-new) 0 #t)
;;
(define-struct game [board piece next-piece
                     score active? land-cnt])


;; Void -> Game
;; create an initial state of a game
(define (game-new)
  (game (board-new) (piece-new) (piece-new) 0 #t 0))

;; Game -> Boolean
;; check if a game has finished
(define (game-over? g)
  (not (game-active? g)))

;; Game Game -> Boolean
;; given two consecutive states of a game, decide if a piece has landed
(define (game-landed? g1 g2)
  (not (= (game-land-cnt g1) (game-land-cnt g2))))

;; Game -> Piece
;; get the piece in the state of after it has landed
(define (game-piece-onland g)
  (define alt (board-altitude (game-board g) (game-piece g)))
  (display alt)
  (piece-down-n (game-piece g) alt))

;; Game -> Game
;; perform a left move
(define (game-left g)
  (change-piece g piece-left))

;; Game -> Game
;; perform a right move
(define (game-right g)
  (change-piece g piece-right))

;; Game -> Game
;; perform rotation
(define (game-rotate g)
  (change-piece g piece-rotate))

;; Game -> Game
;; move the piece down (clock event)
(define (game-down-tick g)
  (game-down g 1))

;; Game -> Game
;; move the piece down (pressed key)
(define (game-down-key g)
  (game-down g 2))

;; Game -> Game
;; perform a fall in a game
(define (game-fall g)
  (define alt (board-altitude (game-board g) (game-piece g)))
  (define g1 (change-piece g (lambda (p) (piece-down-n p alt))))
  (define g2 (game-score+ g1 (* 3 alt)))
  (define g3 (game-down g2 0))
  g3)


;;-----------
;; Routines
;;-----------

;; Game Number -> Game
;; move the piece down and increment the score
(define (game-down g s)
  (if (game-move-down? g) (game-score+ (game-move-down g) s)
      (game-land g)))

;; Game -> Boolean
;; check if the piece can be moved down
(define (game-move-down? g)
  (board-piece? (game-board g) (piece-down (game-piece g))))

;; Game -> Boolean
;; move the piece down in a game
(define (game-move-down g)
  (define new-p (piece-down (game-piece g)))
  (struct-copy game g [piece new-p]))

;; Game -> Game
;; if the piece is above, finish the game, otherwise land the piece
(define (game-land g)
  (if (game-piece-above? g) (game-stop g) (game-land-piece g)))

;; Game -> Game
;; land the piece and update the pieces
(define (game-land-piece g)
  (struct-copy game g
               [board (board-land (game-board g) (game-piece g))]
               [piece (game-next-piece g)]
               [next-piece (piece-new)]
               [land-cnt (add1 (game-land-cnt g))]))

;; Game Number -> Game
;; increment the score in a game
(define (game-score+ g ds)
  (define new-sc (+ (game-score g) ds))
  (struct-copy game g [score new-sc]))

;; Game -> Boolean
;; check if the piece is above the board in a game
(define (game-piece-above? g)
  (piece-above? (game-piece g)))

;; Game -> Game
;; stop a game
(define (game-stop g)
  (struct-copy game g [active? #f]))

;; Game -> Game
;; update pieces in a game
(define (change-piece g fn)
  (define new-p (fn (game-piece g)))
  (if (board-piece? (game-board g) new-p)
      (struct-copy game g [piece new-p]) g))

;;=======================================
;; Window
;;=======================================
;;
;; API:
;;
;; window-new
;; window-tick
;; window-key
;; window-draw
;;

;; A Window is a structure:
;;   (window Game Number Number Number Number Image)
;; represents the highest-level state of a game of tetris, contains the current level
;;   and additional data that is needed to control the speed of the game
;;
;; Ex.: (window (game-new) 1 0 INIT-THRESHOLD 1)
;;
(define-struct window [game cnt cnt-act th level board-image])


;; Void -> Window
;; create a window for a new game
(define (window-new)
  (window (game-new) 1 0 INIT-THRESHOLD 1 (background-image)))

;; Window -> Window
;; a global tick changes the state of a window
(define (window-tick w)
  (cond [(window-stopped? w) w]
        [else
         (define w1 (window-update-counters w))
         (cond [(not (window-action? w1)) w1]
               [else (window-change-state w1 game-down-tick)])]))
                
;; Window KeyEvent -> Window
;; handle a big-bang's key event
(define (window-key w k)
  (cond [(and (window-stopped? w) (key=? k " ")) (window-new)]
        [(window-stopped? w) w]
        [(key=? k "left")  (window-key-left w)]
        [(key=? k "right") (window-key-right w)]
        [(key=? k "up")    (window-key-up w)]
        [(key=? k "down")  (window-key-down w)]
        [(key=? k " ")     (window-key-space w)]
        [else w]))

;; Window -> Image
;; render a window
(define (window-draw w)
  (image-render-window w))


;;-----------
;; Routines
;;-----------

;; Window Func -> Window
;; change the state of the game object and if a piece lands,
;; change the saved board image
(define (window-change-state w fn)
  (define g1 (window-game w))
  (define g2 (fn g1))
  (define bi1 (window-board-image w))
  (define bi2 (cond [(game-landed? g1 g2) (board->image (game-board g2))]
                    [else bi1]))
  (struct-copy window w [game g2] [board-image bi2]))

;; Window -> Window
;; the 'left' key has been pressed
(define (window-key-left w)
  (struct-copy window w [game (game-left (window-game w))]))

;; Window -> Window
;; the 'right' key has been pressed
(define (window-key-right w)
  (struct-copy window w [game (game-right (window-game w))]))

;; Window -> Window
;; the 'up' key has been pressed
(define (window-key-up w)
  (struct-copy window w [game (game-rotate (window-game w))]))

;; Window -> Window
;; the 'down' key has been pressed
(define (window-key-down w)
  (window-change-state w game-down-key))

;; Window -> Window
;; the 'space' key has been pressed
(define (window-key-space w)
  (define g (game-fall (window-game w)))
  (struct-copy window w [game g] [board-image (board->image (game-board g))]))

;; Window -> Boolean
;; check if a game has finished
(define (window-stopped? w)
  (game-over? (window-game w)))

;; Window -> Boolean
;; ...
(define (window-action? w)
  (= (window-cnt-act w) 0))

;; Window -> Window
;; update counters
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


;;=======================================
;; Big-bang
;;=======================================

;; Void -> Window
;; run a game of tetris
(define (start-game)
  (big-bang (window-new)
            [on-tick window-tick RATE]
            [on-key  window-key]
            [to-draw window-draw]))


;;=======================================
;; Drawing images
;;=======================================

;; Void -> Image
;; render the background for a board (an empty board looks like this)
(define (background-image)
  (define col1 (rectangle PIX (* PIX H) "solid" "LightGoldenrodYellow"))
  (define col2 (rectangle PIX (* PIX H) "solid" "PaleGoldenrod"))
  (define cols (build-list W (lambda (n) (if (= (modulo n 2) 0) col1 col2))))
  (apply beside cols))

(define BACKGROUND (background-image))

;; Color -> Image
;; render one block
(define (image-block color)
  (define outline (square PIX 'outline "black"))
  (define sq (square PIX "solid" color))
  (overlay outline sq))

;; Image Number Number Image
;; put an image on a scene
(define (image-image/scene im x y scene)
  (underlay/xy scene (* x PIX) (* y PIX) im))

;; Number Number ID Image -> Image
;; draw a block on a scene
(define (image-block/scene x y id scene)
  (define color (global-piece-color id))
  (define b-im (image-block color))
  (image-image/scene b-im x y scene))

;; [List-of Block] ID Image -> Image
;; draw a set of blocks on a scene
(define (image-block*/scene bs id scene)
  (foldl (lambda (b im) (image-block/scene (block-x b) (block-y b) id im))
         scene bs))

;; Piece Image -> Image
;; draw a piece on a scene
(define (image-piece/scene p scene)
  (define bs (piece->visible-blocks p))
  (image-block*/scene bs (piece-id p) scene))

;; Board -> Image
;; draw a board
(define (board->image brd)
  (define im BACKGROUND)
  (define (row->image es y)
    (for ([x W] [e es])
      (when (entry-taken? e) (set! im (image-block/scene x y (entry-id e) im)))))
  ;; --IN--
  (for ([y H] [row brd])
    (row->image (row-entries row) y))
  im)

;; ID Number -> Image
;; render a piece to be shown as the next piece
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

;; Window -> Image
;; render a window
(define (image-render-window w)
  (define rpanel (image-rpanel w))
  (define borpic (image-piece/scene (game-piece (window-game w))
                                    (window-board-image w)))
  (beside/align "top" borpic rpanel))

;; Window -> Image
;; render the right panel
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

;; Number -> Image
;; render a score
(define (image-score sc)
  (define rect (rectangle 200 100 'solid "WhiteSmoke"))
  (define txt (text (number->string sc) 30 "black"))
  (overlay txt rect))

;; Number -> Image
;; render a level
(define (image-level n)
  (define rect (rectangle 200 50 'solid "LightGray"))
  (define str (string-append "Level " (number->string n)))
  (define txt (text str 20 "black"))
  (overlay txt rect))
