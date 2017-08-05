;;=======================================
;; Helpers
;;=======================================

(check-false (id? #f))
(check-false (id? 1000))
(check-true (id? 0))
(check-true (id? 1))


;;=======================================
;; Block
;;=======================================

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

(check-equal? (min-x (list (b 3 30) (b 4 20) (b 1 10) (b 2 15))) 1) 
(check-equal? (min-y (list (b 3 30) (b 4 20) (b 1 10) (b 2 15))) 10) 
(check-equal? (max-x (list (b 3 30) (b 4 20) (b 1 10) (b 2 15))) 4)
(check-equal? (max-y (list (b 3 30) (b 4 20) (b 1 10) (b 2 15))) 30)

(check-equal? (shift-top-left (list (b 1 2) (b 1 3) (b 1 4) (b 2 3)))
              (list (b 0 0) (b 0 1) (b 0 2) (b 1 1)))


;;=======================================
;; Game pieces
;;=======================================

(check-equal? (global-piece-color 1) P1-COLOR)
(check-equal? (global-piece-blocks 0 2) (list (b 0 1) (b 1 1) (b 2 1) (b 1 2)))
(check-equal? (global-piece-type# 1) 1)


;;=======================================
;; Piece
;;=======================================

(check-pred piece? (piece-new))

(check-equal? (piece-left (piece 0 0 2 5)) (piece 0 0 1 5))
(check-equal? (piece-right (piece 0 0 2 5)) (piece 0 0 3 5))
(check-equal? (piece-down-n (piece 0 0 2 5) 3) (piece 0 0 2 8))
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

(check-equal? (piece-width 0 0) 3)
(check-equal? (piece-width 0 1) 2)
(check-equal? (piece-width 1 0) 2)
(check-equal? (piece-width 2 0) 4)
(check-equal? (piece-width 2 1) 1)
(check-equal? (piece-width 5 0) 3)
(check-equal? (piece-width 5 1) 2)

(check-equal? (piece-height 0 0) 2)
(check-equal? (piece-height 0 1) 3)
(check-equal? (piece-height 1 0) 2)
(check-equal? (piece-height 2 0) 1)
(check-equal? (piece-height 2 1) 4)
(check-equal? (piece-height 5 0) 2)
(check-equal? (piece-height 5 1) 3)







;(define (piece-height p)
;  (define raw-bs (piece->blocks p))
;  (define bs (shift-top-left raw-bs))
;  (add1 (max-y bs)))





;;=======================================
;; Entry
;;=======================================

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




;; □ □ □ □ □
;; □ □ □ □ □
;; □ □ □ □ □
;; □ □ □ □ ■
;; ■ ■ □ ■ ■
;; □ ■ □ □ □


(check-equal? (board-fall-count board-ex-2 (piece 0 3 3 0)) 0)
(check-equal? (board-fall-count board-ex-2 (piece 0 3 2 0)) 1)
(check-equal? (board-fall-count board-ex-2 (piece 0 3 1 -2)) 4)
(check-equal? (board-fall-count board-ex-2 (piece 3 0 1 -1)) 2)
(check-equal? (board-fall-count board-ex-2 (piece 3 0 0 -1)) 3)


;;=======================================
;; Game
;;=======================================

;;=======================================
;; Window
;;=======================================

(check-pred window? (window-new))


;;=======================================
;; Big-bang
;;=======================================

;;=======================================
;; Drawing images
;;=======================================

;;=======================================
;; Drawing Window
;;=======================================






