#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)




;;
;; DRAWING
;;
(define (piece-draw/scene p scene)
  (define bls (piece->valid-blocks p))
  (blocks-draw/scene bls (piece-id p) scene))




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



