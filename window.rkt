#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)


;=======================================
; API
;=======================================
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
; (game-fall    g)
;
;

;=======================================
; Window
;=======================================

(define-struct window [game])













(big-bang
  INIT-GAME
 [on-tick tickh RATE]
 [on-key keyh]
 [to-draw drawh])