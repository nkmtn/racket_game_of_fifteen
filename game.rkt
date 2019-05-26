;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Racket implementation of classic Game of Fifteen
;;;;
;;;; Nika Motina <nkmtn10@gmail.com>

#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Game configuration

;;; State of the world
(struct world
  (board-size
   bricks
   brick-pixel-size)
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary functions

(define (sq arg)
  (* arg arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gane of Fifteen logic

;; Calculates a block row in a grid n x n
(define (brick-row pos size)
  (quotient pos size))

;; Calculates a block column in a grid n x n
(define (brick-col pos size)
  (remainder pos size))

;; Generates a list of position numbers of neighboring blocks
(define (neighbor-bricks brick-pos size)
  (filter (λ (pos)
            (= (+ (abs (- (brick-row brick-pos size) (brick-row pos size)))
                  (abs (- (brick-col brick-pos size) (brick-col pos size)))) 1))
          (build-list (sq size) values)))

;; Generates a list with a single element - the position number of the empty block
(define (bricks-to-swap-with bricks brick-pos size)
  (filter (λ (pos) (= (list-ref bricks pos) (sq size)))
          (neighbor-bricks brick-pos size)))

;; Exchange two items given by indexes in the list
(define (swap-in-list lst pos1 pos2)
  (cond [(= pos1 pos2) lst]
        [else
         (append
          (take lst (min pos1 pos2))
          (list (list-ref lst (max pos1 pos2)))
          (take (drop lst (add1 (min pos1 pos2)))
                (sub1 (- (max pos1 pos2) (min pos1 pos2))))
          (list (list-ref lst (min pos1 pos2)))
          (drop lst (add1 (max pos1 pos2))))]))

;; Calculates the state after the next move
(define (move-brick bricks brick-pos size)
  (let ((swap-with (bricks-to-swap-with bricks brick-pos size)))
    (cond [(empty? swap-with) bricks]
          [else (swap-in-list bricks brick-pos (first swap-with))])))

;; Calculates a state after moving an empty cell
(define (move-empty bricks size direction)
  (let ([empty-pos (index-of bricks (sq size))])
    (let ([to-row (+ (brick-row empty-pos size) (last direction))]
          [to-col (+ (brick-col empty-pos size) (first direction))])
      (cond
        [(and (>= to-row 0) (>= to-col 0) (< to-row size) (< to-col size))
          (swap-in-list bricks empty-pos (+ (* to-row size) to-col))]
        [else bricks]))))

;; Checks if the puzzle is already folded
(define (game-done w)
  (let ([size (world-board-size w)] [bricks (world-bricks w)])
    (equal? bricks (build-list (sq size) (λ(x) (add1 x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing functions

;; Calculates the y-coordinate of the block
(define (brick-y pos bps size)
  (+ (quotient bps 2) (* (brick-row pos size) bps)))

;; Calculates the x-coordinate of the block.
(define (brick-x pos bps size)
  (+ (quotient bps 2) (* (brick-col pos size) bps)))

;; Draws one block, size * size corresponds to an empty square
(define (draw-brick pos number scene bps size)
  (let ([y (brick-y pos bps size)] [x (brick-x pos bps size)])
  (cond
    [(= number (sq size))
      ; Блок с номером size * size не отрисовывается, это просто белый квадрат
      (place-image (square bps "solid" "white") x y scene)]
    [else
      ; Обычный блок - это белый текст с номером на синем фоне                       
      (place-image/align
      (text (number->string number) (quotient (- bps 4) 2) "white")
      x y "center" "center"
      (place-image (square (- bps 4) "solid" "slateblue") x y scene))])))

;; Draws the scene
(define (game-scene size bps)
  (square (* size bps) "solid" "white"))
  
;; Renders all blocks recursively
(define (draw-bricks w)
  (let ([size (world-board-size w)]
        [bricks (world-bricks w)]
        [bps (world-brick-pixel-size w)])
    (define (draw-brick-cur current-position remaining-bricks scene )
      (cond [(null? remaining-bricks) scene]
            [else
              (draw-brick current-position
                          (first remaining-bricks)
                          (draw-brick-cur (add1 current-position)
                                          (cdr remaining-bricks) scene) bps size)]))
    (draw-brick-cur 0 bricks (game-scene size bps))))

;; Draws the latest game state
(define (draw-last w)
  (let ([size (world-board-size w)]
        [bricks (world-bricks w)]
        [bps (world-brick-pixel-size w)])
    (place-image/align
      (text "You won!" (* 0.8 bps) "slateblue")
      (* 0.5 bps size) (* 0.5 bps size) "center" "center" (game-scene size bps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control

;; Calculate the position at which the mouse clicked
(define (position-clicked x y bps size)
  (+ (* (quotient y bps) size) (quotient x bps)))

;; Check whether the mouse click was made on a non-empty cell
(define (nonempty-clicked? size bricks pos)
  (not (= (list-ref bricks pos) (sq size))))

;; Processing a mouse click
(define (mouse-handler w x y event)
  (let ([size (world-board-size w)]
        [bricks (world-bricks w)]
        [bps (world-brick-pixel-size w)])
    (let ([brick-pos (position-clicked x y bps size)])
      (cond [(and
              (string=? event "button-down")
              (nonempty-clicked? size bricks brick-pos))
             (struct-copy world w [bricks (move-brick bricks brick-pos size)])]
            [else w]))))

;; Keystroke processing
(define (key-handler w k)
  (define (handle direction)
    (struct-copy world w
                 [bricks (move-empty (world-bricks w)
                                     (world-board-size w)
                                     direction)]))  
  (cond
    [(key=? k "up") (handle '(0 -1))]
    [(key=? k "down") (handle '(0 1))] 
    [(key=? k "left") (handle '(-1 0))]
    [(key=? k "right") (handle '(1 0))]
    [else w]))

;; Create a starting world
(define (new-game size bps #:test [test #f])
  (world size
         (if test
             (append (build-list (- (sq size) 2) add1)
                     (list (sq size))
                     (list (- (sq size) 1)))
             (shuffle (build-list (sq size) add1)))
         bps))
  
;; Starting game function
(define (start [size 4] #:brick-pixel-size [bps 100] #:test [test #f])
  (big-bang (new-game size bps #:test test)
    (on-mouse mouse-handler)
    (on-key key-handler)
    (on-draw draw-bricks)
    (stop-when game-done draw-last)))