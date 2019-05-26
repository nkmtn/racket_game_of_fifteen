# racket_game_of_fifteen
Game of Fiften. Written in a family owned Lisp / Scheme - Racket.

	Launch options:
* `(start)` -- classic 5x5 game
* `(start 6)` -- you can change number of blocks in row and column respectively(defolt is 5)
* `(start 5 #:test true)` -- state of the world with one step to win
* `(start 5 #:brick-pixel-size 80 #:test #t)` -- you can change bricks pixels size (no screen size restrictions)
