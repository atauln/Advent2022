#!/usr/bin/racket
#lang scheme

(require racket/base)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define TOKENS (map (lambda (p) (string-split p " ")) INPUT))

(define (play-game user-score index) 
    (if (< index (length TOKENS))
        (let ((enemy-value (list-ref (list-ref TOKENS index) 0)) (user-value (list-ref (list-ref TOKENS index) 1)) (next-index (+ index 1)))
            (cond 
                ((equal? user-value "X")
                    (
                        (cond 
                            ((equal? enemy-value "A") 
                                (
                                    (play-game (+ user-score 3) next-index)
                                ))
                            ((equal? enemy-value "B") 
                                (
                                    (play-game (+ user-score 1) next-index)
                                ))
                            ((equal? enemy-value "C") 
                                (
                                    (play-game (+ user-score 2) next-index)
                                ))
                        )
                    )
                )
                ((equal? user-value "Y") 
                    (
                        (cond 
                            ((equal? enemy-value "A") 
                                (
                                    (play-game (+ user-score 4) next-index)
                                ))
                            ((equal? enemy-value "B") 
                                (
                                    (play-game (+ user-score 5) next-index)
                                ))
                            ((equal? enemy-value "C") 
                                (
                                    (play-game (+ user-score 6) next-index)
                                ))
                        )
                    )
                )
                ((equal? user-value "Z") 
                    (
                        (cond 
                            ((equal? enemy-value "A") 
                                (
                                    (play-game (+ user-score 8) next-index)
                                ))
                            ((equal? enemy-value "B") 
                                (
                                    (play-game (+ user-score 9) next-index)
                                ))
                            ((equal? enemy-value "C") 
                                (
                                    (play-game (+ user-score 7) next-index)
                                ))
                        )
                    )
                )
            )
        )
        (display #\newline)
    )
    user-score
)
(display (play-game 0 0))
(newline)