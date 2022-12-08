#!/usr/bin/racket
#lang scheme

(require racket/base)
(require racket/set)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define (visible-horizontal? n max-ind line index)
    (if (>= index max-ind)
        #t
        (let ((val-at-point (string->number (string (string-ref line index)))))
            (if (<= n val-at-point)
                #f
                (visible-horizontal? n max-ind line (+ index 1))
            )
        )
    )
)

(define (visible-vertical? n max-ind ind index)
    (if (>= index max-ind)
        #t
        (let ((val-at-point (string->number (string (string-ref (list-ref INPUT index) ind)))))
            (if (<= n val-at-point)
                #f
                (visible-vertical? n max-ind ind (+ index 1))
            )
        ) 
    )
)

(define (trees-visible-in-row sum line ind index)
    (if (>= index (string-length line))
        sum
        (let ((val-at-point (string->number (string (string-ref line index)))))
            (println val-at-point)
            (println (visible-horizontal? val-at-point (string-length line) line (+ index 1)))
            (if (or (visible-horizontal? val-at-point index line 0) (visible-horizontal? val-at-point (string-length line) line (+ index 1)) (visible-vertical? val-at-point (length INPUT) index (+ ind 1)) (visible-vertical? val-at-point ind index 0))
                (trees-visible-in-row (+ sum 1) line ind (+ index 1))
                (trees-visible-in-row sum line ind (+ index 1))
            )
        )
    )
)

(define (sol1 sum ind)
    (println "-------------------------------------------------")
    (if (>= ind (length INPUT))
        sum
        (sol1 (+ sum (trees-visible-in-row 0 (list-ref INPUT ind) ind 0)) (+ ind 1))
    )
)


(println (sol1 0 0))