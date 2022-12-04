#!/usr/bin/racket
#lang scheme

(require racket/base)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define (in? r1 r2 t1 t2)
    (cond
        ((= r1 t1) #t)
        ((< r1 t1) (>= r2 t2))
        ((> r1 t1) (<= r2 t2))
    )
)

(define (get-range s)
    (string-split s "-")
)

(define (get-values line)
    (let ((vals (string-split line ",")))
        (list 
            (string->number (list-ref (get-range (list-ref vals 0)) 0))
            (string->number (list-ref (get-range (list-ref vals 0)) 1))
            (string->number (list-ref (get-range (list-ref vals 1)) 0))
            (string->number (list-ref (get-range (list-ref vals 1)) 1))
        )
    )
)

(define (sol-1 index sum)
    (if (>= index (length INPUT))
        sum
        (let ((vals (get-values (list-ref INPUT index))))
            (if (in? (list-ref vals 0) (list-ref vals 1) (list-ref vals 2) (list-ref vals 3))
                (sol-1 (+ index 1) (+ sum 1))
                (sol-1 (+ index 1) sum)
            )
        )
    )
)

(display (sol-1 0 0))
(newline)