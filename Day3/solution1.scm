#!/usr/bin/racket
#lang scheme

(require racket/base)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define (get-char-value s index) 
    (let ((all-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
        (if (>= index (string-length all-chars))
            0
            (if (equal? s (string-ref all-chars index))
                (+ index 1)
                (get-char-value s (+ index 1))
            )
        )
    )
)

(define (in? s comp index) 
    (if (>= index (string-length comp))
        #f
        (if (equal? s (string-ref comp index))
            #t
            (in? s comp (+ index 1))
        )
    )
)

(define (find-duplicate index comp1 comp2)
    (if (>= index (string-length comp1))
        #f
        (if (in? (string-ref comp1 index) comp2 0)
            (string-ref comp1 index)
            (find-duplicate (+ index 1) comp1 comp2)
        )
    )
)

(define (half-string index comp1 comp2 input)
    (if (>= index (/ (string-length input) 2))
        (list comp1 comp2)
        (if (= index 0)
            (half-string (+ index 1) (string (string-ref input index)) (string (string-ref input (- (string-length input) (+ index 1)))) input)
            (half-string (+ index 1) (string-append comp1 (string (string-ref input index))) (string-append comp2 (string (string-ref input (- (string-length input) (+ index 1))))) input)
        )
    )
)

(define (sol index priority-sum)
    (if (>= index (length INPUT))
        priority-sum
        (sol (+ index 1) (+ priority-sum (get-char-value (find-duplicate 0 (list-ref (half-string 0 "" "" (list-ref INPUT index)) 0) (list-ref (half-string 0 "" "" (list-ref INPUT index)) 1) ) 0)))
    )
)



(display (sol 0 0))
(newline)