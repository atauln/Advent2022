#!/usr/bin/racket
#lang scheme

(require racket/base)
(require racket/set)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define (visible-left? n sum line index)
    (if (< index 0)
        sum
        (let ((val-at-point (string->number (string (string-ref line index)))))
            (if (<= n val-at-point)
                (+ sum 1)
                (visible-left? n (+ sum 1) line (- index 1))
            )
        )
    )
)

(define (visible-right? n sum line index)
    (if (>= index (string-length line))
        sum
        (let ((val-at-point (string->number (string (string-ref line index)))))
            (if (<= n val-at-point)
                (+ sum 1)
                (visible-right? n (+ sum 1) line (+ index 1))
            )
        )
    )
)

(define (visible-up? n sum v_index index)
    (if (< v_index 0)
        sum
        (let ((val-at-point (string->number (string (string-ref (list-ref INPUT v_index) index)))))
            (if (<= n val-at-point)
                (+ sum 1)
                (visible-up? n (+ sum 1) (- v_index 1) index)
            )
        )
    )
)

(define (visible-down? n sum v_index index)
    (if (>= v_index (length INPUT))
        sum
        (let ((val-at-point (string->number (string (string-ref (list-ref INPUT v_index) index)))))
            (if (<= n val-at-point)
                (+ sum 1)
                (visible-down? n (+ sum 1) (+ v_index 1) index)
            )
        )
    )
)

(define (get-scenic-score n v_index index product run)
    (cond
        ((= run 0)
            (get-scenic-score
                n
                v_index
                index
                (* product (visible-left? n 0 (list-ref INPUT v_index) (- index 1)))
                (+ run 1)
            )
        )
        ((= run 1)
            (get-scenic-score
                n
                v_index
                index
                (* product (visible-right? n 0 (list-ref INPUT v_index) (+ index 1)))
                (+ run 1)
            )
        )
        ((= run 2)
            (get-scenic-score
                n
                v_index
                index
                (* product (visible-up? n 0 (- v_index 1) index))
                (+ run 1)
            )
        )
        ((= run 3)
            (get-scenic-score
                n
                v_index
                index
                (* product (visible-down? n 0 (+ v_index 1) index))
                (+ run 1)
            )
        )
        (else product)
    )
)

(define (sol2 highest-product v-index index)
    (cond
        ((>= v-index (length INPUT))
            highest-product
        )
        (else 
            (cond
                ((>= index (string-length (list-ref INPUT v-index)))
                    (sol2 highest-product (+ v-index 1) 0)
                )
                (else (let ((result (get-scenic-score (string->number (string (string-ref (list-ref INPUT v-index) index))) v-index index 1 0)))
                    (if (> result highest-product)
                        (sol2 result v-index (+ index 1))
                        (sol2 highest-product v-index (+ index 1))
                    )
                ))
            )
        )
    )
)

(println (sol2 0 0 0))