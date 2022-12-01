#!/usr/bin/racket
#lang scheme

(require racket/base)

(define elves-list (string-split (file->string "source.txt") "\n"))

(define (sum-higher? max-sums sum index) 
    (if (>= index (length max-sums))
        #f
        (if (> sum (list-ref max-sums index))
            #t
            (sum-higher? max-sums sum (+ index 1))
        )
    )
)

(define (reorganize-max-sums max-sums sum)
    (cons sum  (cdr (sort max-sums <)))
)

(define (read_elf_cals max-sums cur_sum index file) 
    (if (< index (length file))
        (if (equal? (list-ref file index) "")
            (if (sum-higher? max-sums cur_sum 0)
                (read_elf_cals (reorganize-max-sums max-sums cur_sum) 0 (+ index 1) file)
                (read_elf_cals max-sums 0 (+ index 1) file)
            )
            (read_elf_cals max-sums (+ cur_sum (string->number (list-ref file index))) (+ index 1) file)
        )
        max-sums
    )
)

(define (sum max-sums index cur_sum)
    (if (>= index (length max-sums))
        cur_sum
        (sum max-sums (+ index 1) (+ cur_sum (list-ref max-sums index)))
    )
)

(display (sum (read_elf_cals '(0 0 0) 0 0 elves-list) 0 0))

(newline)