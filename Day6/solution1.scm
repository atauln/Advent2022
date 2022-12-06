#!/usr/bin/racket
#lang scheme

(require racket/base)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define (check-equal? q str-list index)
    (cond
        ((>= index (length str-list)) #t)
        ((= index q) (check-equal? q str-list (+ index 1)))
        ((equal? (list-ref str-list q) (list-ref str-list index)) #f)
        (else (check-equal? q str-list (+ index 1)))
    )
)

(define (unique? s index)
    (if (>= index (string-length s))
        #t
        (let ((str-list (filter (lambda (p) (> (string-length p) 0))(string-split s ""))))
            (if (check-equal? index str-list 0)
                (unique? s (+ index 1))
                #f
            )
        )
    )
)

(define (sol1 index)
    (if (unique? (substring (list-ref INPUT 0) index (+ index 14)) 0)
        (+ index 14)
        (sol1 (+ index 1))
    )
)

(println (sol1 0))