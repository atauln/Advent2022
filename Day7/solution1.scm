#!/usr/bin/racket
#lang scheme

(require racket/base)
(require racket/set)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define file-structure '("/"))

(define (get-content content index)
    (if (>= index (length INPUT))
        content
        (if (equal? (string-ref (list-ref INPUT index) 0) #\$)
            content
            (let ((line-list (string-split (list-ref INPUT index) " ")))
                (if (equal? (list-ref line-list 0) "dir")
                    (get-content (append content (list (list-ref line-list 1))) (+ index 1))
                    (get-content (append content (list (cons (list-ref line-list 1) (string->number (list-ref line-list 0))))) (+ index 1))
                )
            )
        )
    )
)

(define (expand-structure HEAD new-structure structure new-items index)
    (if (>= index (length structure))
        new-structure
        (if (equal? (list-ref structure index) HEAD)
            (expand-structure HEAD (append new-structure (list (cons HEAD (list new-items)))) structure new-items (+ index 1))
            (if (list? (list-ref structure index))
                (expand-structure HEAD (append new-structure (list (expand-structure (list-ref (list-ref structure index) 0) (list) (list-ref (list-ref structure index) 1) new-items 0)) structure new-items (+ index 1)))
                (expand-structure HEAD (append new-structure (list (list-ref structure index))) structure new-items (+ index 1))
            )
        )
    )
)

(define (sol1 HEAD structure index)
    (println structure)
    (if (>= index (length INPUT))
        structure
        (let ((line-list (string-split (list-ref INPUT index))))
            (cond
                ((equal? (list-ref line-list 0) "$")
                    (cond
                        ((equal? (list-ref line-list 1) "ls")
                            (sol1 
                                HEAD 
                                (expand-structure 
                                    HEAD 
                                    '()
                                    structure 
                                    (get-content 
                                        (list) 
                                        0
                                    ) 
                                    0
                                ) 
                                (+ index 1)
                            )
                        )
                        ((equal? (list-ref line-list 1) "cd")
                            (println (list-ref line-list 2))
                            (sol1 (list-ref line-list 2) structure (+ index 1))
                        )
                    )
                )
                (else (sol1 HEAD structure (+ index 1)))
            )
        )
    )
)


(println (sol1 "/" '("/") 0))