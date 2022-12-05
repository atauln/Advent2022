#!/usr/bin/racket
#lang scheme

(require racket/base)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define (move stack1 stack2 amount num)
    (list (drop stack1 amount) (append (take stack1 amount) stack2))
)

(define (get-num-items line)
    (/ (+ (string-length line) 1) 4)
)

(define (get-item line stack_num)
    (string (string-ref line (+ (* (- stack_num 1) (/ (+ (string-length line) 1) (get-num-items line))) 1)))
)

(define (get-list-of-lines lines index)
    (if (equal? (string-ref (list-ref INPUT index) 1) #\1)
        lines
        (get-list-of-lines (append lines (list (list-ref INPUT index))) (+ index 1))
    )
)

(define (fill-stack complete-list lines stack_num index)
    (if (>= index (length lines))
        complete-list
        (if (not (equal? (get-item (list-ref lines index) stack_num) " "))
            (fill-stack (append complete-list (list (get-item (list-ref lines index) stack_num))) lines stack_num (+ index 1))
            (fill-stack complete-list lines stack_num (+ index 1))
        )
    )
)

(define stack-lines (get-list-of-lines '() 0))
(define num_of_stacks (get-num-items (list-ref INPUT 0)))

(define (get-master-list master_list num_stacks stack)
    (if (> stack num_stacks)
        master_list
        (get-master-list (append master_list (list (fill-stack '() stack-lines stack 0))) num_stacks (+ stack 1))
    )
)

(define (apply-instructions new-master-list master-list list1 list2 num_to_move stack)
    (cond
        ((> stack (length master-list)) new-master-list)
        ((= stack list1) (apply-instructions (append new-master-list (list (list-ref (move (list-ref master-list (- list1 1)) (list-ref master-list (- list2 1)) num_to_move 0) 0))) master-list list1 list2 num_to_move (+ stack 1)))
        ((= stack list2) (apply-instructions (append new-master-list (list (list-ref (move (list-ref master-list (- list1 1)) (list-ref master-list (- list2 1)) num_to_move 0) 1))) master-list list1 list2 num_to_move (+ stack 1)))
        (else (apply-instructions (append new-master-list (list (list-ref master-list (- stack 1)))) master-list list1 list2 num_to_move (+ stack 1)))
    )
)

(define (sol1 master-list index)
    (if (>= index (length INPUT))
        master-list
        (let ((instructions (string-split (list-ref INPUT index))))
            (sol1 (apply-instructions '() master-list (string->number (list-ref instructions 3)) (string->number (list-ref instructions 5)) (string->number (list-ref instructions 1)) 1) (+ index 1))
        )
    )
)

(println (sol1 (get-master-list '() num_of_stacks 1) 10))