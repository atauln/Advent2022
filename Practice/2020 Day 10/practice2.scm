#!/usr/bin/racket
#lang scheme

(require racket/base)

(define INPUT (string-split (file->string "source.txt") "\n"))

(define (opposite? s) 
    (cond
        ((equal? s "[")
            "]"
        )
        ((equal? s "]")
            "["
        )
        ((equal? s "(")
            ")"
        )
        ((equal? s ")")
            "("
        )
        ((equal? s "{")
            "}"
        )
        ((equal? s "}")
            "{"
        )
        ((equal? s "<")
            ">"
        )
        ((equal? s ">")
            "<"
        )
    )
)

(define (opening? s) 
    (if (or (equal? s "[") (equal? s "(") (equal? s "{") (equal? s "<"))
        #t
        #f
    )
)

(define (value? s)
    (cond
        ((equal? s "(") 1)
        ((equal? s "[") 2)
        ((equal? s "{") 3)
        ((equal? s "<") 4)
    )
)

(define (complete-line line score index)
    (if (< index 0)
        score
        (let ((val (opposite? (string (string-ref line index)))))
            (complete-line 
                line
                (+ (* score 5) (value? (opposite? val)))
                (- index 1)
            )
        )
    )
)

(define (line-corrupted? line index waiting-for) 
    (println (string-append (number->string index) " " (number->string (list-ref waiting-for 0)) " " (list-ref waiting-for 1) " " line))
    (if (>= index (string-length line))
        (if (> (string-length line) 0)
            (line-corrupted? line (- index 1) (list (- index 2) (string (string-ref line (- index 2)))))
            0
        )
        (let ((val (string (string-ref line index))))
            (cond
                ((= index 0)
                    (line-corrupted? line (+ index 1) (list index val))
                )
                ((not (opening? val))
                    (if (equal? (opposite? val) (list-ref waiting-for 1))
                        (line-corrupted? (string-append (substring line 0 (- index 1)) (substring line (+ index 1) (string-length line))) (- index 1) (list (- index 2) (string (string-ref line (- index 2)))))
                        0
                    )
                )
                ((opening? val)
                    (if (>= (+ index 1) (string-length line))
                        (complete-line line 0 (- (string-length line) 1))
                        (line-corrupted? line (+ index 1) (list index val))
                    )
                )
            )
        )
    )
)

(define (evaluate-source scores index)
    (if (>= index (length INPUT))
        (list-ref (sort scores <) (/ (- (length scores) (modulo (length scores) 2)) 2))
        (let ((result (line-corrupted? (list-ref INPUT index) 0 '(1 "["))))
            (if (= result 0)
                (evaluate-source scores (+ index 1))
                (evaluate-source (append scores (list result)) (+ index 1))
            )
        )
    )
)


(display (evaluate-source '() 0))
(newline)   