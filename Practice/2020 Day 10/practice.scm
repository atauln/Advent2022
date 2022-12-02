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
                        (cond 
                            ((equal? val ")") 3)
                            ((equal? val "]") 57)
                            ((equal? val "}") 1197)
                            ((equal? val ">") 25137)
                        )
                    )
                )
                ((opening? val)
                    (if (>= (+ index 1) (string-length line))
                        ;(cond 
                        ;    ((equal? val "(") 3)
                        ;    ((equal? val "[") 57)
                        ;    ((equal? val "{") 1197)
                        ;    ((equal? val "<") 25137)
                        ;)
                        0
                        (line-corrupted? line (+ index 1) (list index val))
                    )
                )
            )
        )
    )
)

(define (evaluate-source score index)
    (if (>= index (length INPUT))
        score
        (let ((result (line-corrupted? (list-ref INPUT index) 0 '(1 "["))))
            (println result)
            (evaluate-source (+ score result) (+ index 1))
        )
    )
)


(display (evaluate-source 0 0))
(newline)