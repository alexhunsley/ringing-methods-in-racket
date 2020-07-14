#lang racket

(define (remove-dups l)
  (cond
   [(empty? l) empty]
   [(empty? (rest l)) l]
   [else
    (let ([i (first l)])
      (if (equal? i (first (rest l)))
          (remove-dups (rest l))
          (cons i (remove-dups (rest l)))))]))

(remove-dups (list "a" "b" "b" "b" "c" "c"))

(define (pn startRow res startIndex inputPn)
  (cond
    ;; finishing condition
    #;[(> startIndex (string-length startRow)) "E"]
    ;; catches 'badly formed' PN with implicit places, e.g. '1' on even stages
    [(= startIndex (string-length startRow)) (substring startRow (- startIndex 1) startIndex)]
    [else
     ;; if this place in row has a 'make place' in the PN:
     (if (string-contains? inputPn (number->string startIndex))
         (string-append
          ;; the number at that pos in the row
          (substring startRow (- startIndex 1) startIndex)
          ;; append recursive application of this function
          (pn startRow res (+ startIndex 1) inputPn))
         ;; swap the first two items in string
         (string-append
          (string-append
           (substring startRow startIndex (+ startIndex 1))
           (substring startRow (- startIndex 1) startIndex))
          (pn startRow res (+ startIndex 2) inputPn))
     )]
    )
  )

#;(pn "1234" "" 0 "14")

#;(pn "1234" "a" 4 "14")

(pn "1234" "a" 1 "1")
