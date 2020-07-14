#lang racket

(define (pn startIndex inputPn startRow)
  (cond
    ;; finishing condition
    [(> startIndex (string-length startRow)) ""]
    ;; possible finish condition, and catches 'badly formed' PN with implicit places, e.g. '1' on even stages
    [(= startIndex (string-length startRow)) (substring startRow (- startIndex 1) startIndex)]
    [else
     ;; does this place in row have a 'make place' in the PN?
     (if (string-contains? inputPn (number->string startIndex))
         ;; yes, so make a place
         (string-append
          ;; the number at that pos in the row
          (substring startRow (- startIndex 1) startIndex)
          ;; append recursive application of this function
          (pn (+ startIndex 1) inputPn startRow))
         ;; no, so swap the first two items in string
         (string-append
          (string-append
           (substring startRow startIndex (+ startIndex 1))
           (substring startRow (- startIndex 1) startIndex))
          (pn (+ startIndex 2) inputPn startRow))
     )]
    )
  )

(pn 1 "1" "12345678")
(pn 1 "12" "12345678")
(pn 1 "34" "12345678")
(pn 1 "1478" "12345678")
