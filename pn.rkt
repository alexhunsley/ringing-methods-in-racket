#lang racket

(define (pn startRow startIndex inputPn)
  (cond
    ;; finishing condition
    [(> startIndex (string-length startRow)) ""]
    ;; possible finish condition, and catches 'badly formed' PN with implicit places, e.g. '1' on even stages
    [(= startIndex (string-length startRow)) (substring startRow (- startIndex 1) startIndex)]
    [else
     ;; if this place in row has a 'make place' in the PN:
     (if (string-contains? inputPn (number->string startIndex))
         (string-append
          ;; the number at that pos in the row
          (substring startRow (- startIndex 1) startIndex)
          ;; append recursive application of this function
          (pn startRow (+ startIndex 1) inputPn))
         ;; swap the first two items in string
         (string-append
          (string-append
           (substring startRow startIndex (+ startIndex 1))
           (substring startRow (- startIndex 1) startIndex))
          (pn startRow (+ startIndex 2) inputPn))
     )]
    )
  )

(pn "12345678" 1 "1")
(pn "12345678" 1 "12")
(pn "12345678" 1 "34")
(pn "12345678" 1 "1478")
