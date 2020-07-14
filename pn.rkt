#lang racket

(define (pn inputPn startRow)
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
  (pn 1 inputPn startRow)
)

(pn "1" "12345678")
(pn "12" "12345678")
(pn "34" "12345678")
(pn "1478" "12345678")
(pn "8" "12345678")
(pn "x" "12345678")
(pn "x" "1234567")

(curry pn "14")

(define method '("x" "14" "x" "14" "x" "14" "x" "12"))
method

;;(define testos (map (lambda (x) (string-append x "P")) method))
;;testos


(define methodFuncs (map (lambda (x) (curry pn x)) method))
methodFuncs

((list-ref methodFuncs 0) "1234")


(define twoPNs (compose (list-ref methodFuncs 1) (list-ref methodFuncs 0)))
(twoPNs "1234")


;;(define comptestall (compose methodFuncs))

(define singleLead (apply compose (reverse methodFuncs)))
 
(singleLead "1234")

