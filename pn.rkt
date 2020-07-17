#lang racket


(define (pn inputPn startRow prevRows)
  (define (innerPn startIndex inputPn startRow)
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
            (innerPn (+ startIndex 1) inputPn startRow))
           ;; no, so swap the first two items in string
           (string-append
            (string-append
             (substring startRow startIndex (+ startIndex 1))
             (substring startRow (- startIndex 1) startIndex))
            (innerPn (+ startIndex 2) inputPn startRow))
           )]
      )
    )

  (define resultRow (innerPn 1 inputPn startRow))

  ;; Use 'values' to return muliple values!
  ;; if we use 'list', it breaks 'composition' call
  (values resultRow (append prevRows (list resultRow)))
)

;; Our place notation (PB Minimus)
(define method '("x" "14" "x" "14" "x" "14" "x" "12"))

;; List of functions that apply each place notate
;; (Note: we will have duplicates here - no caching/memoization)
(define methodFuncs (map (lambda (notate) (curry pn notate)) method))

;; Composition of the notate functions that applies
;; a single lead of the method
(define singleLead (apply compose (reverse methodFuncs)))

;; Composition of the single lead function to do multiple leads
(define (apply-n-leads n single-lead-func)
  (apply compose (make-list n single-lead-func))
;;  (apply compose get-second-item (make-list n single-lead-func))
  )

;; Generate entire method (3 leads)
;; (Need to get rid of the repeated '1234' here.)
((apply-n-leads 3 singleLead) "1234" '("1234"))

;;(singleLead "1234" '("1234"))

(define (get-second-item items)
  (list-ref 1 items)
  )
  
;;(define wholeMethod (apply compose (make-list 3 singleLead)))
;; or can use compose:
;;((compose singleLead singleLead singleLead) "1234" '("1234")) 

;;(define wholeMethod (apply-n-leads 3 singleLead))
;;(wholeMethod  "1234" '("1234"))

;; or run directly, like:

;;((apply-n-leads 3 singleLead) "1234" '("1234"))

;;(get-second-item "s" "g")

;;(get-second-item ((apply-n-leads 3 singleLead) "1234" '("1234")))


;;;;;;;;;;;;;;;; chaff

;;(define testos (map (lambda (x) (string-append x "P")) method))
;;testos


;;(define twoPNs (compose (list-ref methodFuncs 1) (list-ref methodFuncs 0)))
;;(twoPNs "1234")


;;(define (pnFinal inputPn startRow prevRows)
;;  (define result (pn inputPn startRow prevRows))
;;  (list-ref result 1)
;;  )

;;(pn "14" '("12345678") "12345678")

;;(pn "12" "12345678")
;;(pn "34" "12345678")
;;(pn "1478" "12345678")
;;(pn "8" "12345678")
;;(pn "x" "12345678")
;;(pn "x" "1234567")

;;(curry pn "14")

