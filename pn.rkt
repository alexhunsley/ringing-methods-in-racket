#lang racket
(require br/debug)
(require memoize)

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

(define/memo (rev-compose funcs) 
  (apply compose (reverse methodFuncs)))

;; Composition of the notate functions that applies
;; a single lead of the method
(define singleLead (rev-compose methodFuncs))

;; Composition of the single lead function to do multiple leads
(define/memo (apply-n-leads n single-lead-func)
  (apply compose (make-list n single-lead-func))
  )

;; Generate entire method (3 leads)
;; (Need to get rid of the repeated '1234' here.)
;;(report ((apply-n-leads 3 singleLead) "1234" '("1234")))

;;(define first-row-and-rows ((apply-n-leads 3 singleLead) "1234" '("1234")))

#;(define allRows (let-values ([(resultRow allRows) first-row-and-rows])
  allRows))

;;(list allRows)

#;(define (replace lst str rep)
  (map (lambda (x) (if (string=? x str) rep x)) 
       lst))

(define (string-replace input-string from-char)
  (list->string
   (for/list ((item (string->list input-string)))   
     (if (equal? item from-char)
         #\*
         #\ ))))

(define (outputListData list)
  (cond 
    [(null? list) #f]             ; actually doesn't really matter what we return
    [else (printf "~s\n" (string-replace (first list) #\2))    ; display the first item ...
          (outputListData (rest list))])) ; and start over with the rest


(let-values ([(resultRow allRows) ((apply-n-leads 3 singleLead) "1234" '("1234")) ])
  (outputListData allRows))
  ;;(list resultRow allRows))resultRow allRows



;;(let-values ([(x y) (quotient/remainder 10 3)]))