#lang racket

(require memoize)
(require racket/promise)

#|

Playground for delayed/lazy execution

|#

;; the act of defining this doens't run the body
(define (writ)
  (writeln "writ being evaluated")
  "writ-return-val"
  )

;; body is called each time you call the function
(writ)
(writ)


;; the act of defining this doens't run the body
(define/memo (writ2)
  (writeln "writ2 being evaluated")
  "writ2-return-val"
  )

;; body is only called once due to define/memo - the result is cached.
;; Note that the return value is cached and returned every time
(writ2)
(writ2)


#|
(define (writ3)
  (delay (lambda () writeln "writX"))
)
|#


#|
;; can't do this - no expression in begin
(define (param-promise x)
  (define writ4
    (delay
      (writeln "writ4 being evaluated")
      (writeln x)
      "writ4-return-val"
      )
    )
)
|#

(define writ3
  (delay
    (writeln "writ3 being evaluated")
    "writ3-return-val"
    )
  )

;; returns #f to indicate it's not a promise
(promise? writ2)

;; returns #t to indicate it's a promise
(promise? writ3)

;; results in error - you cannot call a promise as if it's a regular function
;;(writ3)

;; forces the promise to execute the code the first time; caches the result.
;; note that we can't pass in any parameter
(force writ3)
(force writ3)
