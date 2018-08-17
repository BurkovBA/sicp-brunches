#lang racket

; this is implementation of backtracking algorithm in Lisp for 8-queens problem

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
           (map (lambda (new-row)
                 (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (-k 1))))))
   (queen-cols board-size))

(define (safe? k postions)
 (filter
  (lambda (position) (= k-position (length positions)-(index-of position positions)))
  (available-positions positions k)
 )
)

(define (adjoin-position))

(define (enumerate-interval low high)
  (if (> low high)
   nil
   (cons low (enumerate-interval (+ low 1) high))
  )
)

(define (available-positions queen-cols k)
  (filter
   (lambda (index) (safe? index queen-cols))
   (difference (enumerate-interval 1 k) queen-cols)
  )
)

(define (difference bigger-list smaller-list) (
 (if (in? car(bigger-list) smaller-list)
  (difference cdr(bigger-list) smaller-list)
  (list car(bigger-list) difference(cdr(bigger-list) smaller-list))
 )
))

(define (in? element list) (
 (if (equals? list (nil))
  'false
  (if (equals? element (car list))
   'true
   (in? element (cdr list))
  )
 )
))
