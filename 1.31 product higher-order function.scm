#lang racket
(require racket/trace)

; 1.31
; a.  The [sum] procedure is only the simplest of a vast number of similar abstractions 
; that can be captured as higher-order procedures. Write an analogous procedure 
; called [product] that returns the product of the values of a function at points 
; over a given range. Show how to define [factorial] in terms of product. 

; b.  If your [product] procedure generates a recursive process, write one that 
; generates an iterative process. If it generates an iterative process, write one 
; that generates a recursive process. 

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))
; (trace product-recursive)

(define (product_iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)) )))
  (trace iter)
  (iter a 1 ))

(define (func x) (* 3 x))
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (identity x) x)

; (product func 2 inc 6)

(define (factorial x) (product_iterative identity 1 inc x))
(factorial 5)


; Also use product to compute approximations to Pi using the formula by John Wallis
; Pi / 4 = (2 · 4 · 4 · 6 · 6 · 8...) / (3 · 3 · 5 · 5 · 7 · 7...)

(define (even? x) (= (remainder x 2) 0))
(define (numerator x)
  (if (even? x)
      (+ x 2)
      (+ x 1)))

(define (denominator x)
  (if (even? x)
      (+ x 1)
      (+ x 2)))

(define (pi)
  (* 4 (/ (product_iterative numerator 1 inc 1000) (product_iterative denominator 1 inc 1000)) )
  )

(exact->inexact (pi))