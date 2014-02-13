#lang racket
(require racket/trace)
; 1.32 accumulator

(define (accumulate_recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate_recursive combiner null-value term (next a) next b))))
(trace accumulate_recursive)

(define (accumulate_iterative combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)) )))
  (trace iter)
  (iter a null-value ))

(define (product_recursive term a next b)
  (accumulate_recursive * 1 term a next b))

(define (product_iterative term a next b)
  (accumulate_iterative * 1 term a next b))

(define (sum_recursive term a next b)
  (accumulate_recursive + 0 term a next b))

(define (sum_iterative term a next b)
  (accumulate_iterative + 0 term a next b))

(define (triple x) (* x 3))
(define (square x) (* x x))
(define (inc x)    (+ x 1))
(define (even? x)  (= (remainder x 2) 0))
(define (identity x) x)

;(sum_iterative triple 1 inc 3) ;18
;(sum_recursive triple 1 inc 3) ;18

;(product_iterative triple 1 inc 3) ;162
;(product_recursive triple 1 inc 3) ;162


; 1.33 filtered accumulator
; a. the sum of the squares of the prime numbers in the interval a to b
; (assuming that you have a prime? predicate already written)

(define (filtered_accumulate_recursive filter combiner null-value term a next b)
  ;(trace combiner)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (filtered_accumulate_recursive filter combiner null-value term (next a) next b)))
  )

(define (filtered_accumulate_iterative filter combiner null-value term a next b)
  (define (iter a result)
    ;(write result)
    ;(newline)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter a b) (term a) null-value)) )))
  ;(trace iter)
  (iter a null-value ))


(define (smallest-divisor n)
  (define (iter k)
    (cond ((> (square k) n) n)
          ((= (remainder n k) 0) k)
          (else (iter (inc k))) ))
  ; (trace iter)
  (iter 2))

; (smallest-divisor 17)

(define (prime? n)
  (define (prime) (= n (smallest-divisor n)))
  ; (when (prime) (write (string-append "prime found: " (number->string n))) )
  (prime)
  )

; (prime? 7)

(trace filtered_accumulate_iterative)
;(filtered_accumulate_iterative prime? + 0 square 1 inc 40)

;b. the product of all the positive integers less than n that are 
; relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1). 

; greatest common divisor, Euclid's algorithm
(define (gcd a b)
  (cond ((= b 0) a)
    (else (gcd b (remainder a b))) ))

; (gcd 12 10)

(define (relative-prime? i n) (= (gcd i n) 1))
; (trace relative-prime?)
;(relative-prime? 4 51)

(filtered_accumulate_iterative relative-prime? * 1 identity 1 inc 14)