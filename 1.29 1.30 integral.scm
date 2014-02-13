#lang scheme

; 1.29-1.30. Shows usage of higher-order functions (such as sum),
; that is used by other functions - to calculate integrals
(require racket/trace)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))



(define (inc i) (+ i 1))
(define (identity1 x) x)
(define (sum-integers a b)
  (sum identity1 a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


; (* 8 (pi-sum 1 1000))






(define (cube a) (* a a a))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(trace sum)

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)) )))
  (trace iter)
  (iter a 0 ))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (integral-simpson f a b n)
  (define (h) (/ (- b a) n))
  (define (a_ k) (+ a (* k (h))))
  (define (even? x) (= (remainder x 2) 0))
  
  (define (coeff k)
    (cond
      ((= k 0)   1)
      ((= k n)   1)
      ((even? k) 2)
      (else 4)
      )
    )
  
  (define (element k) (* (coeff k) (f (a_ k))) )
  
  (* (sum element 0 inc n) (/ (h) 3) )
  )


(integral-simpson cube 2 4 10)
; (integral cube 2 4 0.5)