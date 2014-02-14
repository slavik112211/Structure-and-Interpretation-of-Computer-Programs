#lang racket
(require racket/trace)

(define (square a) (* a a))

(define (fl x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))


(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))


;(+ (let ((x 3))
;      (+ x (* x 10)))
;   5)



; (cos 0.7390822985224023)


(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  ; (trace f)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iter)
    (let ((next (f guess)))
      (display iter)(display ": ")(display next)(newline)
      (if (close-enough? guess next)
          next
          (try next (+ iter 1)))))
  (try first-guess 1))


; (fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)


(define (average x y) (/ (+ x y) 2))
(trace average)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))


;(sqrt 16)

; 1.35
(define (phi)
  (fixed-point (lambda (phi) (+ 1 (/ 1 phi))) 1.0))

; (phi)

; 1.36
(define (func)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (func_aver)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))) ) 2.0))

; (func)
; (func_aver)

; 1.37

(define (cont-frac-recursive n d k i)
    (if (> i k) 0
        (/ (n i) (+ (d i) (cont-frac-recursive n d k (+ i 1)) ) ) )
)
;(trace cont-frac-recursive)

(define (cont-frac-iterative n d k)
  (define (iter i result)
    (if (< i 1) result
        (iter (- i 1) (/ (n i) (+ (d i) result)) ) )
    )
  (trace iter)
  (iter k 0)
  )
  
; finding 'phi' (golden ratio) using finite continued fraction:
(define (phi2)
  (/ 1
  (cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) 12 1)
  )
)
;(phi2)

(define (phi3)
  (/ 1
  (cont-frac-iterative (lambda (i) 1.0) (lambda (i) 1.0) 12)
  )
)
;(phi3)

; 1.38 finding e - Euler constant

(define (euler_d i)
  (cond
    ((= i 2) 2)
    ((= (remainder (- i 2.0) 3) 0) (+ (euler_d (- i 3)) (euler_d (- i 2)) (euler_d (- i 1))))
    (else 1)
        ) )
(trace euler_d)
;(euler_d 1) ; 1
;(euler_d 2) ; 2
;(euler_d 5) ; 4
;(euler_d 7) ; 1
;(euler_d 11) ; 8
;(euler_d 13) ; 1
;(euler_d 26) ; 18

(define (euler)
  (+ 2 (cont-frac-recursive (lambda (i) 1.0) euler_d 20 1)))

;(euler)

; 1.39 Calculating tangent by J.H. Lambert

(define (cont-frac-recursive-oper n d k i oper)
    (if (> i k) 0
        (/ (n i) (oper (d i) (cont-frac-recursive-oper n d k (+ i 1) oper) ) ) )
)

(define (tan-cf x k)
  (define (tan-n i) (if (= i 1) x (* x x)) )
  (define (tan-d i) (if (= i 1) 1.0 (+ 2 (tan-d (- i 1))) ) )
  (cont-frac-recursive-oper tan-n tan-d k 1 -)
  )

(tan-cf -1 20)
(tan-cf 0 20)
(tan-cf 1 20)

(tan-cf 2 10)
(tan-cf 3 10)
(tan-cf 4 10)