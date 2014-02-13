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

(phi)

; 1.36
(define (func)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (func_aver)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))) ) 2.0))

; (func)
; (func_aver)

; 1.37

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k) 0
        (/ (n i) (+ (d i) (iter (+ i 1)) ) ) )
    )
  (/ 1 (iter 1))
)

; finding 1/phi using continued fraction:
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)