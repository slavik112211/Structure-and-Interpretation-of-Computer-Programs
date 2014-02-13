(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (plus1 a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b)) ))
; (plus1 4 5)
; (inc (+ (dec 4) 5))
; (inc (+ (- 4 1) 5))
; (+ (+ (- 4 1) 5) 1)

(define (plus2 a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b)) ))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
