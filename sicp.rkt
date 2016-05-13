#lang racket

(require rackunit)

; 1.1.4 Compound Procedures

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))

(check-eq? (f 5) 136)

; 1.1.6 Conditional Expressions and Predicates

;(define (abs x)
;  (cond ((= x 0) 0)
;        ((> x 0) x)
;        ((< x 0) (- x))))

;(define (abs x)
;  (cond ((< x 0) (- x))
;        (else x)))

(define (abs x)
  (if (< x 0) (- x) x))

(check-eq? (abs 2) 2)
(check-eq? (abs -2) 2)
(check-eq? (abs 0) 0)

; Exercise 1.3.
(define (sum-of-squares-of-top-2 a b c)
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
        ((and (>= a b) (>= c b)) (sum-of-squares a c))
        (else (sum-of-squares b c))))

(check-eq? (sum-of-squares-of-top-2 2 2 2) 8)
(check-eq? (sum-of-squares-of-top-2 3 2 2) 13)
(check-eq? (sum-of-squares-of-top-2 2 3 2) 13)
(check-eq? (sum-of-squares-of-top-2 2 2 3) 13)
(check-eq? (sum-of-squares-of-top-2 3 3 2) 18)
(check-eq? (sum-of-squares-of-top-2 3 2 3) 18)
(check-eq? (sum-of-squares-of-top-2 2 3 3) 18)
(check-eq? (sum-of-squares-of-top-2 1 2 3) 13)
(check-eq? (sum-of-squares-of-top-2 1 3 2) 13)
(check-eq? (sum-of-squares-of-top-2 2 1 3) 13)
(check-eq? (sum-of-squares-of-top-2 2 3 1) 13)
(check-eq? (sum-of-squares-of-top-2 3 1 2) 13)
(check-eq? (sum-of-squares-of-top-2 3 2 1) 13)

; Exercise 1.4.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(check-eq? (a-plus-abs-b 3 -2) 5)
(check-eq? (a-plus-abs-b 3 0) 3)
(check-eq? (a-plus-abs-b 3 2) 5)

; Exercise 1.5.
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))
; (test 0 (p)) ; 実行すると無限ループ

; 1.1.7 Example: Sqare Roots by Newton's Method

(define (sqrt x)
  (sqrt-iter 1 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(check-= (square (sqrt 2.0)) 2.0 0.001)

; Exercise 1.6.

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(check-eq? (new-if (= 2 3) 0 5) 5)
(check-eq? (new-if (= 1 1) 0 5) 0)

;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x) x)))

; (sqrt 2.0) ; 実行すると無限ループ
