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
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
;  (printf "~a ~a ~a ~n" (square guess) x (- (square guess) x))
;  (printf "~a ~a ~a ~n" guess (/ x guess) (average guess (/ x guess)))
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

; Exercise 1.7.

; (sqrt 0.0001)
; (square (sqrt 0.0001))

;(sqrt 100)
;(sqrt 1000)
;(sqrt 10000)
;(sqrt 100000)
;(sqrt 1000000)
;(sqrt 10000000)
;(sqrt 100000000)
;(sqrt 1000000000000)
;(sqrt 10000000000000)

; (define (good-enough? guess x)
;   (< (/ (abs (- (improve guess x) guess)) guess) 0.0000001))

; Exercise 1.8.

(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (cbrt-good-enough? guess x)
      guess
      (cbrt-iter (cbrt-improve guess x) x)))

(define (cbrt-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x)
  (* x x x))

(check-= (cube (cbrt 3)) 3 0.001)

; 1.1.8 Procedures as Black-Box Abstractions

(define (square-2 x) (exp (double (log x))))
(define (double x) (+ x x))

(check-= (square-2 3) 9 0.001)

(define (sqrt-blockstructure x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(check-= (square (sqrt-blockstructure 2)) 2 0.001)

; 1.2.1 Linear Recursion and Iteration

;(define (factorial n)
;  (if (= n 1)
;      1
;      (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product) (+ counter 1) max-count)))
(check-eq? (factorial 1) 1)
(check-eq? (factorial 3) 6)

; Exercise 1.9.

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(check-eq? (inc 3) 4)
(check-eq? (dec 3) 2)

;(define (o+ a b)
;  (if (= a 0)
;      b
;      (inc (o+ (dec a) b))))

(define (o+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(check-eq? (o+ 4 5) 9)
