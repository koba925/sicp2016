#lang racket

(require rackunit)

; 1.1.4 Compound Procedures

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))

(check-eq? (f 5) 136)

; 1.1.6 Conditional Expressions and Predicates

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
