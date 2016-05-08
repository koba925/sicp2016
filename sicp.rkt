#lang racket

(require rackunit)

; 1.1.4 Compound Procedures

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))

(check-eq? (f 5) 136)
