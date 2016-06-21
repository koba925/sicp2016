#lang racket

(define (prime? next)
  (lambda (n) (= n (smallest-divisor n next))))

(define (smallest-divisor n next)
  (find-divisor n 2 next))

(define (find-divisor n test-divisor next)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor) next))))

(define (slow-next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 1)))

(define (fast-next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder a b) 0))

(define (timed-prime? test? n)
  (let ((start-time (current-inexact-milliseconds)))
    (if (test? n)
        (- (current-inexact-milliseconds) start-time)
        #f)))

(define (search-for-primes test? lower count)
  (define (iter n found total-time)
    (define (next elapsed-time)
      (cond (elapsed-time
             ;(printf "~a : ~a~n" n elapsed-time)
             (iter (+ n 2) (+ found 1) (+ total-time elapsed-time)))
            (else
             (iter (+ n 2) found total-time))))
    (if (= found count)
        total-time
        (next (timed-prime? test? n))))
  (/ (iter lower 0 0) count))

; オーバーヘッド測定用の何もしないテスト
(define (dummy n) #t)

(define (average-time times test? lower count)
  (define (iter c total-time)
    (if (= c times)
        total-time
        (iter (+ c 1) (+ total-time (search-for-primes test? lower count)))))
  (/ (iter 0 0) times))

(define (slow-fast-ratio n)
  
  ;(printf "~nn=~a~n" n)
  (let ((d (average-time 10 dummy n 3))
        (s (average-time 10 (prime? slow-next) n 3))
        (f (average-time 10 (prime? fast-next) n 3)))
    ;(printf "d=~a~n" d)
    ;(printf "s=~a~n" s)
    ;(printf "f=~a~n" f)
    ;(printf "s/f=~a~n" (/ s f))
    ;(printf "(s-d)/(f-d)=~a~n" (/ (- s d) (- f d)))
    (/ (- s d) (- f d))))

(define (average-ratio times n)
  (define (iter c total-time)
    (if (= c times)
        total-time
        (iter (+ c 1) (+ total-time (slow-fast-ratio n)))))
  (/ (iter 0 0) times))

(define (ex1-23)

  ; 1回目の実行に時間がかかるため一度実行しておく
  (search-for-primes dummy 2 1)
  (search-for-primes (prime? slow-next) 2 1)
  (search-for-primes (prime? fast-next) 2 1)

  (printf "   1001 : ~a~n" (average-ratio 10 1001))
  (printf "  10001 : ~a~n" (average-ratio 10 10001))
  (printf " 100001 : ~a~n" (average-ratio 10 100001))
  (printf "1000001 : ~a~n" (average-ratio 10 1000001)))

(ex1-23)
