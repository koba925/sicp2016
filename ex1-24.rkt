#lang racket

(define (ex1-24)
  ;例によって一度やっておく
  (timed-prime? (lambda (n) (fast-prime? n 1)) 2)

  (reported-timed-prime? 1009)
  (reported-timed-prime? 1013)
  (reported-timed-prime? 1019)
  (reported-timed-prime? 10007)
  (reported-timed-prime? 10009)
  (reported-timed-prime? 10037)
  (reported-timed-prime? 100003)
  (reported-timed-prime? 100019)
  (reported-timed-prime? 100043)
  (reported-timed-prime? 1000003)
  (reported-timed-prime? 1000033)
  (reported-timed-prime? 1000037))

(define (reported-timed-prime? n)
  (printf "~a : ~a~n"
          n
          (timed-prime? (lambda (n) (fast-prime? n 1000000)) n)))

(define (timed-prime? test? n)
  (let ((start-time (current-inexact-milliseconds)))
    (if (test? n)
        (- (current-inexact-milliseconds) start-time)
        #f)))

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square x) (* x x))

; (ex1-24)

(define (ex1-24-2)
  (let ((d (timed-prime? (lambda (n) (dummy-fast-prime? n 1000000)) 1009))
        (t1 (timed-prime? (lambda (n) (fast-prime? n 1000000)) 1009))
        (t2 (timed-prime? (lambda (n) (fast-prime? n 1000000)) 1000003)))
    (printf "d     = ~a~n" d)
    (printf "t1    = ~a~n" t1)
    (printf "t2    = ~a~n" t2)
    (printf "t2/t1 = ~a~n" (/ t2 t1))
    (printf "(t2-d)/(t1-d) = ~a~n" (/ (- t2 d) (- t1 d)))))

(define (dummy-fast-prime? n times)
    (cond ((= times 0) true)
          ((dummy-fermat-test n) (dummy-fast-prime? n (- times 1)))
          (else false)))

(define (dummy-fermat-test n)
  (define (try-it a)
    (= (dummy-expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (dummy-expmod base exp m) base)

(ex1-24-2)