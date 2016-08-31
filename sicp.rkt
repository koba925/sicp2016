#lang racket

(require rackunit)

; 1.1.4 Compound Procedures

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
;(define (f a) (sum-of-squares (+ a 1) (* a 2)))

;(check-eq? (f 5) 136)

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

;(define (sqrt x)
;  (sqrt-iter 1.0 x))

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

; (check-= (square (sqrt 2.0)) 2.0 0.001)

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

; Exercise 1.10.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(check-eq? (A 1 10) 1024)
(check-eq? (A 2 4) 65536)
(check-eq? (A 3 3) 65536)

;(define (f n) (A 0 n))
;(define (g n) (A 1 n))
;(define (h n) (A 2 n))

;(f 1)
;(f 2)
;(f 3)
;(f 4)
;
;(g 1)
;(g 2)
;(g 3)
;(g 4)
;
;(h 1)
;(h 2)
;(h 3)
;(h 4)

; 1.2.2 Tree Recursion

;(define (fib n)
;  (cond ((= n 0) 0)
;        ((= n 1) 1)
;        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(check-eq? (fib 0) 0)
(check-eq? (fib 1) 1)
(check-eq? (fib 2) 1)
(check-eq? (fib 6) 8)

; 1.2.2 Tree Recursion
; Example: Counting change

(define (count-change amount)
  (cc amount 5))

;(define (cc amount kinds-of-coins)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (= kinds-of-coins 0)) 0)
;        (else (+ (cc amount
;                     (- kinds-of-coins 1))
;                 (cc (- amount
;                        (first-denomination kinds-of-coins))
;                     kinds-of-coins)))))
;
;(define (first-denomination kinds-of-coins)
;  (cond ((= kinds-of-coins 1) 1)
;        ((= kinds-of-coins 2) 5)
;        ((= kinds-of-coins 3) 10)
;        ((= kinds-of-coins 4) 25)
;        ((= kinds-of-coins 5) 50)))

;(check-eq? (count-change 100) 292)
;
;(define step 0)
;
;(define (count-change2 amount)
;  (set! step 0)
;  (cc2 amount 5)
;  step)
;
;(define (cc2 amount kinds-of-coins)
;  (set! step (+ step 1))
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (= kinds-of-coins 0)) 0)
;        (else (+ (cc2 amount
;                      (- kinds-of-coins 1))
;                 (cc2 (- amount
;                         (first-denomination kinds-of-coins))
;                      kinds-of-coins)))))
;
;(check-eq? (count-change2 11) 55)
;
;(define (list-cc n)
;  (define (iter c)
;    (display (count-change2 c)) (newline)
;    (cond ((< c n) (iter (+ c 1)))))
;  (iter 0))

; (list-cc 100)

; Exercise 1.11.

(define (f-1-11-r n)
  (cond ((< n 3) n)
        (else (+ (f-1-11-r (- n 1))
                 (f-1-11-r (- n 2))
                 (f-1-11-r (- n 3))))))

(check-eq? (f-1-11-r 0) 0)
(check-eq? (f-1-11-r 1) 1)
(check-eq? (f-1-11-r 2) 2)
(check-eq? (f-1-11-r 3) 3)
(check-eq? (f-1-11-r 4) 6)
(check-eq? (f-1-11-r 5) 11)

(define (f-1-11-i n)
  (define (iter a b c count)
    (cond ((= count 0) c)
          (else (iter (+ a b c) a b (- count 1)))))
  (iter 2 1 0 n))

(check-eq? (f-1-11-i 0) 0)
(check-eq? (f-1-11-i 1) 1)
(check-eq? (f-1-11-i 2) 2)
(check-eq? (f-1-11-i 3) 3)
(check-eq? (f-1-11-i 4) 6)
(check-eq? (f-1-11-i 5) 11)

; Exercise 1.12.

(define (pascal n k)
  (cond ((or (= k 1) (= k n)) 1)
        (else (+ (pascal (- n 1) (- k 1))
                 (pascal (- n 1) k)))))

(check-eq? (pascal 1 1) 1)
(check-eq? (pascal 3 1) 1)
(check-eq? (pascal 3 3) 1)
(check-eq? (pascal 5 3) 6)

; Exercise 1.15.

(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(check-= (sine 12.15) (sin 12.15) 0.01)

; 1.2.4 Exponentiation

;(define (expt b n)
;  (if (= n 0)
;      1
;      (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

(check-eq? (expt 3 0) 1)
(check-eq? (expt 3 3) 27)

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(check-eq? (expt 3 0) 1)
(check-eq? (expt 3 3) 27)
(check-eq? (expt 3 4) 81)

; Exercise 1.16

(define (fast-expt-ex1-16 b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (* b b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(check-eq? (fast-expt-ex1-16 5 7) (fast-expt 5 7))

; Excercise 1.17.

(define (o* a b)
  (if (= b 1)
      a
      (+ a (o* a (- b 1)))))

(check-eq? (o* 5 7) 35)

;(define (double n) (* n 2)) ; 定義済み
(define (halve n) (/ n 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))

(check-eq? (fast-* 5 7) 35)

; Exercise 1.18.

(define (fast-iter-* a b)
  (define (iter c a b)
    (cond ((= b 0) c)
          ((even? b) (iter c (double a) (halve b)))
          (else (iter (+ c a) a (- b 1)))))
  (iter 0 a b))

(check-eq? (fast-iter-* 5 7) 35)

; Exercise 1.19.

(define (fib-ex1-19 n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* 2 p q) (* q q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(check-eq? (fib-ex1-19 20) (fib 20))

; 1.2.5 Greatest Common Divisors

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(check-eq? (gcd 206 40) 2)
(check-eq? (gcd 40 206) 2)

; 1.2.6 Example: Testing for Primality

(define (smallest-divisor n)
  (find-divisor n 2))
; Modified for exercise 1.23.
;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? n test-divisor)test-divisor)
;        (else (find-divisor n (+ test-divisor 1)))))
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor)test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder a b) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(check-eq? (prime? 53) #t)
(check-eq? (prime? 51) #f)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(check-eq? (expmod 3 5 100) 43)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

(check-eq? (fast-prime? 53 100) #t)
(check-eq? (fast-prime? 51 100) #f)

; Exercise 1.21.

(check-eq? (smallest-divisor 199) 199)
(check-eq? (smallest-divisor 1999) 1999)
(check-eq? (smallest-divisor 19999) 7)

; Exercise 1.22.

; テキストのコード
; 使えないので書き直す
;(define (timed-prime-test n)
;  (newline)
;  (display n)
;  (start-prime-test n (current-inexact-milliseconds)))
;(define (start-prime-test n start-time)
;  (when (prime? n)
;    (report-prime (- (current-inexact-milliseconds) start-time))))
;(define (report-prime elapsed-time)
;  (display " *** ")
;  (display elapsed-time))

(define (timed-prime? n)
  (define (start-timed-prime? start-time)
    (define (elapsed-time finished-time)
      (display n) (display " *** ") (display (- finished-time start-time))
      (newline)
      (- finished-time start-time))
    (if (prime? n)
        (elapsed-time (current-inexact-milliseconds))
        #f))
  (start-timed-prime? (current-inexact-milliseconds)))

(define (search-for-primes lower count)
  (define (iter n found total-time)
    (define (next elapsed-time)
      (cond (elapsed-time
             (iter (+ n 2) (+ found 1) (+ total-time elapsed-time)))
            (else
             (iter (+ n 2) found total-time))))
    (if (= found count)
        total-time
        (next (timed-prime? n))))
  (/ (iter lower 0 0) count))

;(search-for-primes 1001 3)
;(search-for-primes 10001 3)
;(search-for-primes 100001 3)
;(search-for-primes 1000001 3)

; Exercise 1.23.
; See also 1.2.6
; See also ex1-23.rkt

(define (timed-prime-overhead n)
  (define (start-timed-prime? start-time)
    (define (elapsed-time finished-time)
      (display n) (display " *** ") (display (- finished-time start-time))
      (newline)
      (- finished-time start-time))
    (elapsed-time (current-inexact-milliseconds)))
  (start-timed-prime? (current-inexact-milliseconds)))

;(timed-prime-overhead 2)

;(/ (- 0.004313151041666667 0.0009765625) (- 0.0030110677083333335 0.0009765625))
;(/ (- 0.012288411458333334 0.0009765625) (- 0.007405598958333333 0.0009765625))
;(/ (- 0.037272135416666664 0.0009765625) (- 0.019694010416666668 0.0009765625))
;(/ (- 0.11832682291666667 0.0009765625) (- 0.06005859375 0.0009765625))

; Exercise 1.24.
; See also ex1-24.rkt

; Exercise 1.25.

(define (expmod-ex1-25 base exp m)
  (remainder (fast-expt base exp) m))

;(let ((s (current-inexact-milliseconds)))
;  (expmod 99999 100001 7)
;  (- (current-inexact-milliseconds) s))
;(let ((s (current-inexact-milliseconds)))
;  (expmod-ex1-25 99999 100001 7)
;  (- (current-inexact-milliseconds) s))

(define (exp-time f)
  (let ((s (current-inexact-milliseconds)))
    (f)
    (- (current-inexact-milliseconds) s)))

;(exec-time (expmod-ex1-25 99999 100001 7))
;(exp-time (lambda () (expmod 99999 100001 7)))
;(exp-time (lambda () (expmod-ex1-25 99999 100001 7)))

; Exercise 1.26.

(define (expmod-ex1-26 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod-ex1-26 base (/ exp 2) m)
                       (expmod-ex1-26 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod-ex1-26 base (- exp 1) m))
                    m))))

(check-eq? (expmod 7 9 5) (expmod-ex1-26 7 9 5))

; Exercise 1.27.

(define (carmichael-test n)
  (define (iter a)
    (cond ((= n a) #t)
          ((= (expmod a n n) a) (iter (+ a 1)))
          (else #f)))
  (iter 1))

;(carmichael-test 2)
;(carmichael-test 3)
;(carmichael-test 4)
;(carmichael-test 5)
;(carmichael-test 6)
;(carmichael-test 7)
;(carmichael-test 8)
;(carmichael-test 9)
;(carmichael-test 10)
;(newline)
;(prime? 561)
;(carmichael-test 561)
;(prime? 1105)
;(carmichael-test 1105)
;(prime? 1729)
;(carmichael-test 1729)
;(prime? 2465)
;(carmichael-test 2465)
;(prime? 2821)
;(carmichael-test 2821)
;(prime? 6601)
;(carmichael-test 6601)

; Exercise 1.28.

(define (miller-rabin-test n)
  
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let* ((root (expmod base (/ exp 2) m))
                  (e (remainder (square root) m)))
             (if (and (not (= root 1))
                      (not (= root (- n 1)))
                      (= e 1))
                 0
                 e)))
          (else
           (remainder (* base (expmod base (- exp 1) m))
                      m))))
  
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-ex1-28? n times)
    (cond ((= times 0) true)
          ((miller-rabin-test n) (fast-prime-ex1-28? n (- times 1)))
          (else false)))

(define (miller-rabin-test-test to)
  (define (iter n)
    (when (not (eq? (fast-prime-ex1-28? n 100)
                    (fast-prime? n 100)))
      (printf "~a~n" n))
    (when (<= n to) (iter (+ n 1))))
  (iter 2))

; (miller-rabin-test-test 6601)

; 1.3.1 Procedures as Arguments

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(check-eq? (sum-integers 1 10) 55)
(check-eq? (sum-integers 3 6) 18)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(check-eq? (sum-cubes 1 10) (square 55))
(check-eq? (sum-cubes 2 4) 99)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;(* (pi-sum 1 3) 8)
;(* (pi-sum 1 30) 8)
;(* (pi-sum 1 300) 8)
;(* (pi-sum 1 3000) 8)
;(* (pi-sum 1 30000) 8)
;(* (pi-sum 1 300000) 8)

(define (sum term a next b)
  ;(printf "a:~a~n" a)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes-h a b)
  (sum cube a inc b))

;(sum-cubes-h 1 10)
;(sum-cubes-h 2 4)

(define (identity x) x)
(define (sum-integers-h a b)
  (sum identity a inc b))

;(sum-integers-h 1 10)
;(sum-integers-h 3 6)

(define (pi-sum-h a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;(* (pi-sum-h 1 3) 8)
;(* (pi-sum-h 1 30) 8)
;(* (pi-sum-h 1 300) 8)
;(* (pi-sum-h 1 3000) 8)
;(* (pi-sum-h 1 30000) 8)
;(* (pi-sum-h 1 300000) 8)

(define (integral f a b dx)
  (define (add-dx a) (+ a dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

;(integral cube 0 1 0.01)
;(integral cube 0 1 0.001)

; Exercise 1.29.

(define (my-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-dx a) (+ a h))
  (printf "a:~a~n" a)
  (printf "b:~a~n" b)
  (printf "h:~a~n" h)
  (printf "(f a):~a~n" (f a))
  (printf "(f b):~a~n" (f b))
  (* (/ h 2) (+ (f a) (* 2 (sum f (+ a h) add-dx (- b h))) (f b))))

;(my-integral cube 0.0 1.0 100)
;(my-integral cube 0.0 1.0 1000)

(define (my-integral2 f a b n)
  (define h (/ (- b a) n))
  (define (iter c sum)
    (if (> c (- n 1))
        sum
        (iter (+ c 1) (+ sum (f (+ a (/ (* (- b a) c) n)))))))
  (* (/ h 2) (+ (f a) (* 2 (iter 1 0)) (f b))))

;(my-integral2 cube 0.0 1.0 100)

;(integral square 0.0 1.0 0.01)
;(my-integral2 square 0.0 1.0 100)

(define (simpson-integral f a b n)
  (define r (- b a))
  (define h (/ r n))
  (define (iter c sum)
    (define x (+ a (/ (* r c) n)))
    (define co (cond ((or (= c 0) (= c n)) 1)
                     ((odd? c) 4)
                     (else 2)))
    ;(printf "c co:~a ~a~n" c co)
    (if (> c n)
        sum
        (iter (+ c 1)
              (+ sum (* co (f x))))))
  (* (/ h 3) (iter 0 0)))

;(simpson-integral cube 0.0 1.0 10)
;(simpson-integral cube 0.0 1.0 100)
;(simpson-integral cube 0.0 1.0 1000)
;(simpson-integral cube 0.0 1.0 10000)
;(simpson-integral cube 0.0 1.0 100000)
;
;(simpson-integral (lambda (x) (* x x x x)) 0.0 1.0 10)
;(simpson-integral (lambda (x) (* x x x x)) 0.0 1.0 100)
;(simpson-integral (lambda (x) (* x x x x)) 0.0 1.0 1000)
;(simpson-integral (lambda (x) (* x x x x)) 0.0 1.0 10000)
;(simpson-integral (lambda (x) (* x x x x)) 0.0 1.0 100000)
;
;(simpson-integral sin 0.0 pi 10)
;(simpson-integral sin 0.0 pi 100)
;(simpson-integral sin 0.0 pi 1000)
;(simpson-integral sin 0.0 pi 10000)
;(simpson-integral sin 0.0 pi 100000)

(define (simpson-integral-h f a b n)
  (define r (- b a))
  (define h (/ r n))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (f (+ a (/ (* r k) n)))))
  (* (/ h 3) (sum term 0 inc n)))

;(simpson-integral-h sin 0.0 pi 10)
;(simpson-integral-h sin 0.0 pi 100)
;(simpson-integral-h sin 0.0 pi 1000)
;(simpson-integral-h sin 0.0 pi 10000)
;(simpson-integral-h sin 0.0 pi 100000)

; Exercise 1.30.

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-cubes-i a b)
  (sum-i cube a inc b))

;(sum-cubes-i 1 10)
;(sum-cubes-i 2 4)

(define (sum-integers-i a b)
  (sum-i identity a inc b))

;(sum-integers-i 1 10)
;(sum-integers-i 3 6)

(define (pi-sum-i a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-i pi-term a pi-next b))

;(* (pi-sum-i 1 3) 8)
;(* (pi-sum-i 1 30) 8)
;(* (pi-sum-i 1 300) 8)
;(* (pi-sum-i 1 3000) 8)
;(* (pi-sum-i 1 30000) 8)
;(* (pi-sum-i 1 300000) 8)

(define (integral-i f a b dx)
  (define (add-dx a) (+ a dx))
  (* (sum-i f (+ a (/ dx 2)) add-dx b) dx))

;(integral-i cube 0 1 0.01)
;(integral-i cube 0 1 0.001)

; Exercise 1.31.

(define (product term a next b)
  ;(printf "~a ~a ~a~n" a (term a) b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial-p n)
  (product identity 1 inc n))

;(factorial-p 1)
;(factorial-p 2)
;(factorial-p 3)

(define (pi-p n)
  (define (term n)
    (square (/ (+ (* 2 n) 2.0) (+ (* 2 n) 1.0))))
  (/ (* 4 2 (product term 1 inc n))
     (+ (* 2 n) 3)))

;(pi-p 10)
;(pi-p 100)
;(pi-p 1000)
;(pi-p 10000)

; Exercise 1.32.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; (accumulate + 0 cube 1 inc 3)
; (accumulate * 1 identity 1 inc 4)

(define (sum-a term a next b)
  (accumulate + 0 term a next b))
(define (product-a term a next b)
  (accumulate * 1 term a next b))

; (sum-a cube 1 inc 3)
; (product-a identity 1 inc 4)

(define (accumulate-i combiner null-value term a next b)
  (define (iter c ans)
    (if (> c b)
        ans
        (iter (next c) (combiner (term c) ans))))
  (iter a null-value))

; (accumulate-i + 0 cube 1 inc 3)
; (accumulate-i * 1 identity 1 inc 4)

(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter c ans)
    (if (> c b)
        ans
        (if (filter c)
            (iter (next c) (combiner (term c) ans))
            (iter (next c) ans))))
  (iter a null-value))

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

;(sum-of-squares-of-primes 3 6)

(define (product-of-relatively-primes n)
  (filtered-accumulate * 1 (lambda (m) (= (gcd n m) 1))
                       identity 1 inc n))

;(product-of-relatively-primes 8)

; 1.3.2 Constructing Procedures Using Lambda

(define (pi-sum-l a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

;(* (pi-sum-l 1 300000) 8)

(define (integral-l f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;(integral cube 0 1 0.01)
;(integral-l cube 0 1 0.01)
;(integral cube 0 1 0.001)
;(integral-l cube 0 1 0.001)

; Exercise 1.34.
;(define (f g) (g 2))

;(f square)
;(f (lambda (z) (* z (+ z 1))))
;(f f)

; 1.3.3 Procedures as General Methods

; Finding roots of equations by the half-interval method

(define (search f neg-point pos-point)
  (let ((midpoint (/ (+ neg-point pos-point) 2)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

; (search (lambda (x) (- (* x x) 4)) 0.0 3.0)

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;(half-interval-method (lambda (x) (- (* x x) 4)) 0.0 3.0)
;(half-interval-method (lambda (x) (- (* x x) 4)) 3.0 0.0)
;(half-interval-method (lambda (x) (- (* x x) 4)) -3.0 3.0)

;(half-interval-method sin 2.0 4.0)
;(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)

; Finding fixed points of functions

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    ; (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (fixed-point cos 1.0)
; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt-2 x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;(sqrt-2 2)

(define (sqrt-3 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;(sqrt-3 2)

; Exercise 1.35.

;(define φ (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;φ
;(+ 1 (/ 1 φ))

; Exercise 1.36.

; (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

; Exercise 1.37.

(define (cont-frac n d k)
  (define (C i)
    ; (printf "~a ~a ~a~n" i (n i) (d i))
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (C (+ i 1))))))
  (C 1))

;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1)
;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1000)

(define (get-n-digits a n)
  (let ((m (expt 10 n)))
    (/ (floor (* a m)) m)))

(define (count-for-accuracy ans f n)
  (let ((ans-n-digits (get-n-digits ans n)))
    (define (iter k)
      (if (= ans-n-digits
             (get-n-digits (f k) n))
          k
          (iter (+ 1 k))))
    (iter 1)))

;(count-for-accuracy (/ 1 (/ (+ 1 (sqrt 5)) 2))
;                    (lambda (k) (cont-frac (lambda (i) 1.0)
;                                           (lambda (i) 1.0)
;                                           k))
;                    10)

(define (cont-frac-i n d k)
  (define (iter i ans)
    (if (= i 0)
        ans
        (iter (- i 1) (/ (n k) (+ (d k) ans)))))
  (iter (- k 1) (/ (n k) (d k))))

;(cont-frac-i (lambda (i) 1.0) (lambda (i) 1.0) 1)
;(cont-frac-i (lambda (i) 1.0) (lambda (i) 1.0) 10)
;(cont-frac-i (lambda (i) 1.0) (lambda (i) 1.0) 100)
;(cont-frac-i (lambda (i) 1.0) (lambda (i) 1.0) 1000)

; Exercise 1.38.

;(define e (+ 2 (cont-frac (lambda (i) 1.0)
;                          (lambda (i)
;                            (if (= (remainder i 3) 2)
;                                (* 2 (/ (+ i 1) 3))
;                                1))
;                          10)))
;
;e

; Exercise 1.39.

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))

;(define qp (/ pi 4))
;(define qps (* qp qp))
;
;(/ qp 1)
;(tan-cf (/ pi 4) 1)
;
;(/ qp (- 1 (/ qps 3)))
;(tan-cf (/ pi 4) 2)
;
;(/ qp (- 1 (/ qps (- 3 (/ qps 5)))))
;(tan-cf (/ pi 4) 3)
;
;(tan-cf (/ pi 4) 30)
;(tan (/ pi 4))

; 1.3.4 Procedures as Returned Values

(define (average-damp f)
  (lambda (x) (average x (f x))))

; ((average-damp square) 10)

(define (sqrt-4 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; (sqrt-4 2.0)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

; (cube-root 3)

; Newton's method

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x(/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-5 x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

; (sqrt-5 2.0)

(define (cube-root-n x)
  (newtons-method (lambda (y) (- (cube y) x)) 1.0))

; (cube-root-n 3)

; Abstractions and first-class procedures

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-6 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

;(sqrt-6 2.0)

(define (sqrt-7 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;(sqrt-7 2.0)

; Exercise 1.40.

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;((cubic 2 3 4) 2)

; (newtons-method (cubic -1 -1 -2) 1.0)

; Exercise 1.41.

;(define (twice f) (lambda (x) (f (f x))))
(define twice (lambda (f) (lambda (x) (f (f x)))))

;(((twice (twice twice)) inc) 5)
;        ^^^^^^^^^^^^^
;        twiceに引数が与えられているから
;        (lambda (f) (lambda (x) (f (f x))))のfを
;        twiceで置き換える
;(((twice (lambda (x) (twice (twice x)))) inc) 5)
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
; twiceに引数が与えられているから
; (lambda (f) (lambda (x) (f (f x))))のfを
; (lambda (x) (twice (twice x)))で置き換える
;(((lambda (x) ((lambda (x) (twice (twice x))) ((lambda (x) (twice (twice x))) x))) inc) 5)
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  
; (lambda (x) ... (twice x)))のxをincに置き換える
;(((lambda (x) (twice (twice x))) ((lambda (x) (twice (twice x))) inc)) 5)
;
;(((lambda (x) (twice (twice x))) (twice (twice inc))) 5)
;(((lambda (x) (twice (twice x)))
;  (twice (lambda (x) (inc (inc x))))) 5)
;(((lambda (x) (twice (twice x)))
;  (lambda (x) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) x)))) 5)
;(((lambda (x) (twice (twice x)))
;  (lambda (x) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) x)))) 5)

; Exercise 1.42.

(define (compose f g)
  (lambda (x) (f (g x))))

; ((compose square inc) 6)

; Exercise 1.43.

;(define (repeated f n)
; (lambda (x)
;   (if (= n 0)
;       x
;       (f ((repeated f (- n 1)) x)))))

(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

; ((repeated square 2) 5)

; Exercise 1.44.

(define (average3 a b c) (/ (+ a b c) 3))
(define (smooth f)
  (lambda (x) (average3 (f (- x dx)) (f x) (f (+ x dx)))))
(define (nth-smooth f n)
  ((repeated smooth n) f))

;((smooth (lambda (x) x)) 3)
;((nth-smooth (lambda (x) x) 3) 3)
;
;(square 2)
;((smooth square) 2)
;((nth-smooth square 2) 2)
;((nth-smooth square 3) 2)
;((nth-smooth square 4) 2)
;((nth-smooth square 5) 2)

; Exercise 1.45.

;(define (times k e n)
;  (if (> e n)
;      (- k 1)
;      (times (+ k 1) (* e 2) n)))
;
;(let loop ((n 2))
;  (printf "~a ~a~n" n (times 1 2 n))
;  (when (< n 16) (loop (+ n 1))))

;(define (times n)
;  (if (< n 2)
;      0
;      (+ 1 (times (/ n 2)))))
;
;(let loop((n 2))
;  (printf "~a ~a~n" n (times n))
;  (when (< n 16) (loop (+ n 1))))

(define (nth-root n x)
  (define (times n)
    (if (< n 2)
        0
        (+ 1 (times (/ n 2)))))
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (times n))
                            1.0))

;(let loop ((n 2))
;  (printf "~a ~a~n" n (expt (nth-root n 2) n))
;  (when (< n 16) (loop (+ n 1))))

; Exercise 1.46.


(define (sqrt-8 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

(define (fixed-point-2 improve first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) tolerance))
  (define (iter guess)
    (if (good-enough? guess)
        (improve guess)
        (iter (improve guess))))
  (iter first-guess))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        (improve guess)
        (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (sqrt-9 x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (/ (+ guess (/ x guess)) 2)))
   1.0))

;(sqrt-9 2.0)

(define (fixed-point-3 f first-guess)
    ((iterative-improve
      (lambda (guess) (< (abs (- (f guess) guess)) tolerance))
      (lambda (guess) (f guess)))
     first-guess))

(define (sqrt-10 x)
  (fixed-point-3 (average-damp (lambda (y) (/ x y))) 1.0))

;(sqrt-10 2.0)

; 2.1.1 Example: Arithmetic Operations for Rational Numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;(define (make-rat n d) (cons n d))
;(define (make-rat n d)
;  (let ((g (gcd n d)))
;    (cons (/ n g) (/ d g))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
;(print-rat one-half)
(check-equal? one-half (cons 1 2))
(define one-third (make-rat 1 3))
;(print-rat (add-rat one-half one-third))
(check-equal? (add-rat one-half one-third) '(5 . 6))
;(print-rat (mul-rat one-half one-third))
(check-equal? (mul-rat one-half one-third) '(1 . 6))
;(print-rat (add-rat one-third one-third))
(check-equal? (add-rat one-third one-third) '(2 . 3))

; Exercise 2.1.

(check-equal? (make-rat 3 5) '(3 . 5))
(check-equal? (make-rat -3 5) '(-3 . 5))
(check-equal? (make-rat 3 -5) '(-3 . 5))
(check-equal? (make-rat -3 -5) '(3 . 5))

; 2.1.2 Abstraction Barriers

;(define (make-rat n d)
;  (cons n d))
;
;(define (numer x)
;  (let ((g (gcd (car x) (cdr x))))
;    (/ (car x) g)))
;
;(define (denom x)
;  (let ((g (gcd (car x) (cdr x))))
;    (/ (cdr x) g)))

; Exercise 2.2.

(define (make-segment sp ep) (cons sp ep))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

(define P1 (make-point 2 3))
(define P2 (make-point 4 7))
(define S1 (make-segment P1 P2))
; (print-point (midpoint-segment S1))

; Exercise 2.3.

;(define (make-rectangle p1 p2) (cons p1 p2))
;(define (make-rectangle P1 P2)
;  (cons (make-point (min (x-point P1) (x-point P2))
;                    (min (y-point P1) (y-point P2)))
;        (make-point (max (x-point P1) (x-point P2))
;                    (max (y-point P1) (y-point P2)))))
;(define (make-rectangle x1 x2 y1 y2)
;  (cons (make-point (min x1 x2) (min y1 y2))
;        (make-point (max x1 x2) (max y1 y2))))

(define (make-rectangle x1 x2 y1 y2)
  (cons (cons (min x1 x2) (min y1 y2))
        (cons (max x1 x2) (max y1 y2))))

(define (p1-rectangle r) (car r))
(define (p2-rectangle r) (cdr r))

(define (perimeter-rectangle r)
  (* 2 (+ (- (x-point (p2-rectangle r)) (x-point (p1-rectangle r)))
          (- (y-point (p2-rectangle r)) (y-point (p1-rectangle r))))))
(define (area-rectangle r)
  (* (- (x-point (p2-rectangle r)) (x-point (p1-rectangle r)))
     (- (y-point (p2-rectangle r)) (y-point (p1-rectangle r)))))

;(define (perimeter-rectangle r)
;  (* 2 (+ (abs (- (x-point (p1-rectangle r)) (x-point (p2-rectangle r))))
;          (abs (- (y-point (p1-rectangle r)) (y-point (p2-rectangle r)))))))
;(define (area-rectangle r)
;  (* (abs (- (x-point (p1-rectangle r)) (x-point (p2-rectangle r))))
;     (abs (- (y-point (p1-rectangle r)) (y-point (p2-rectangle r))))))

;(define (x1-rectangle r)
;  (min (x-point (car r)) (x-point (cdr r))))
;(define (x2-rectangle r)
;  (max (x-point (car r)) (x-point (cdr r))))
;(define (y1-rectangle r)
;  (min (y-point (car r)) (y-point (cdr r))))
;(define (y2-rectangle r)
;  (max (y-point (car r)) (y-point (cdr r))))

;(define (x1-rectangle r) (x-point (car r)))
;(define (x2-rectangle r) (x-point (cdr r)))
;(define (y1-rectangle r) (y-point (car r)))
;(define (y2-rectangle r) (y-point (cdr r)))

;(define (max a b) (if (> a b) a b))
;(define (min a b) (if (< a b) a b))

;(define (perimeter-rectangle r)
;  (* 2 (+ (- (x2-rectangle r) (x1-rectangle r))
;          (- (y2-rectangle r) (y1-rectangle r)))))
;
;(define (area-rectangle r)
;  (* (- (x2-rectangle r) (x1-rectangle r))
;     (- (y2-rectangle r) (y1-rectangle r))))

;(define R1
;  (make-rectangle (make-point 2 3) (make-point 4 7)))
;(define R1 (make-rectangle 2 4 3 7))
;(perimeter-rectangle R1)
;(area-rectangle R1)

;(define R2
;  (make-rectangle (make-point 4 3) (make-point 2 7)))
;(define R2 (make-rectangle 4 2 3 7))
;(perimeter-rectangle R2)
;(area-rectangle R2)

;(define R3
;  (make-rectangle (make-point 2 7) (make-point 4 3)))
;(define R3 (make-rectangle 2 4 7 3))
;(perimeter-rectangle R3)
;(area-rectangle R3)

;(define R4
;  (make-rectangle (make-point 4 7) (make-point 2 3)))
;(define R4 (make-rectangle 4 2 7 3))
;(perimeter-rectangle R4)
;(area-rectangle R4)

;2.1.3 What Is Meant by Data?

(define (cons2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
(define (car2 z) (z 0))
(define (cdr2 z) (z 1))

; (car2 (cdr2 (cons2 (cons2 1 2) (cons2 3 4))))

; Exercise 2.4.

(define (cons3 x y) (lambda (m) (m x y)))
(define (car3 z) (z (lambda (p q) p)))
(define (cdr3 z) (z (lambda (p q) q)))

; (car3 (cdr3 (cons3 (cons3 1 2) (cons3 3 4))))

; Exercise 2.5.

(define (cons4 x y) (* (expt 2 x) (expt 3 y)))
(define (cr z n)
  (if (= (remainder z n) 0)
      (+ 1 (cr (/ z n) n))
      0))
(define (car4 z) (cr z 2))
(define (cdr4 z) (cr z 3))

;(car4 (car4 (cons4 (cons4 3 4) (cons4 5 6))))
;(cdr4 (car4 (cons4 (cons4 3 4) (cons4 5 6))))
;(car4 (cdr4 (cons4 (cons4 3 4) (cons4 5 6))))
;(cdr4 (cdr4 (cons4 (cons4 3 4) (cons4 5 6))))

; Exercise 2.6.

(define zero
  (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(define one (add-1 zero))
(define two (add-1 one))

;((zero add1) 1)
;((one add1) 2)
;((two add1) 3)

;(add-1 zero)
;((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (f) (lambda (x) x)))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x)))
;
;(add-1 (add-1 zero))
;(add-1 (lambda (f) (lambda (x) (f x))))
;((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (f) (lambda (x) (f x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;(lambda (f) (lambda (x) (f (f x))))

; (define one (lambda (f) (lambda (x) (f x))))
; (define two (lambda (f) (lambda (x) (f (f x)))))

;(define (ch+ n m)
;  (if (zero? m)
;      n
;      (ch+ (add-1 n) (sub-1 m))))

(define (ch+ n m)
  ((n add-1) m))

; (((ch+ one two) add1) 1)

; 2.1.4 Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

;(define (div-interval x y)
;  (mul-interval x
;                (make-interval (/ 1.0 (upper-bound y))
;                               (/ 1.0 (lower-bound y)))))

; Exercise 2.7.

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;(add-interval (make-interval 0.9 1.1) (make-interval 9.9 10.1))
;(mul-interval (make-interval 0.9 1.1) (make-interval 9.9 10.1))
;(mul-interval (make-interval -0.9 -1.1) (make-interval 9.9 10.1))
;(div-interval (make-interval 0.9 1.1) (make-interval 9.9 10.1))

; Exercise 2.8.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;(sub-interval (make-interval 9.9 10.1) (make-interval 0.9 1.1))

; Exercise 2.10.

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;(div-interval (make-interval 0.9 1.1) (make-interval 9.9 10.1))
;(div-interval (make-interval 0.9 1.1) (make-interval -1.0 1.0))
;(div-interval (make-interval 0.9 1.1) (make-interval 0.0 1.0))

; Exercise 2.11.

(define (plus?-interval x) (> (lower-bound x) 0))
(define (minus?-interval x) (< (upper-bound x) 0))
(define (mul-interval-c x y)
  (cond ((plus?-interval x)
         (cond ((plus?-interval y)  ; [1,2]x[3,4]
                (make-interval (* (lower-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((minus?-interval y) ; [1,2]x[-4,-3]
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (upper-bound y))))
               (else                ; [1,2]x[-3,4]
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))))
        ((minus?-interval x)
         (cond ((plus?-interval y)  ; [-2,-1]x[3,4]
                (make-interval (* (lower-bound x) (upper-bound y))
                               (* (upper-bound x) (lower-bound y))))
               ((minus?-interval y) ; [-2,-1]x[-4,-3]
                (make-interval (* (upper-bound x) (upper-bound y))
                               (* (lower-bound x) (lower-bound y))))
               (else                ; [-2,-1]x[-3,4]
                (make-interval (* (lower-bound x) (upper-bound y))
                               (* (lower-bound x) (lower-bound y))))))
        (else
         (cond ((plus?-interval y)  ; [-1,2]x[3,4]
                (make-interval (* (lower-bound x) (upper-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((minus?-interval y) ; [-1,2]x[-4,-3]
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (lower-bound y))))
               (else                ; [-1,2]x[-3,4]
                (mul-interval x y))))))


(check-equal? (mul-interval-c (make-interval 1 2) (make-interval 3 4))     '(3 . 8))
(check-equal? (mul-interval-c (make-interval 1 2) (make-interval -4 -3))   '(-8 . -3))
(check-equal? (mul-interval-c (make-interval 1 2) (make-interval -3 4))    '(-6 . 8))
(check-equal? (mul-interval-c (make-interval -2 -1) (make-interval 3 4))   '(-8 . -3))
(check-equal? (mul-interval-c (make-interval -2 -1) (make-interval -4 -3)) '(3 . 8))
(check-equal? (mul-interval-c (make-interval -2 -1) (make-interval -3 4))  '(-8 . 6))
(check-equal? (mul-interval-c (make-interval -1 2) (make-interval 3 4))    '(-4 . 8))
(check-equal? (mul-interval-c (make-interval -1 2) (make-interval -4 -3))  '(-8 . 4))
(check-equal? (mul-interval-c (make-interval -1 2) (make-interval -3 4))   '(-6 . 8))
(check-equal? (mul-interval-c (make-interval -2 1) (make-interval -3 4))   '(-8 . 6))
(check-equal? (mul-interval-c (make-interval -1 2) (make-interval -4 3))   '(-8 . 6))
(check-equal? (mul-interval-c (make-interval -2 1) (make-interval -4 9))   '(-18 . 9))

(check-equal? (mul-interval-c (make-interval 0 2) (make-interval 3 4))     '(0 . 8))
(check-equal? (mul-interval-c (make-interval -2 0) (make-interval 3 4))    '(-8 . 0))
(check-equal? (mul-interval-c (make-interval 1 2) (make-interval 0 4))     '(0 . 8))
(check-equal? (mul-interval-c (make-interval 1 2) (make-interval -3 0))    '(-6 . 0))
(check-equal? (mul-interval-c (make-interval 0 2) (make-interval 0 4))     '(0 . 8))
(check-equal? (mul-interval-c (make-interval 0 2) (make-interval -3 0))    '(-6 . 0))
(check-equal? (mul-interval-c (make-interval -1 0) (make-interval 0 4))    '(-4 . 0))
(check-equal? (mul-interval-c (make-interval -1 0) (make-interval -3 0))   '(0 . 3))
(check-equal? (mul-interval-c (make-interval 0 0) (make-interval 3 4))     '(0 . 0))
(check-equal? (mul-interval-c (make-interval 1 2) (make-interval 0 0))     '(0 . 0))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; (add-interval (make-center-width 2 1) (make-center-width 4 2))

; Exercise 2.12.

(define (make-center-percent c p)
  (make-center-width c (* c p 0.01)))
(define (percent i)
  (* (/ (width i) (center i)) 100))

;(make-center-percent 100 5)
;(percent (make-center-percent 100 5))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;(define R1 (make-center-percent 100 10))
;(define R2 (make-center-percent 200 10))
;
;R1
;R2
;(center (par1 R1 R2))
;(percent (par1 R1 R2))
;(center (par2 R1 R2))
;(percent (par2 R1 R2))

; Exercise 2.14.

(define (center-percent i)
  (cons (center i) (percent i)))

;(define R1 (make-center-percent 100 1))
;(define R2 (make-center-percent 200 2))

;(center-percent (add-interval R1 R1))
;(center-percent (add-interval R1 R2))
;(center-percent (add-interval R2 R2))
;
;(center-percent (sub-interval R1 R2))
;(center-percent (sub-interval R2 R1))
;
;(center-percent (mul-interval R1 R1))
;(center-percent (mul-interval R1 R2))
;(center-percent (mul-interval R2 R2))
;
;(center-percent (div-interval R1 R1))
;(center-percent (div-interval R1 R2))
;(center-percent (div-interval R2 R1))
;(center-percent (div-interval R2 R2))

; Exercise 2.15.

(define one-i (cons 1 1))

;(add-interval R1 one-i)
;(add-interval one-i R1)
;
;(sub-interval R1 one-i)
;(sub-interval one-i R1)
;
;(center-percent (mul-interval R1 one-i))
;(center-percent (mul-interval one-i R1))
;
;(center-percent (div-interval R1 one-i))
;(center-percent (div-interval one-i R1))

; Exercise 2.16.

;----

; 2.2.1 Representing Sequences

(define one-through-four (list 1 2 3 4))
;one-through-four
;(car one-through-four)
;(cdr one-through-four)
;(car (cdr one-through-four))
;(cons 10 one-through-four)
;(cons 5 one-through-four)

; List operations

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))

;(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))

;(length odds)

(define (length-i items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;(length-i odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;(append squares odds)
;(append odds squares)

; Exercise 2.17.

;(define (last-pair l)
;  (if (null? (cdr l))
;      l
;      (last-pair (cdr l))))

(define (last-pair l)
  (let ((m (cdr l)))
    (if (null? m)
        l
        (last-pair m))))

; (last-pair (list 23 72 149 34))

; Exercise 2.18.

(define (reverse l)
  (define (iter l ans)
    (if (null? l)
        ans
        (iter (cdr l) (cons (car l) ans))))
  (iter l (quote ())))

;(reverse (list 1 4 9 16 25))

(define (reverse-r l)
  (if (null? l)
      l
      (append (reverse-r (cdr l))
              (cons (car l) (quote ())))))

;(reverse-r (list 1 4 9 16 25))

(define (append-i list1 list2)
  (define (iter l ans)
    (if (null? l)
        ans
        (iter (cdr l) (cons (car l) ans))))
  (iter (reverse list1) list2))

;(append-i squares odds)
;(append-i odds squares)

; Exercise 2.19.

(define us-coins (list 50 25 10 5 1))
(define uk-conis (list 100 50 20 10 5 2 1 0.5))

;(define (cc amount kinds-of-coins)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (null? kinds-of-coins)) 0)
;        (else (+ (cc amount (cdr kinds-of-coins))
;                 (cc (- amount (car kinds-of-coins)) kinds-of-coins)))))

(define cc-count 0)
(define (cc amount coin-values)
  (set! cc-count (+ cc-count 1))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))

;(define (no-more? coin-values)
;  (null? coin-values))
;(define (except-first-denomination coin-values)
;  (cdr coin-values))
;(define (first-denomination coin-values)
;  (car coin-values))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(set! cc-count 0)
(cc 100 us-coins)
cc-count

(set! cc-count 0)
(cc 100 (reverse us-coins))
cc-count

