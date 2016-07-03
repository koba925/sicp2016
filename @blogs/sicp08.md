# SICPを読む(8)

### Exercise 1.30.

* sumを線形繰り返しプロセスに書き換えよ

```
(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
```
