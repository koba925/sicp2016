# SICPを読む(7)

### Exercise 1.25.

* `(define (expmod base exp m) (remainder (fast-expt base exp) m))`ではなぜよくないのか

数学的にはまったく何の問題もないです
実行のオーダもどちらもΘ(log n)で同じ
細かく見るとremainderの実行回数はこちらのexpmodでは1回だけなのに対し
オリジナルのexpmodではΘ(log n)で増えていきます
それだけを見ればこっちの方がいいくらい

ただしコンピュータで実行する場合は数字が大きくなりすぎるとよろしくないです
たとえば100001が素数かどうかを確かめようとして
99999^100001 mod 100001を計算しようとすると
fast-expの値はだいたい50万桁の数になります
実行回数はΘ(log n)とは言え
50万桁のかけ算を実行するにはかなり時間がかかることは想像に固くありません

一方、オリジナルのexpmodは毎回remainderを取る必要がありますが
おかげで値が大きくなることはありません

どれくらい違うのかというと程度問題ですので実際やってみます

```
> (let ((s (current-inexact-milliseconds)))
    (expmod 99999 100001 7)
    (- (current-inexact-milliseconds) s))
0.00390625
> (let ((s (current-inexact-milliseconds)))
    (expmod-ex1-25 100000 100000 7)
    (- (current-inexact-milliseconds) s))
22.5341796875
```  

4桁ほど違います

ところで実行時間を計る関数がほしいですね
でも

```
(define (exp-time f)
  (let ((s (current-inexact-milliseconds)))
    f
    (- (current-inexact-milliseconds) s)))
```

ではうまくいかないんですよね

```
> (exp-time (expmod-ex1-25 99999 100001 7))
0.0
```

`exec-time`を呼ぶ前に`(exec-time (expmod-ex1-25 99999 100001 7))`を
評価してしまいますから
時間を図りたい式をいったんlambdaでくるむかな

こうして

```
(define (exp-time f)
  (let ((s (current-inexact-milliseconds)))
    (f)
    (- (current-inexact-milliseconds) s)))
```

こう

```
> (exp-time (lambda () (expmod-ex1-25 99999 100001 7)))
39.64501953125
```

うまくいった模様です
たぶんマクロで書くのが筋だと思われますが詳しくないので放置
ライブラリに定義されてそうな気はするんですけどすぐには見つかりませんでした

### Exercise 1.26.

* Louisの書いたexpmodはなぜΘ(log n)ではなくΘ(n)で動くのか

Louisの書いたexpmodでは、もとのexpmodでこのように書いていたところを

```
         (remainder (square (expmod base (/ exp 2) m))
                    m))
```

このように書いてしまっています

```
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
```

元の書き方だと、expを半分にすることでexpmodの実行回数を半分にすることができ
そのおかげでΘ(log n)のオーダで値を求めることができました

ところがLouis版ではせっかく半分になったexpmodを2回実行しています
これでは結局n回のかけ算が必要になってしまうのでオーダはΘ(n)になってしまいます

### Exercise 1.27.

* カーマイケル数がフェルマーテストを欺くことを示すプログラムを書け
* つまり、a<nであるすべてのaについてa^n=a(mod n)であることを確かめるプログラムを書け

```
(define (carmichael-test n)
  (define (iter a)
    (cond ((= n a) #t)
          ((= (expmod a n n) a) (iter (+ a 1)))
          (else #f)))
  (iter 1))
```

expmodがΘ(log n)で
それをn回繰り返すのでΘ(nlog n)で動く素数判定プログラムということになります

```
> (prime? 561)
#f
> (carmichael-test 561)
#t
> (prime? 1105)
#f
> (carmichael-test 1105)
#t
> (prime? 1729)
#f
> (carmichael-test 1729)
#t
```

### Exercise 1.28.

* 欺かれることのないフェルマーテストの変形のひとつにミラー・ラビンテストがある
* nが素数ならば正の整数a(<n)についてa^(n-1)≡1(mod n)である、というもの
* expmodの中で自乗するとき、"nを法とする自明でない1の平方根"が見つかっていないかを確認せよ
* "nを法とする自明でない1の平方根"とは1とn-1以外の数で、自乗すると1(mod n)であるもの
* このような数が存在すればnは素数ではない
* また素数でない奇数についてはaより小さい数のうちの半数以上がミラー／ラビンテストに失敗することがわかっている

書きました

```
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
```

またしてもズルでlet*使いました
その気になればlambdaでだって書けるもん

expmodがnを知ってなきゃいけないので
expmodをmiller-rabin-testの中に入れました
nを引数で渡すのもアレなんで

どういうテストを通ったら信用していいのかいまひとつわからないので
10000までの数でfast-prime?と結果を比較します

```
(define (miller-rabin-test-test to)
  (define (iter n)
    (when (not (eq? (fast-prime-ex1-28? n 100)
                    (fast-prime? n 100)))
      (printf "~a~n" n))
    (when (<= n to) (iter (+ n 1))))
  (iter 2))
```

またしてもズ（略

```
> (miller-rabin-test-test 6601)
561
1105
1729
2465
2821
6601
```

想定どおりです

## 1.3 Formulating Abstractions with Higher-Order Procedures

* `cube`は特定の数に対する操作ではなく、さまざまな数に対する3乗という概念を記述した
* 同じように、特定の手続きに対する操作ではなく、さまざまな手続きに対する概念を表現したい
* 手続きを引数に取ったり、値として引数を返す「高階手続き」を導入する
