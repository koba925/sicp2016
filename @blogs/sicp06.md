# SICPを読む(6)

### Exercise 1.23.

* smallest-divisorは2,3,4,5,...で割っていてもったいない
* (next test-divisor)を定義して2,3,5,7,...で割るようにせよ
* 時間は半分になったか？

find-divisorを書き換えます

```
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))
```

どれどれ

```
> (search-for-primes 1001 3)
0.0030110677083333335
> (search-for-primes 10001 3)
0.007405598958333333
> (search-for-primes 100001 3)
0.019694010416666668
> (search-for-primes 1000001 3)
0.06005859375
```

1001 0.00390 → 0.00301 1.29倍
10001 0.00936 → 0.00741 1.26倍
100001 0.0297 → 0.0197 1.50倍
1000001 0.0937 → 0.0601 1.55倍

というわけで2倍速くはなりませんでした
考えられるのはこれくらい？

a) (+1 test-divisor)が(next test-divisor)になって遅くなった分
b) start-timeを取得してからprime?を呼び出すまでにかかった時間
c) prime?が終了してからfinished-timeを取得するまでの時間
d) 測定条件というかたまたま(何度もやってみると少しばらつきがある)

補正できるかな？

a) は(+1 test-divisor)の代わりにこういうのを使えばほぼフェアな比較になるはず

```
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 1)))
```

```
> (search-for-primes 1001 3)
0.004313151041666667
> (search-for-primes 10001 3)
0.012288411458333334
> (search-for-primes 100001 3)
0.037272135416666664
> (search-for-primes 1000001 3)
0.11832682291666667
```

これを使って比較する

b)、c)はprime?を呼びださないで時間を測定してみればいいかな

```
    (if (prime? n)
        (elapsed-time (current-inexact-milliseconds))
        #f))
```

を単に

```
    (elapsed-time (current-inexact-milliseconds))
```

にして

```
> (timed-prime? 2)
0.0009765625
```

1ミリ秒くらいはかかるんだな
これは思ったより大きい
defineの中にdefineを入れると毎回defineを実行して時間がかかる？

さてどうなった

```
> (/ (- 0.004313151041666667 0.0009765625) (- 0.0030110677083333335 0.0009765625))
1.6400000000000001
> (/ (- 0.012288411458333334 0.0009765625) (- 0.007405598958333333 0.0009765625))
1.7594936708860762
> (/ (- 0.037272135416666664 0.0009765625) (- 0.019694010416666668 0.0009765625))
1.9391304347826084
1.9862258953168044
> (/ (- 0.11832682291666667 0.0009765625) (- 0.06005859375 0.0009765625))
1.9862258953168044
```

数が大きい時はかなりいい感じになったけど数が少ない時がまだ2倍にならないな
まだ数によらない部分で時間がかかっているところがあるということ？
補正したけど補正しきれてないだけ？

こういうとこついコピペでやっちゃうのは怠惰の美徳が足りてないっぽい気がする
全体的に書き直し

```
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
        (iter (+ c 1) (+ total-time
                         (search-for-primes test? lower count)))))
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
```

フライングですがlambdaとかletとか使ってます

この形にたどり着くまでにいろいろと紆余曲折が

まず1度目の関数実行だけ時間がかかることに気づきました
何でかな
測定のためにはこれはどけておかないといけないので
とりあえず1回実行してから測定するようにしました
さっき何もしないときの時間がけっこうかかると思ったのはこれかもしれない
あと数が小さい時の結果が思わしくなかったのも

しかしなかなかばらつきが大きい
実行するたびに1.5倍〜2.2倍くらいの間でまったく異なる結果が出てしまいます

試行が1回づつだからかなと思い
dummyを10回実行した平均、
(prime? slow-next)を10回実行した平均、
(prime? slow-next)を10回実行した平均
を取ってから計算しましたがまだばらつきが大きい

100回にしても1000回にしてもまだばらつきが大きい
しかも、数十回くらい実行すると1回の試行にかかる時間が急に短くなることがある
実行時に最適化しているのかキャッシュにでも入ったのか
これはなかなか手ごわい

半ばヤケ気味に上で出した平均で遅い方と速い方の比を計算し
それを10回繰り返して平均を取ったらやっとそこそこ安定しました

```
   1001 : 1.8337048475130946
  10001 : 1.9836974335283653
 100001 : 2.0142126400853986
1000001 : 1.9625607023080118
```

1001の結果が微妙にアレですが
統計の人ならこりゃ優位に差がありますねとか言いそうです

ばらつきに一定の傾向がみられることも悩ましさに拍車をかけてました
ただバラバラなら平均をとって終わりにすることもできると思うんですが
すこし工夫すれば取り除けそうで
こういう場合は中央値を取った方がいいのかも、とか
それとも上下いくつかを外してから平均を取ろうか、とか

まあでもこれくらいで手を打ちませんか先生
