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

1001の結果が微妙にアレです
統計の人ならこりゃ有意に差がありますねとか言いそうです

数によらない要因がまだ残ってるかなあ
prime?の代わりにdummyを使ってオーバーヘッドを計測するのはまだ完全じゃないか
timed-prime?でstart-timeを取得→prime?呼び出し→smallest-divisor呼び出し
あたりが含まれてないことになるし
find-divisorをダミーに置き換えるほうが正確になるかな
こう？

```
(define (dummy-prime? next)
  (lambda (n) (= n (dummy-smallest-divisor n next))))

(define (dummy-smallest-divisor n next)
  (dummy-find-divisor n 2 next))

(define (dummy-find-divisor n test-divisor next)
  n)
```

どれ

```
   1001 : 1.8200530290372314
  10001 : 2.0692433092477485
 100001 : 1.988264206864656
1000001 : 1.9388971246922337
```

あんまかわらんすよ
これくらいで手を打ちませんか先生

ばらつきに一定の傾向がみられることも悩ましさに拍車をかけてました
ただバラバラなら平均をとって終わりにすることもできると思うんですが
すこし工夫すれば取り除けそうで
こういう場合は中央値を取った方がいいのかも、とか
それとも上下いくつかを外してから平均を取ろうか、とか

### Exercise 1.24. 

* fast-prime?でさっき見つけた12個の素数を試してみなさい
* 1000000にかかる時間は1000にかかる時間にくらべてどうか？

Θ(log n)なので2倍で済むはず

```
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
          (timed-prime? (lambda (n) (fast-prime? n 100)) n)))

(define (timed-prime? test? n)
  (let ((start-time (current-inexact-milliseconds)))
    (if (test? n)
        (- (current-inexact-milliseconds) start-time)
        #f)))
```

とりあえず素直に何も補正せず1回きりの試行で
fast-prime?で何回試すのが相場なのかよくわかりませんが100回で

```
1009 : 0.447021484375
1013 : 0.257080078125
1019 : 0.377197265625
10007 : 0.491943359375
10009 : 0.364990234375
10037 : 0.43212890625
100003 : 0.572998046875
100019 : 0.489013671875
100043 : 0.43505859375
1000003 : 0.385009765625
1000033 : 0.38916015625
1000037 : 0.39111328125
```

(つд⊂)ｺﾞｼｺﾞｼ
1000000の方が速くなってます！
どうすんすか先生
これはあれかな
繰り返しやってると速くなる現象かな
なんかそういうのなくさないと

順番を逆にしてみた

```
1000003 : 0.431884765625
1000033 : 0.4521484375
1000037 : 0.359130859375
100003 : 0.2978515625
100019 : 0.308837890625
100043 : 0.31201171875
10007 : 0.263916015625
10009 : 0.2509765625
10037 : 0.260986328125
1009 : 0.242919921875
1013 : 0.2080078125
1019 : 0.215087890625
```

お、なんか理論値に近づいた気が
よし先生にはこの値を提出しよう（捏造じゃないよ

さておき
ちゃんと測定できてる気になれないと先へ進めません
困った

また平均を取るか・・・
ってこれ100回やった平均みたいなもんだろ
どうすんだよ

1000000回やらせてみた（力技

```
1009 : 1959.90185546875
1013 : 2031.31396484375
1019 : 2123.968994140625
10007 : 2607.056884765625
10009 : 2477.416015625
10037 : 2539.02392578125
100003 : 2922.492919921875
100019 : 3010.7470703125
100043 : 3047.215087890625
1000003 : 3417.47705078125
1000033 : 3478.629150390625
1000037 : 3495.89306640625
```

まあまあ揃ってきた？

```
10007 : 2607.056884765625
10009 : 2477.416015625
```

こういうのはもう諦めよう
再起動直後にこれだけ起動するとかすればもうちょっとくらいは揃うのかもしれないけど
いや、再起動直後じゃかえってばらつくかも？

さてΘ(log n)さん的には2倍時間がかからないとおかしいわけですが
同じ関数でnが変わっているだけなので1.23よりも条件が揃ってて
nに影響されない分を引いてやるくらいしか思いつかないです

どこまでがnに影響されない部分かな
expmodの外側は全部ってことになるか
expmodをダミー化

```
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
```

さて

```
d     = 271.924072265625
t1    = 1926.79296875
t2    = 3389.715087890625
t2/t1 = 1.7592523653901906
(t2-d)/(t1-d) = 1.88401088584629
```

説明がついたとは言い切れない比
しかしギブアップ

# SICPを読む(7)

### Exercise 1.25

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


