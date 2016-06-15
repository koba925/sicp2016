
# SICPを読む(5)

## 1.2.5 Greatest Common Divisors

* a÷bの余りがrならばaとbの最大公約数はbとrの最大公約数に等しい

知ってはいたけどまじめに考えたことなかったな
どういうことなのかな

ch1-2-5-euclid-gcd.jpg

そういうことか
これはΘでいうとどれくらいなんだろう

速い時は1回で終わる
余りが1なら即おしまい

遅いときってどんなときだ
a÷b=1でb÷rも1になって・・・をずっと繰り返すときが
一番なかなか減らないことになりそう

たとえば(2,3)をスタートにして上の関係を逆にたどっていくと・・・

(2,3)→(3,5)→(5,8)→...

ん？これって・・・
Fibか
(0,1)から始めたら完全にFibだ
狙って出してるの？

てすぐ下に書いてあったよ！（Lameの定理）

Fibは指数的に増えていくから
GCDの計算ステップ数は対数的にしか増えていかないわけだな
感覚的には納得

```
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
```

これってa<bだと動かないかな？
・・・
動くな
よくできてる

### Exercise 1.20.

normal-orderは必要になるまで引数を評価しないので

```
  (gcd 206 40)
= (if (= 40 0) 206 (gcd 40 (remainder 206 40)))
= (gcd 40 (remainder 206 40))
= (if (= (remainder 206 40) 0)
      40
      (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
```

ここでifの条件節を評価しなければならないので一度remainderを評価します
（でも他は評価しない）

```
= (if (= 6 0)
      40
      (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
= (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
= (if (= (remainder 40 (remainder 206 40)) 0)
      (remainder 206 40)
      (gcd (remainder 40 (remainder 206 40))
           (remainder (remainder 206 40)
                      (remainder 40 (remainder 206 40)))))
```

かなり大変なことになってきました
またifの条件節だけ評価して

```
= (if (= 4 0)
      (remainder 206 40)
      (gcd (remainder 40 (remainder 206 40))
           (remainder (remainder 206 40)
                      (remainder 40 (remainder 206 40)))))
= (gcd (remainder 40 (remainder 206 40))
       (remainder (remainder 206 40)
                  (remainder 40 (remainder 206 40))))
= (if (= (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40))) 0)
      (remainder 40 (remainder 206 40))
      (gcd (remainder (remainder 206 40)
                      (remainder 40 (remainder 206 40)))
           (remainder (remainder 40 (remainder 206 40))
                      (remainder (remainder 206 40)
                                 (remainder 40 (remainder 206 40)))))
= (if (= 2 0)
      (remainder 40 (remainder 206 40))
      (gcd (remainder (remainder 206 40)
                      (remainder 40 (remainder 206 40)))
           (remainder (remainder 40 (remainder 206 40))
                      (remainder (remainder 206 40)
                                 (remainder 40 (remainder 206 40)))))
= (gcd (remainder (remainder 206 40)
                  (remainder 40 (remainder 206 40)))
       (remainder (remainder 40 (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40 (remainder 206 40))))
= (if (= (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))) 0)
      (remainder (remainder 206 40)
                 (remainder 40 (remainder 206 40)))
      (gcd ...))
= (if (= 0 0)
      (remainder (remainder 206 40)
                 (remainder 40 (remainder 206 40)))
      (gcd ...))
= (remainder (remainder 206 40)
             (remainder 40 (remainder 206 40)))
= 2
```

見るからにnormal-orderつらい
というかつらかった

remainderの評価回数は
ifの条件節のremainderを14回
実際にgcdを求めるremainderを4回
合計18回

applicative-orderはもうちょっと楽なはず

```
  (gcd 206 40)
= (if (= 40 0) 206 (gcd 40 (remainder 206 40)))
= (gcd 40 (remainder 206 40))
= (gcd 40 6)
= (if (= 6 0) 40 (gcd 6 (remainder 40 6)))
= (gcd 6 (remainder 40 6))
= (gcd 6 4)
= (if (= 4 0) 6 (gcd 4 (remainder 6 4)))
= (gcd 4 (remainder 6 4))
= (gcd 4 2)
= (if (= 2 0) 4 (gcd 2 (remainder 4 2)))
= (gcd 2 (remainder 4 2))
= (gcd 2 0)
= (if (= 0 0) 2 (remainder 2 0))
= 2
```

remainderの評価回数は4回
全然楽

## 1.2.6 Example: Testing for Primality

* この章では素数かどうかを判別する手法をふたつ紹介する
* ひとつはΘ(√n)のもの、もうひとつは「確率的な」アルゴリズムでΘ(log n)のもの
* ひとつ目は2から始めて順に割っていき、nまで割り切れなかったら素数というもの
* nまで全部割り算しなくても、√nまで割り算すれば十分なのでΘ(√n)
* Θ(log n)のアルゴリズムにはフェルマーの小定理を用いる

> フェルマーの小定理：
> もしnが素数で、aがnより小さい正の整数であればa^nはnを法としてaに合同である

よく出てくる定理だけれどもこれもなんか不思議

* a^nがaに合同でなければ、nは確実に素数ではない
* a^nがaに合同であればけっこうな確率でnは素数
* いろんなaで試せばnが素数である確率は上がる
* このアルゴリズムをフェルマーテストという

コード自体は特筆することなし
そういえば#t、#fじゃなくてtrue、falseで書いている
素のr5rsだとエラーになるみたいだけど何の意図だろうか
r6rsとかr7rsとかなら定義されてるのかな
(racketでは定義済み)

* いままでのアルゴリズムは正しい答えが保証されていた
* フェルマーテストはおそらく正しい答えが出るのみ
* 回数を増やせば確率は上がる
* ただし、フェルマーテストを騙す数がある

カーマイケル数という

```
> (prime? 561)
#f
> (fast-prime? 561 10000)
#t
```

適当に選んだ大きな数がカーマイケル数である確率は
コンピュータが宇宙線で誤動作する確率より低いとのこと

* 騙されないフェルマーテストの変種もある
* テストを繰り返せば好きなだけ間違いの確率を減らすことができる
* こういうアルゴリズムを確率的アルゴリズムという

### Exercise 1.21.

```
> (smallest-divisor 199)
199
> (smallest-divisor 1999)
1999
> (smallest-divisor 19999)
7
```

うわあびっくり（棒

てことでいいんでしょうか

