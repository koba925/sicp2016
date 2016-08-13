# SICPを読む(12)

## 2.1.3 What Is Meant by Data?

このへんの話は好きなところ

* add-ratやsub-ratといった有理数の操作ははmake-rat、numer、denomで実装できた
* これらの操作は分子・分母・有理数というデータオブジェクトにより定義できたと言える
* データオブジェクトのふるまいはmake-rat、numer、denomにより決まっている
* では、「データ」とはいったいなにか

哲学ですか

* セレクタとコンストラクタが定義されているものというだけでは不十分
* xが`(make-rat n d)`ならば必ず`(numer x)/(denom x)=n/d`を満たす
* セレクタとコンストラクタと、それらが満たすべき条件をあわせたものがデータであると考えることができる

無定義述語みたいな話

* ペアについても同様なことが言える
* zが`(cons x y)`ならば必ず`(car z)`がxであり`(cdr z)`がyである必要がある
* Schemeではcons、car、cdrは始めから組み込まれているが、この条件を満たせばなんでもペアとして使うことができる
* 驚くべきことに、何のデータ構造も使わず、手続きだけでcons、car、cdrを実装することができる

```
(define (cons2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
(define (car2 z) (z 0))
(define (cdr2 z) (z 1))
```

consを定義してしまうと大変なので名前は変えてます

昔読んだ時はこれの意味わからなかったんですよね
今は手習い読んだ（特にvalueを書いた）おかげですんなり理解できるようになりました
そういえばここでもクロージャについては特に触れずに説明してますね
手習いと同じだ
最初からそういうものだと教えられればすんなり飲み込めるのかな

* 手続きのこういう使い方は我々が思うところの自然なデータの記法とはまったく異なる
* しかしこれは上記の条件を満たしているおり、ペアを表現しているといえる
* これらを組み合わせてペアを使っている限り、普通のcons、car、cdrと区別することはできない
* 手続きをオブジェクトとして扱えるならば、複合データを表現することができるということになる
* ちょっと奇妙に思えるかもしれないが、これはメッセージパッシングと呼ばれ、これからもよく使う

### Exercise 2.4.

* 以下の表現で`(car (cons x y))`がxを返すことを確かめよ

```
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
```

置換モデルで確かめます

```
  (car (cons X Y))
= (car (lambda (m) (m X Y)))
= ((lambda (m) (m X Y)) (lambda (p q) p))
= ((lambda (p q) p) X Y)
= X
```

* cdrを書け

```
(define (cdr z) (z (lambda (p q) q)))
```

ifを使って書くよりこっちの方がシンプルに見えます
ちょっとラムダ計算入門みたいな感じ

### Exercise 2.5.

* 2^a*3^bの形で非負整数a、bのペアを表現せよ

素因数分解の一意性からあいまいさは生じず、ある数が2^a*3^bの形であれば必ず
aとbが特定できるわけですね

carとかcdrは要するに2とか3で何回割れるか計算すればいいので

```
(define (cons4 x y) (* (expt 2 x) (expt 3 y)))
(define (cr z n)
  (if (= (remainder z n) 0)
      (+ 1 (cr (/ z n) n))
      0))
(define (car4 z) (cr z 2))
(define (cdr4 z) (cr z 3))
```

たとえば`(cons4 (cons4 3 4) (cons4 5 6))`は2^648*3^23328という数になります
さすがに一瞬待ちますが計算できてしまうのがすごい
ゲーデル数も計算できるかな（無理

### Exercise 2.6.

* 手続きが使えれば数がなくてもなんとかなる
* 0と+1を以下のように定義する

0と+1さえあれば自然数(0を含む)はなんとかなります
ありがとうG. Peano

```
(define zero
  (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
```

なんでしょうね
zeroは見たままの形ですがadd-1の方は少し複雑です

* これは発明したAlonzo Churchにちなんでチャーチ数と呼ばれる

計算機科学っぽく

* oneとtwoをzeroやadd-1を使わずに定義せよ

でもzeroとadd-1を使った形から出発します

```
  one
= (add-1 zero)
= ((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (f) (lambda (x) x)))
= (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
= (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
= (lambda (f) (lambda (x) (f x)))

  two
= (add-1 one)
= (add-1 (lambda (f) (lambda (x) (f x))))
= ((lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) (lambda (f) (lambda (x) (f x))))
= (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
= (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
= (lambda (f) (lambda (x) (f (f x))))
```

これでいいかな
並べて書くと

```
zero = (lambda (f) (lambda (x) x)))
one  = (lambda (f) (lambda (x) (f x)))
two  = (lambda (f) (lambda (x) (f (f x))))
```

自然数nを、「関数fを引数に取り、xにfをn回適用する関数を返す関数」で表現していることがわかります

逆算すると、add-1はxにfをもう1回余分に適用する関数を返す関数なはず
そう思ってadd-1の定義を見てみると

1. (n f)はfをn回適用する関数
2. ((n f) x)はxにfをn回適用した結果
3. (f ((n f) x))はxにfをn回適用した結果にfを適用した結果  
つまりxにfを(n+1)回適用した結果

うんn+1になってそうです

* +を、add-1の繰り返しでない形で定義せよ

add-1の繰り返しの形ってどういうのかなあ
手習い思い出して書くとこんな感じだっけ

```
(define (ch+ n m)
  (if (zero? m)
      n
      (ch+ (add-1 n) (sub-1 m))))
```

確かにこれで作ろうとするとsub-1とかzero?とか作らなきゃいけなくて大変そうです

「関数fを引数に取り、xにfをm回適用する関数を返す関数」と
「関数fを引数に取り、xにfをn回適用する関数を返す関数」とから
「関数fを引数に取り、xにfを(m+n)回適用する関数を返す関数」を作ればいいわけですから
そっち方向で考えてみます

nにadd-1をm回適用すれば(n+m)になるはずですのでnとかmを直接使えばできそうな感じです
普通の数を使ってやってみるとこんな感じ
（このadd1は普通の数の+1です）

```
> ((zero add1) 1)
1
> ((one add1) 2)
3
> ((two add1) 3)
5
```

うまくいきました
つまりこうかな

```
(define (ch+ n m)
  ((n add-1) m))
```

くっそ単純にできました
add-1の繰り返しでない形になってる・・・よね？
繰り返しと言えば繰り返しと言えなくもない気もしますけどたぶん大丈夫

どうやって確かめたらいいんでしょう

```
> (ch+ one two)
#<procedure:...icp2016/sicp.rkt:1563:2>
```

これじゃわかりません
：
そっかさっきと同じでいいのか

```
> (((ch+ one two) add1) 1)
4
```

うまくいってるようです

関数をn回適用する関数でnを表現するってのはうまいアイデアですね
