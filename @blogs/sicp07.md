# SICPを読む(7)

### Exercise 1.25.

* `(define (expmod base exp m) (remainder (fast-expt base exp) m))`ではなぜよくないのか

数学的にはまったく何の問題もないです
実行のオーダもどちらもΘ(log n)で同じ
細かく見るとremainderの実行回数はこちらのexpmodでは1回だけなのに対し
オリジナルのexpmodではΘ(log n)で増えていきます
それだけを見ればこっちの方がいいくらい

でも実際問題としては数字が大きくなりすぎるとよろしくないってことでしょうね
たとえば100001が素数かどうかを確かめようとして
99999^100001 mod 100001を計算しようとすると
fast-expの値はだいたい50万桁の数になります
実行回数はΘ(log n)とは言え
50万桁のかけ算を実行するにはかなり時間がかかることは想像に難くありません

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
時間を測りたい式をいったんlambdaでくるむかな

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
（しかも騙される）

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
模範解答ではどう書いてあるんだろう

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

* また素数でない奇数についてはaより小さい数のうちの半数以上がミラー／ラビンテストに失敗することがわかっている

ということは、間違いの確率はテストごとに半分以下になるので
間違いの確率pを一定以下にするための試行回数はΘ(log p)で増加する、てことかな
そして任意の数についてpを好きなだけ小さくできる

## 1.3 Formulating Abstractions with Higher-Order Procedures

* `cube`は特定の数に対する操作ではなく、さまざまな数に対する3乗という概念を記述した
* 同じように、特定の手続きに対する操作ではなく、さまざまな手続きに対する概念を表現したい
* 手続きを引数に取ったり、値として引数を返す「高階手続き」を導入する

出ました高階手続き

## 1.3.1 Procedures as Arguments

連続する整数の和と
連続する整数の3乗の和と
π/8に収束する級数を見比べて共通のパターンを抽出します

連続する整数の和と連続する整数の3乗の和、と言えば
1からnまでの整数の和の2乗が1からnまでの整数の3乗の和に等しい
って話が思い出されますが話の筋には関係ないかな

* 数学ではΣを使って（特定の和ではなく）和の概念を扱う
* プログラミング言語でも同様に、特定の和を求める手続きではなく、和の概念を表す手続きを書きたい

というところで高階関数を導入

* 和を概念化できたので、これを使ってさらに進んだ概念を形式化することができる

たとえば、ということで積分を数値的に近似する関数を定義します
細長い長方形に分けるやつ

このへん、懇切丁寧に書いてあるのでほとんど疑問も書き足したいこともないです
Scheme手習いとかScheme修行はただ読んでいくだけでいろいろ疑問が出てきましたが

教科書としてはSICPのやりかたがまっとうで王道なんでしょうが、
Scheme手習い式もなかなか面白かったです
この項目は読んで写経しておしまい

### Exercise 1.29.

* fとaとbとn(分割数)を引数に取り、シンプソンの公式で積分を計算する関数を作れ

シンプソンの公式とはこんなもの
まともに数式を書くのは面倒そうなのでまあなんとなく分かる程度に

```
∫[a b]f=(h/3)[y0+4y1+2y2+4y3+...+2yn-2+4yn-1+yn]
```

数学が好きな高校生くらいなら知っててもよさそうな感じの式ですが
係数が14242...241てどういうことでしょうね

プログラミングとは関係ありませんがちょっと考えてみます
本文に記載の近似式は、n-1からnまでの幅hの値をa+h/2の時の値で代表させたもの
細長い長方形で近似していることになります
もっといい近似を取ろうと思ったらまず思いつくのは台形で近似すること
aからbの区間をn個に分割するとして、h=(a-b)/nとすると
台形の面積は(h/2)(<左側のy>+<右側のy>)だから全体を合計するとこう

```
∫[a b]f=(h/2)[y0+2y1+2y2+...+2yn-1+yn]
```

まだシンプソンの公式にはなりませんね
ちなみに本文に記載されたシンプルな近似を少し書き直すとこんな雰囲気
（雰囲気だけ）

```
∫[a b]f=h[y0+y1+y2+...+yn-1+yn]
```

比べてみると式の形としてはシンプソンの公式に一歩近づいた気はします
係数の並びが111...111から122...221になったからには次に142...241になったり
1/1が1/2になったからには次に1/3になったり
してもいいよね！

ものは試しでこの式もプログラムにしてみます
本文の流れからするとsumを使って実装するんでしょうね
k=0とk=nのときの特別扱いはsumでは難しそうだから別扱いにするしかないかな？

```
(define (my-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-dx a) (+ a h))
  (* (/ h 2) (+ (f a) (* 2 (sum f (+ a h) add-dx (- b h))) (f b))))
```

どれどれ

```
> (my-integral cube 0.0 1.0 100)
0.24032201000000036
```

あれ？

・・・なんか1回分くらい抜けてるんじゃないだろうか
sumでaを表示させてみた
秘技printfデバッグというやつ(racketにはprintfという関数がある(豆

```
> (my-integral cube 0.0 1.0 100)
a:0.01
a:0.02
a:0.03
:
a:0.9800000000000006
a:0.9900000000000007
0.24032201000000036
```

やっぱり1回分抜けてる
(aが0.99を超えた回は計算しないから)
細々とhを足し続けるやり方がよくないんでしょうが
超ゴマカシでb-hまででsumが終わるべきところbまで計算させてみました

```
> (my-integral cube 0.0 1.0 100)
0.2500250000000004
```

それでもなんか精度が上がってないなあと思って冷静に式を見てみたら
長方形のときと比べて端っこの値が違うだけでした

```
∫[a b]f=(h/2)[y0+2y1+2y2+...+2yn-1+yn]
       =h[y0/2+y1+y2+...+yn-1+yn/2]
```

これでは精度が上がらなくてもしかたないかな
図を書いた時にはすごく精度が上がる気がしたんですけど所詮は直線ということか・・・
(精度が下がってることの説明にはなってませんけど)

横道にそれすぎてる気がするのでこれくらいにして先に進みます

1点の値で代表していたのが、2点を結ぶ直線で近似するようになったわけだから
自然に延長すれば3点を結ぶ2次曲線で近似するってことでしょう

そう思ってできあがりの式をみると

```
  ∫[a b]f
= (h/3)[y0+4y1+2y2+4y3+...+2yn-2+4yn-1+yn]
= (h/3)[y0+4y1+y2
              +y2+4y3+y4
                     +y4+5y5+y6
                            +y6+...]
```

ってことっぽいですね
ひとかたまりだけ取り出すとこういうことかと

3点(x1,y1),(x2,y2),(x3,y3) (ただしx2=x1+h、x3=x2+h)を通る放物線y=g(x)について
```
∫[x1 x3]g=y1+4y2+y3
```

ほんと？
2次曲線ですから2次っぽい項がでてきてもよさそうなものですが
ちょっとそこだけ心配
ちょっと確かめてみるか

ストレートにy=g(x)=ax^2+bx+cと置いてみます

```
  ∫[x1 x3]f
≒ ∫[x1 x3]g
= ∫[x1 x3](ax^2+bx+c)dx
= [(a/3)x^3+(b/2)x^2+cx][x1 x3]
= (a/3)(x3^3-x1^3)+(b/2)(x3^2-x1^2)+c(x3-x1)
= (略)
= (h/3)(y1+4y2+y3)
```

一応確かめることはできました
積分なんか計算したのは何十年ぶりだろうか

途中でx3-x1=2hとかx3+x1=2x2とかx3x1=x2^2-h^2とか
y1=ax1^2+bx1+cとかy1+y3=2y2+2ah^2とか使ってます
手品感満載
できあがりの形を知ってたからできましたが導き出したという感じではないなあ
一般化してn+1個の点を通るn次式の積分なんかもできそうな雰囲気だけど
こんな式変形では一般化できそうにありません
がいいアイデアもないのでここは先に進むことにします


ではプログラムにします（やっと

```
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-dx a) (+ a h))
```

ああ、またさっきと同じ問題が起きるな・・・
ていうか係数が4242てなるのなんてsumじゃ書けなくない？
今何番目の項を計算してるかなんてわからないし
渡せばいいけどなにか違う

sumは使わなくていいのかな・・・
そしたらさっきの問題も解決できるし
それでいい？

```
(define (simpson-integral f a b n)
  (define r (- b a))
  (define h (/ r n))
  (define (iter c sum)
    (define x (+ a (/ (* r c) n)))
    (define co (cond ((or (= c 0) (= c n)) 1)
                     ((odd? c) 4)
                     (else 2)))
    (if (> c n)
        sum
        (iter (+ c 1)
              (+ sum (* co (f x))))))
  (* (/ h 3) (iter 0 0)))
```

しかし繰り返しプロセスをいちいちこれで書くのはちょっと面倒だなあ
自然な再帰で書こうとするとnから0に向かって進む感じになってちょっと違和感だし
と思うのは頭が手続き型なんでしょうか

で実行

```
> (simpson-integral cube 0.0 1.0 100)
0.25000000000000006
> (simpson-integral cube 0.0 1.0 1000)
0.25000000000000006
```

おお、めっちゃ優秀
収束が速い速い

10回くらいでも十分？

```
> (simpson-integral cube 0.0 1.0 10)
0.25
```

え？

```
> (simpson-integral cube 0.0 1.0 4)
0.25
> (simpson-integral cube 0.0 1.0 2)
0.25
```

ええええ？

何ソレ
バグでも作りこんだ？
いやそれにしちゃうまくいきすぎてる

ちょっと他の関数でやってみる

```
> (simpson-integral sin 0.0 pi 10)
2.000109517315004
> (simpson-integral sin 0.0 pi 100)
2.0000000108245044
> (simpson-integral sin 0.0 pi 1000)
2.0000000000010805
> (simpson-integral sin 0.0 pi 10000)
1.9999999999999925
```

うん、フツーな感じ

じゃあcubeのintegralは何なの
たまたま誤差0で近似できてるってこと？
えー？

x1からx3までx^3を近似することにして計算してみる
まずは普通に積分するとこう

```
∫[x1 x3]x^3dx = (1/4)(x3^4-x1^4)
```

次は近似式で
h=(x3-x1)/2、x2=(x3+x1)/2だから

```
  ∫[x1 x3]x^3dx
≒ (h/3)(x1^3+4(x2^3)+x3^3)
= ((x3-x1)/6)(x1^3+4(((x3+x1)/2)^3)+x3^3)
```

ん、なんかこれは・・・

```
= ((x3-x1)/6)(3/2)(x1^3+x1^2x3+x1x3^2+x3^3)
= (1/4)(x3-x1)(x1^3+x1^2x3+x1x3^2+x3^3)
= (1/4)(x3^4-x1^4)
```

おー
一致した

* さっきやったcubeと比較してみろ

ってどういう意図だったの
意地悪なの
びっくりするじゃないですか
