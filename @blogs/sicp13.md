# SICPを読む(13)

## 2.1.4 Extended Exercise: Interval Arithmetic

もうひとつの例として区間演算を取り上げます
回路の抵抗の話で説明されてますがそのへんは割愛

* ふたつの端点で表された「区間」という抽象化されたオブジェクトがあるものとする
* make-interval、lower-bound、upper-boundが使えるものとする

コンストラクタとセレクタが「あるものとする」やり方ですね
中身はあとで考える式

```
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

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
```

うまくいきそうではありますがまだ動かせません

mul-intervalは場合分けしても書けそうですけど面倒くさそう
場合分けがきちんとわかってないとテスト書けませんけど
でもここはちょっとスルー

### Exercise 2.7.

* make-intervalは`(define (make-interval a b) (cons a b))`と書ける
* upper-boundとlower-boundを定義せよ

```
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
```
### Exercise 2.8.

* 区間の差を定義せよ

```
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
```

### Exercise 2.9.

* 区間の和（差）の誤差の幅は、加えられる２区間の誤差の幅の関数であることを示せ

区間i1[a1,a2]と区間i2[b1,b2]について考えます
区間の和は[a1+b1,a2+b2]ですから和の誤差の幅はw=(a2+b2)-(a1+b1)
２区間の誤差の幅はそれぞれw1=a2-a1、w2=b2-b1ですから
w=w1+w2となります

同様に区間の差は[a1-b2,a2-b1]ですからw=(a2-b1)-(a1-b2)
このときもやはりw=w1+w2となります

プログラミングがまったく出てきてませんがいいんでしょうか

* 区間の積（商）の誤差の幅は、加えられる２区間の誤差の幅の関数でない例を示せ

例ってどういう例でしょうか
[2,4]×[3,5]=[6,20]で2,2から14とか
[3,8]÷[2,3]=[1,4]で5,1から3とか
そういうんじゃ関数じゃない例とは言えない気がします
w=w1×w2+10と書けるんです！て言われたら困ります

a1,a2,b1,b2がすべて正なら
区間の積は[a1×b1,a2×b2]で誤差の幅はa2×b2-a1×b1
これはw1とw2では書けそうにありません
割り算でも同様

の方が説得力ありますね
「例としてa1,a2,b1,b2がすべて正の場合を取り上げる」とか言えば
例を挙げたって言えるのかなあ
全部の場合を考えなくていいよ、っていう意味ならこれでいいのかな

### Exercise 2.10.

* 0を含む区間で割った時はエラーになるようにせよ

0を含むかどうかは上端と下端の積が0以下かどうかで判定できます
これくらいならかっこつけすぎってことはないよね

```
(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
```

### Exercise 2.11.

* 端点の符号で9つに場合分けしてmul-intervalを作れ
* かけ算が2回で済まないのは9つのうちひとつだけ

場合分けスルーできませんでした

普通なら正,0,負で分けるところを
ここでは端点がふたつとも正、異符号、ふたつとも負、に分ける感じでしょうか
端点が0の場合はどうするといいかな
もう一方が正か負で分ける？
それとも異符号の仲間に入れる？
感覚的にはどっちでもいけそう
異符号の仲間に入れよう
あとで確かめる

区間の符号（と呼ぶことに）を求める関数でも作っときますか
下端<=上端は間違いないから両方チェックする必要はないよね

```
(define (plus?-interval x) (> (lower-bound x) 0))
(define (minus?-interval x) (< (upper-bound x) 0))
```

愚直に場合分け

```
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
```

else→elseのところは
下端がa1×b2・a2×b1の小さい方、下端がa1×b1・a2×b2の大きい方、なんですが
計算してみないとわからないので４回かけ算が必要になります
つまりmul-intervalと同じことをやらなきゃいけないので呼び出しで書きました

愚直に場合分けして、式が重複してたらまとめようかと思いましたが
重複してるところはありませんね
全部で16とおりある組み合わせのうち8通りが登場しています
なにか規則性はあるかな？

しかしこれ、常に正しい答えを出すという自信が持てないですね
テスト書いてみました
mul-intervalの中まで心配した書き方です

```
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
```

書いたけど考慮漏れとかあるかもしれないし安心できる気がしません
そうそう、端点に0を含む場合も確かめようとしてたんでした

```
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
```

考えうる限りのすべての組み合わせを試したわけじゃないですがまあ大丈夫そうではあります
でもお仕事とかならやってみたらうまくいってるみたいです、とか言ってちゃダメな気がします
実際やることになったら普通にmul-intervalでやりたい
よっぽど性能がクリティカルでかつかけ算がとても高価なときに泣く泣くいやいや書く、くらいかな

演習から本文に戻ります
要件が変更になりました

* 中心と幅で扱えるようにせよ

```
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
```

内部の表現を隠しておいたので
コンストラクタとセレクタを追加しただけで同じように区間を扱えるようになりました

```
> (add-interval (make-center-width 2 1) (make-center-width 4 2))
'(3 . 9)
```

ってことですね

### Exercise 2.12.

* 誤差の幅を％で表せるようにせよ
* パーセントを返すセレクタも作れ

さっき幅で書いてたところがパーセントになっただけなので
さっき作った関数を大々的に使います

```
(define (make-center-percent c p)
  (make-center-width c (* c p 0.01)))
(define (percent i)
  (* (/ (width i) (center i)) 100))
```

### Exercise 2.13.

* 誤差の％が小さければ、区間の積の誤差を近似する簡単な式があることを示せ
* 数はすべてせいであると仮定してよい

```
  [c1-c1p1,c1+c1p1]×[c2-c2p2,c2+c2p2]
= [(c1-c1p1)×(c2-c2p2),(c1+c1p1)×(c2+c2p2)] ;数がすべて正だから
= [c1c2-c1c2p2-c2c1p1+c1p1c2p2,c1c2+c1c2p2+c2c1p1+c1p1c2p2]
= [c1c2(1-(p1+p2)+c1c2p1p2),c1c2(1+p1p2)+c1c2p1p2]
```

誤差のwidthは

```
  ((c1c2(1+(p1+p2)+c1c2p1p2))-(c1c2(1-p1p2)+c1c2p1p2))/2
= c1c2(p1+p2)
```

あれーここでp1,p2は小さいからp1p2はもっと小さくて無視できる、ってやるつもりだったのに
p1p2が消えちゃった
大丈夫かな

誤差のパーセントは、えっとcenterで割ればいいんだな
centerは

```
  ((c1c2(1-(p1+p2)+c1c2p1p2))+(c1c2(1+p1p2)+c1c2p1p2))/2
= c1c2+c1c2p1p2
= c1c2(1+p1p2)
≒ c1c2
```

こっちで出てきました
というわけで誤差のパーセントはだいたい

```
  c1c2(p1+p2)/c1c2
= p1+p2
```

どれどれ

```
> (mul-interval (make-center-percent 100 1) (make-center-percent 200 2))
'(19404.0 . 20604.0)
> (center (mul-interval (make-center-percent 100 1) (make-center-percent 200 2)))
20004.0
> (percent (mul-interval (make-center-percent 100 1) (make-center-percent 200 2)))
2.9994001199760048
```

だいたいあってる

このへんは工学の人向けでしょうか
また本文に戻ります

この関数たちを使って以下のふたつの式を計算します
変形しただけで同じ式です
抵抗R1、R2を並列に接続した時の抵抗値を表しています

```
R1R2/(R1+R2)
1/(1/R1)+(1/R2)
```

ところが計算の結果が違うと

Schemeで書くとこんな関数

```
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
```

計算してみます

```
> (define R1 (make-center-percent 100 10))
> (define R2 (make-center-percent 200 10))
> (center (par1 R1 R2))
69.36026936026937
> (percent (par1 R1 R2))
29.223300970873783
> (center (par2 R1 R2))
66.66666666666667
> (percent (par2 R1 R2))
10.000000000000005
```

けっこう違いますね
誤差が30％にもなったり
centerまでずれるのはちょっと意外でした

さっきの演習でやりましたが
かけ算のときは誤差の幅がかけられる数の誤差の和になるのが効いてそう

誤差がなければ66.666...になります
ふたつ目の式のほうが明らかに性能がいいですね
oneには誤差がないので、oneとの演算では誤差が大きくならないってことかな

中途半端なところですが今回はここまで
