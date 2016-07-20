# SICPを読む(10)

## 1.3.4 Procedures as Returned Values

* 引数として手続きを渡して渡せるようにすると表現力がとても上がることがわかった
* 値として手続きを返せるようになるとさらに表現力が上がる
* average dampingを手続きとして書いてみる

```
(define (average-damp f)
  (lambda (x) (average x (f x))))
```

* average-dampの値はlambdaで作られた手続きとなる
* average-dampを使うとsqrtはこうなる

```
(define (sqrt-4 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
```

前に書いたのはこう

```
(define (sqrt-3 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))
```

yとy/xの平均を取ってますよ、という書き方から
y/xをaverage-dampしてますよ、という書き方になりました

* fixed-point search、average damping、y→x/yという３つのアイデアを明示している
* 1.1.7でやったsqrtとは同じプロセスだが比較してみるとためになる

これ

```
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
```

* 考え方がより明確になった
* プロセスを手続きとして定式化する方法はたいていいくつもある
* わかりやすい定式化を選びなさい
* 他で再利用しやすい、独立した要素を切り出しなさい

このへん大事なところっぽい

### Newton's method

* ニュートン法の一般形は以下のとおり
  * x→g(x)が微分可能であれば、g(x)=0の解はx→f(x)の不動点である
  * ここでf(x)=x-g(x)/Dg(x)
* これを手続きとして表すには、まず微分を手続きにしなければならない
* 微分とはある関数を他の関数に変換する手続きである
* dxを小さい数とするとg(x)の微分はDg(x)=(g(x+dx)-g(x))/dxと書ける

これはそのままプログラムできます

```
(define dx 0.00001)
(define (deriv g) (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
```

`cube`をy=x^3を表す手続きとすると、y=x^3の微分は`(deriv cube)`であり
x=3におけるyの微分は`((deriv cube) 3)`と表せます

これを使うとニュートン法がプログラムできます

```
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
```

平方根はこうやって求められます

```
(define (sqrt-5 x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))
```

さらっと見るだけだとなんとなく混乱します
上に書いたニュートン法の説明はxを求めてますが、この版のsqrtでは
xの平方根を求める、つまりy^2-x=0を解くためにg(y)=y^2-xとおく、
というところからスタートしてるのでした

`square`を`cube`に変えるだけで立方根も求められます
自分で式を変形したりする必要がなくなりました

### Abstractions and first-class procedures

* 平方根を求めるのに、不動点探索を使う方法とニュートン法を使う方法を試した
* ニュートン法は実は不動点探索としても表現できる
* どちらも、ある関数から始めて、関数を変形してから不動点を求めた
* これもまた手続きとして一般化できる

```
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
```

この手続を使うと不動点方式はこう

```
(define (sqrt-6 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
```

ニュートン法はこう書ける

```
(define (sqrt-7 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
```

抽象化とは、同じ形の手続きを探してきて、異なる部分を引数にすることである
と言えそうな気がしてきました

* プログラムの裏に潜む抽象を見つけ、それを基礎として構築し、一般化してさらに強力な抽象を生み出す

ここまででやってきたことですね

* いつもいつも限界まで抽象化しなければいけないわけではないが
* 抽象された観点で思考し、新たな文脈にそれを適用できることは重要

実践はなかなかうまくいきませんが

* こういった抽象を明示的にプログラミング要素として表したものが高階関数
* 高階関数は他のプログラミング要素と同じように扱えるべき
* こういった、制限の少ないプログラミング要素を「ファーストクラス」と呼ぶ
* ファーストクラスであるというのはたとえばこういうこと
  * 名前がつけられる
  * 引数として手続きに渡せる
  * 手続きの値として返すことができる
  * データ構造に含めることができる
* Lispの手続きは完全なファーストクラス
* 効率的な実装は難しいが、表現力の増加はとても大きい

最後はお決まりの表現力自慢で第１章を締めました

振り返ると、1.1.7で

```
(define (sqrt x) (the y (and (>= y 0) (= (square y) x))))
```

なんて書いてもダメだよーというところから始まったニュートン法の話ですが
なんとなく似た感じにも見えなくもないところまで来ました
手続き型と宣言型で考え方はぜんぜん違うんですが

お見事、て感じです

### Exercise 1.40.

* x^3+ax^2+bx+cを定義して、ニュートン法でゼロ点を近似せよ

```
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
```

x^3-x^2-x-2=0のゼロ点を求めてみます
実際の値はx=2

```
> (newtons-method (cubic -1 -1 -2) 1.0)
2.000000000000005
```

おｋ

### Exercise 1.41.

* 1引数の手続きを引数として受け取り、その手続を2回適用する手続きを返す手続きを書け

```
(define (twice f) (lambda (x) (f (f x))))
```

(doubleは定義済みだったので名前を変えました)

* `(((twice (twice twice)) inc) 5)`の値を求めよ

`(twice twice)`で4回だからそれを2回で8回適用かな


```
> (((twice (twice twice)) inc) 5)
21
```

あらそうなるの
16回適用してるってことだな
(twiceを2回)×(twiceを2回)だからtwiceを4回してることになるのか
そうか

確かめてみるかな
キモは`(twice (twice twice))`の部分だからそこだけ取り出してやってみます

`((twice (twice twice)) inc)`を
`(twice (twice (twice inc)))`と書いても結果は同じだと思いますが
こうやって取り出せるということは
パーツに分けるという観点からして前者のほうがいい感じなんでしょうね

置換しやすくしたいのと文字数節約のため
`(define tw (λ(f)(λ(x)(f(f x)))))`ってことでいきます


```
  (tw (tw tw))
= (tw ((λ(f)(λ(x)(f (f x)))) tw))
```

ただの置き換えです
`(define tw (lambda ...))`式の定義はこういう置換が単純にできるのが利点
どこを置き換えるかは恣意的に決めてます

```
= (tw (λ(x)(tw (tw x))))
= ((λ(f)(λ(x)(f (f x)))) (λ(x)(tw (tw x))))
= (λ(x)((λ(x)(tw (tw x))) ((λ(x)(tw (tw x))) x)))
```

気持ちとしてはこれを`(λ(x)(tw (tw (tw (tw x)))))`としてみたいんですが
そういうことができるというルールってあったかな
`((λ(x)(A x)) (λ(x)(B x))) = (λ(x)(A (B x)))`みたいな
ルールでもあればいいんでしょうけど
でもとりあえずここではこれ以上簡約しないことにします

上の関数をincに適用したとすれば先へ進めます

```
→ ((λ(x)(tw (tw x))) ((λ(x)(tw (tw x))) inc))
= ((λ(x)(tw (tw x))) (tw (tw inc)))
= (tw (tw (tw (tw inc))))
```

さっぱりしました
incにtwを4回適用してますので2x2x2x2で16回適用ですね

でも最後まで監訳しようと思うとまだ道は半ばです
ていうかこの形に持ってくるだけでこんな手間とは

```
= (tw (tw (tw ((λ(f)(λ(x)(f (f x)))) inc))))
= (tw (tw (tw (λ(x)(inc (inc x))))))
```

文字数省略のため`(λ(x)(inc (inc x)))`を`LXI2X`と書くことにします
以下同様

```
= (tw (tw (tw LXI2X)))
= (tw (tw (λ(x)(LXI2X (LXI2X x)))))
= (tw (tw LXI4X))
= (tw (λ(x) (LXI4X (LXI4X x))))
= (tw LXI8X)
= (λ(x) (LXI8X (LXI8X x)))
= LXI16X
```

incを16回繰り返す関数になったようです
意外と奥が深い？

xには5とかが入って関数はfに渡される、っていう先入観があったので
xにincが入るところとかで意外と混乱したりしました

### Exercise 1.42.

* 関数fとgを合成する手続きを書け

x→f(g(x))というのが合成だそうですからそのまま書くだけ

```
(define (compose f g) (lambda (x) (f (g x))))
```

### Exercise 1.43.

* 関数fをn回適用する手続きを書け
* composeを使うと便利

```
(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))
```

### Exercise 1.44.

* fをsmooth化する関数を書け
* fをsmooth化した関数というのは、xでの値がf(x-dx)、f(x)、f(x+dx)の平均となる関数のこと
* fをn回smooth化する関数も書け

```
(define (average3 a b c) (/ (+ a b c) 3))
(define (smooth f)
  (lambda (x) (average3 (f (- x dx)) (f x) (f (+ x dx)))))
(define (nth-smooth f n)
  ((repeated smooth n) f))
```

smoothになってるなあ、と実感できる例が思いつかない
きっと合ってると信じて進む

