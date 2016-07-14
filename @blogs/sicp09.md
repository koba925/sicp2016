# SICPを読む(9)

## 1.3.3 Procedures as General Methods

* 特定の数から独立した数値計算の抽象化と、特定の関数から独立した汎用的手法の抽象化を見てきた
* さらにふたつの例を見ていく

まだ数学っぽい話が続きますよ
### Finding roots of equations by the half-interval method

* 二分法はf(x)=0となるxを見つけるための強力な手法

このあと二分法を言葉で説明してますがコードのまんまですね
コードが文章のまんまなのか

```
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
```

プログラミングが必修になるなら、
数学の教科書に二分法はこうです、ってコードで書くようにしてみたらどうでしょうかね
一石二鳥じゃないでしょうか（半分くらい本気

### Finding fixed points of functions

* f(x)=xであるときxをfの不動点という
* fによっては、fを繰り返し適用することにより不動点を求めることができる
* つまりf(x)、f(f(x))、f(f(f(x)))・・・と値があまり変わらなくなるまで続ける

不動点といえば例の不動点オペレータを思い出します
やはりなにか重要なものなんですかね
ここではxは数って言ってますけどね

```
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
```

* 求める精度に達するまで繰り返し予測値を改善する
* そのへんはニュートン法で平方根を求めたやり方と似ている
* ところが平方根を求めるためにy^2=xを変形してy→x/yとすると、これは収束しない

guessを毎回表示させてみるとこうなります

```
> (fixed-point (lambda (y) (/ 2 y)) 1.0)
1.0
2.0
1.0
2.0
```

* これを避けるための一つの方法は、元の推測値と、改善された値の推測値の平均をとってやること
* この例で言えばy→(1/2)(y+x/y)としてやること
* y=(1/2)(y+x/y)はy=x/yを単純に変形したものであることに注意
* このやり方をaverage dumpingと言い、不動点を探すのに役立つ

```
> (fixed-point (lambda (y) (average y (/ 2 y))) 1.0)
1.0
1.5
1.4166666666666665
1.4142156862745097
1.4142135623746899
```

おお、精度の高まり方がすごいな

* こうやって変形すると、実はニュートン法で平方根を求めるのと同じことになっている

ほんとだ
improveと同じ式
不動点＋average dampingには微分なんか出てきてないけどどこで同じになったんだろう？
たまたま？

ニュートン法では1回ごとに合ってる桁数が倍になるとか
上の例だと1桁→1桁→3桁→6桁→12桁
確かにそんな感じ
超優秀

試しに他の式でやってみると

```
> (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
1.0
1.3817732906760363
1.1700876970971783
1.3108557450190208
1.2234283819604843
1.2806961256602427
1.2442634587575785
1.2679211886945934
1.2527485860569305
1.2625605329565772
1.256248323727693
1.2603230004268888
1.2576984377593448
1.2593913555020237
1.2583003677191482
1.2590038597400248
1.2585504047403622
1.2588427629092844
1.2586542990963894
1.2587758014705526
1.2586974741689445
1.2587479705055356
1.2587154172236283
1.258736403592101
1.2587228743052672
1.2587315962971173
```

ちっとも優秀じゃなかった

### Exercise 1.35.

* φはx→1+1/xの不動点であることを示せ

φ^2=φ+1です
x→1+1/xの両辺にxを掛ければx^2=x+1

* そのことを利用して、φを`fixed-point`で求めよ

```
> (define φ (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
> φ
1.6180327868852458
> (+ 1 (/ 1 φ))
1.618034447821682
```

試しにφ使ってみましたがすんなり使えました
Racketだとこういうのもいけます

```
> ((λ (x) (* x x)) 3)
9
```

### Exercise 1.36.

* 途中経過を表示するようにせよ

`(define (try guess) (display guess) (newline) ...)`とするだけ

* x^x=1000となるようなxを求めよ

両辺の対数をとれば・・・

* つまりx=log(1000)/log(x)となるようなxを求めよ

```
> (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
2.0
9.965784284662087
3.004472209841214
6.279195757507157
3.759850702401539
5.215843784925895
4.182207192401397
4.8277650983445906
略
4.555527808516254
4.555540912917957
4.555532270803653
```

確かめ

```
> (expt 4.555532270803653 4.555532270803653)
999.9913579312362
```

（このexptはSICPで作ったやつじゃなくてRacketについてくるexptです）
大丈夫そうです

### Exercise 1.37.

* 無限連分数とは以下のような式

```
            N1
f = ------------------
              N2
    D1 + -------------
                 N3
         D2 + --------
              D3 + ...
```

これは書くのめんどうだな・・・

```
f = N1/(D1+N2/(D2+N3/(D3+...)))
```

ちっとも連分数に見えない

* たとえばDiとNiが全部1なら1/φになる

```
            1
f = -----------------
              1
    1 + -------------
                 1
         1 + --------
              1 + ...
```

これよくみるとfとおんなじ形が分母に出てきてて

```
      1
f = -----
    1 + f
```

両辺に1+fを掛け算してやるとf^2+f=1からf=(-1+√5)/2 (f>0だから)
一方1/φ=1/((1+√5)/2)=2/(1+√5)=(-1+√5)/2
よし

* Dk、Nkまでで止めれば近似値を求めることができる
* そういう関数を書け

まずは再帰プロセスで書いてみよう
今まで再帰プロセスだと状態を表すような変数使ってこなかった気がするけど
これは第何項めかを変数にするしか思いつかないゾと

```
(define (cont-frac n d k)
  (define (C i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (C (+ i 1))))))
  (C 1))
```

こうしか書けませんでした
いいのかなこれで

カウンタが入るだけで繰り返しプロセスに見えてしまうのは理解が浅いからか

* 4桁の精度を出すにはkをいくつにすればいい？

ちょっと見当をつけます

```
> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
0.6179775280898876
> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
0.6180339887498948
> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1000)
0.6180339887498948
```

k=100ではもう十分なようですね
ていうか四捨五入すれば10でも足りてるくらいですね
'accurate to 4 decimal places'てのはどういう意味なんだろう
まあ数字が４つ一致するまで、としておきます

念のため答えも確認

```
> (/ 1 (/ (+ 1 (sqrt 5)) 2))
0.6180339887498948
```

おｋ

ここから先はやはりCPUに働いてもらいましょう

```
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
```

なんか不要に一般化してる気もしますが
DRYは気にするけどYAGNIはあんまり気にしないんです！

```
> (count-for-accuracy (/ 1 (/ (+ 1 (sqrt 5)) 2))
                      (lambda (k) (cont-frac (lambda (i) 1.0)
                                             (lambda (i) 1.0)
                                             k))
                      4)
11
```

がーん

```
> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
0.6180555555555556
```

そうですねあと1回試しておけば終わってましたね
まさにYAGNIでした

悔しいので10桁の精度を得るにはいくつまでやる必要があるか計算してみます

```
> (count-for-accuracy (/ 1 (/ (+ 1 (sqrt 5)) 2))
                      (lambda (k) (cont-frac (lambda (i) 1.0)
                                             (lambda (i) 1.0)
                                             k))
                      10)
25
```

確かめ

```
> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 24)
0.6180339886704432
> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 25)
0.6180339887802426
```

はいOKです

* 繰り返しプロセスで書け

んーとこれはどうすればいいんだえーと
kから1に向かうようにすると楽かな

```
(define (cont-frac-i n d k)
  (define (iter i ans)
    (if (= i 0)
        ans
        (iter (- i 1) (/ (n k) (+ (d k) ans)))))
  (iter (- k 1) (/ (n k) (d k))))
```

そういえば変数名のつけかたに一貫性がない
技術的負債ってやつか！
無視

### Exercise 1.38.

* オイラーによるとe-2はNi=1、Di=1,2,1,1,4,1,1,6,1,1,8・・・という連分数になる
* eの近似値を求めよ

出たなオイラー先生
なんでそうなるのかはちっともわかりませんがコードにするのは別に問題ありません
Diの表し方くらいですかね

```
(define e (+ 2 (cont-frac (lambda (i) 1.0)
                          (lambda (i)
                            (if (= (remainder i 3) 2)
                                (* 2 (/ (+ i 1) 3))
                                1))
                          10)))
```

### Exercise 1.39.

* Lambertの式を使ってtan xを近似せよ

```
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))
```

# SICPを読む(10)

## 1.3.4 Procedures as Returned Values

* 引数として手続きを渡して渡せるようにすると表現力がとても上がることがわかった
* 値として手続きを返せるようになるとさらに表現力が上がる
* average dampingを手続きとして書いてみる

```
(define (average-damp f)
  (lambda (x) (average x (f x))))
```

* average-dumpの値はlambdaで作られた手続きとなる
* average-dumpを使うとsqrtはこうなる

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
お見事です

振り返ると、1.1.7で

```
(define (sqrt x) (the y (and (>= y 0) (= (square y) x))))
```

なんて書いてもダメだよーというところから始まったニュートン法の話ですが
なんとなく似た感じにも見えなくもないですね
手続き型と宣言型で考え方はぜんぜん違うんですが

もちょっと続きます




