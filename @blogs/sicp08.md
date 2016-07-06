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

### Exercise 1.31.

* sumは高階手続きで抽象できる膨大な例のひとつにすぎない
* sumと同様にしてproductを定義し、factorialとπを計算せよ

productは簡単

```
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
```

factorialはsum-integerみたいなもの

```
(define (factorial-p n)
  (product identity 1 inc n))
```

```
> (factorial-p 3)
6
```

πはこの式をもとに計算します

```
     2・4・4・6・6・8・・・
π/4= --------------------
     3・3・5・5・7・7・・・
```

不思議ですねえ

しかしこれ、どうやってproductにかけましょうか
sumと同様、productの中では第何項めかはわからないし
て、第a項めってことにすればいいのか
2/3とか6/7をaにしなきゃいけない気になってた

さらに作りやすいように式をこんな風に変形

```
π=4*2*(4/3)^2*(6/5)^2/7 
```

こうしなくてもif使えば書けると思いますけど

```
(define (pi-p n)
  (define (term n)
    (square (/ (+ (* 2 n) 2.0) (+ (* 2 n) 1.0))))
  (/ (* 4 2 (product term 1 inc n))
     (+ (* 2 n) 3)))
```

やってみます

```
> (pi-p 10)
3.0740551602804405
> (pi-p 100)
3.133864293497813
> (pi-p 1000)
3.140808529664476
```

これはなかなか遅い収束ですね

> Exercise 1.29.
> 
> ていうか係数が4242てなるのなんてsumじゃ書けなくない？

ていうかそんなことなくない？

```
(define (simpson-integral-h f a b n)
  (define r (- b a))
  (define h (/ r n))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (f (+ a (/ (* r k) n)))))
  (* (/ h 3) (sum term 0 inc n)))
```

書けた

とはいうもののテキスト的には`add-dx`で足し算していくのが流れな気もする
いや、そうじゃないな
引数にdxではなくnを取れと言ってるんだから`add-dx`にこだわることはないのか
納得した

式の中にcond書くとかやめたほうがいいかな
defineすればいいだけだけど
rとかhとかdefineしてるところは、再帰のたびにrとかhとか計算したりしてないだろうか
ちゃんとやってくれてるかな
自分が処理系作ったらきっと毎回計算するね

### Exercise 1.32.

* sumとかproductをもと一般化したaccumulateを書け

combinerとnull-valueのふたつの引数が増えてます
これはsumとproductの相違点をさらに指定できるようになったテンプレート

これはなんか見たことあります
ていうかfoldじゃね？
でもなんかごちゃっとしてます
何が変なんだろうと思ったら注51に書いてありました
まだリストが出てきてないからです
そりゃそうだ
リストで渡せないから代わりに(term a next b)で数列を作ってるということですね

答えは特に難しくないです

```
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
```

sumやproductを定義するとこうなります

```
(define (sum-a term a next b)
  (accumulate + 0 term a next b))
(define (product-a term a next b)
  (accumulate * 1 term a next b))
```

* 繰り返しプロセスでも書け

はいはい

```
(define (accumulate-i combiner null-value term a next b)
  (define (iter c ans)
    (if (> c b)
        ans
        (iter (next c) (combiner (term c) ans))))
  (iter a null-value))
```

もしかしてcはaっていう名前のほうがわかりやすいのかと思ったけどたぶんそんなことはない

### Exercise 1.33.

* accumulateをさらに一般化して項にフィルタをかけられるようにせよ

ほいきた

```
(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter c ans)
    (if (> c b)
        ans
        (if (filter c)
            (iter (next c) (combiner (term c) ans))
            (iter (next c) ans))))
  (iter a null-value))
```

なんとなく気分で繰り返しプロセス
ますますごっちゃりしてきましたが気にしない
リストがないからね！

* aからbまでの素数の2乗の和を求めよ

```
(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))
```

* nまでの正の整数のうちnと互いに素なものの和を求めよ

互いに素っていうのはGCDが1ってことだから

```
(define (product-of-relatively-primes n)
  (filtered-accumulate * 1 (lambda (m) (= (gcd n m) 1))
                       identity 1 inc n))
```

しまったlambdaが出てくるのは次だった（白々しい
## 1.3.2 Constructing Procedures Using Lambda

* pi-termとかpi-nextとかいちいち定義するのめんどくさいよね

そうだそうだ

* lambdaを使え

おっしゃるとおりにいたします

* lambdaはdefineと同じ手続きを作る
* 名前がついてないだけ
* 実は`(define (plus4 x) (+ x 4))`は`(define plus4 (lambda (x) (+x 4)))`と同じ

手習いと修行で飽きるほど書きました

> ```
>     (lambda             (x)             (+    x     4))>  the procedure   of an argument x  that adds  x and 4
> ```

負けるな日本人

* lambda式も演算子として使える こんな感じ  
`((lambda (x y z) (+ x y (square z))) 1 2 3)`

## Using let to create local variables

* lambdaはローカル変数を作るのにも使える
* これは使いでがあるので、もっと便利に使えるようにletというのがある
* letは特に新しいしくみは必要としてなくてただのシンタックスシュガー

このへんももうお手のもの
あとletで定義した変数のスコープの話
letの本体でしか使えません

lambdaとletが出てきて、letrecは内部defineで代用できますので
手持ちの武器はこれで全部使えるようになったかな（少な

* letは内部defineで代用できる
* でも内部defineで定義するのは内部手続だけにしておくよ

ローカル変数はletで定義するよという意味
letとletrecだとなれない人にはまぎらわしいってことかな



