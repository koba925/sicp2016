# SICPを読む(11)

## Chapter 2 Building Abstractions with Data

ここは第2章の概要
コードは出てきません

* 第1章では手続きを組み合わせて合成手続きを作ることにより抽象化を行った
* 第2章ではデータを組み合わせて合成データを作ることによる抽象化を行う
* 手続きもデータも、抽象化することにより概念のレベルを上げることができる
* たとえば、有理数はふたつの整数、つまり分母と分子で表すことができる
* しかし、ふたつの整数がばらばらに存在しているだけでは扱いが煩雑

データを組み合わせる方法がないとすると、たとえば1/2+1/3=5/6を表現しようとしたら

```
(add-numer n1 d1 n2 d2)
(add-denom n1 d1 n2 d2)
```

などといった関数を作り
`(add-numer 1 2 1 3)`から5を得、
`(add-denom 1 2 1 3)`から6を得る、といったことをしなければなりません
構造体とかが普通にある世界にいると逆にこういった状況のほうが想像しづらいくらいですけど

やりたいことは`1/2`や`1/3`を直接データで表し
そのまま手続きに渡して
`5/6`という値を得ることです

* 分子と分母を組み合わせて、有理数を表すひとつのデータとして扱う方法がある
* ひとつのデータとして扱えるようになると、それがふたつの整数で表されているということも考えずに済むようになる
* これをデータ抽象化と呼ぶ
* データ抽象化によって、言語の表現力が増し、プログラムの設計・維持・修正が容易になる

手続きによる抽象化と、データ抽象化が対になって説明されてます

* 線形結合を例に取ると、ax+byを`(add (mul a x) (mul b y))`と表し、a・b・x・yが数の時ばかりでなく、addとmulが定義されているすべての対象を扱えるようにすることができる
* このとき、a・b・x・yが何で、内部でどう表現されているかを知る必要はなく、ただaddとmulが定義されている対象であることを知っていればよい
* データ抽象化によって、プログラムのパーツとパーツの間に「抽象化の壁」を築いていく

えーとこれは・・・多相性、とはちょっと違う？
なんだっけ？

* 複雑なデータオブジェクトを作るには、データオブジェクトを組み合わせるための手段が必要
* いろいろな手段が考えられるが、データを操作するための特別な操作を使わないで、手続きだけを使ってデータオブジェクトを組み合わせることもできる
* クロージャという概念がひとつの鍵
* もうひとつの鍵は、抽象化されたデータがモジュールを組合わせる時のインタフェースとなること

'conventional interface'っていうのがいまひとつピンとこなくて日本語にできません
読んでいけばわかるかな

* `symbolic expression`でさらに言語の表現力を高める
* オブジェクトの集まりを表すにはさまざまな方法があり、時間やスペースの消費が大きく異なる
* 場所によって異なる表現をされたデータを'generic operations'によって扱う
* 特に、'data-directed programming'を使えば後からデータ表現を追加することもできる

まあいろいろやるよってことですかね

## 2.1 Introduction to Data Abstraction

* データを抽象化すれば、複合データの使い方と、複合データの構造を分けて考えることができる
* データ抽象化とは、プログラムが「抽象データ」を扱うように構造を作ること
  * プログラムは、タスクを実行するのにどうしても必要なこと以外は仮定しない
  * 「具体的な」表現は、そのデータを使うプログラムとは独立して定義される
  * これらの間のインタフェースは'selector'と'constructor'と呼ばれる手続きで表される

モジュールとかクラスの設計とかでもきっと大事

'selector'っていう言い方は手習いにも出てきてましたね
'constructor'はなんでしょう
普通にコンストラクタって呼ばれてるアレのことかな

## 2.1.1 Example: Arithmetic Operations for Rational Numbers

有理数の加減乗除と比較ができるようにするよ！

* まず、分母と分子から有理数を作る手続きと、有理数から分母と分子を取り出す手続きがあると仮定する

これが'constructor'と'selector'てことですね
それほど大層な話ではありませんでした

* `(make-rat <n> <d>)`はn/dという有理数を返す
* `(numer <x>)`は有理数xの分子を返す
* `(denom <x>)`は有理数xの分母を返す

けど、ここではあくまで「仮定する」であって「書いてみる」ではないぞ、って
言いたいんでしょうか
こういう考え方をしたほうが「タスクを実行するのにどうしても必要なこと以外は
仮定しない」を徹底できるぞ、ってことでしょうか
自分はついボトムから書いてちょっとずつ動かしたくなりますけど

* `wishful thinking`という戦略を使った
* いまのところ有理数をどうやって表すかはわからないけれども
* とにかくこの３つがあれば四則演算と比較を行う手続きを書くことができる

書けますね
動かせないけど

```
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
```

動かせないコードが溜まっていくのはプレッシャーです

* あとは分子と分母をくっつけて有理数にする方法があればよい

## Pairs

* このデータ抽象を具体的に実装するには`cons`という手続きで「ペア」を作る
* 「ペア」から要素を取り出すには`car`と`cdr`を使う

やっと出ました

* ペアを使うとあらゆる種類の複雑なデータ構造を表すことができる
* ペアから作られたデータオブジェクトを「リスト構造データ」と呼ぶ

## Representing rational numbers

ペアを素直に使って有理数を表します
手習いではペアはなかったことにされてましたので対象的なアプローチですね

```
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
```

これで動かせるようになりました
ひと安心

さてこれは説明の都合だったのか、こういう順に書きなさいということだったのか？

* この実装では約分が行われていない
* `make-rat`を変えるだけで約分することができる

```
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
```

* `make-rat`の内部を変更しても、`add-rat`などの修正は不要

上で言っている言い方を使えば
`add-rat`などが約分について不要な仮定をしてなかったから、ということになりますかね

### Exercise 2.1.

* 正負の有理数を扱う`make-rat`を書け
* 正なら分母・分子とも正、負なら分子のみ負となるように正規化せよ

分母と分子がそれぞれ正負で4通りの場合分けになるかと思いましたが
よく考えたら分母が負の時だけ分母分子の符号を変えてやるだけでいいですね
`gcd`には絶対値を渡してあげましょうか

```
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))
```

もちろん`add-rat`などに修正は不要です

## 2.1.2 Abstraction Barriers

ここ大事な感じ！
※個人の感想です

* 我々はcons、car、cdrを使ってmake-rat、number、denomを使って有理数を定義した
* さらにmake-rat、number、denomだけを使ってadd-rat等の有理数の演算と比較を定義した
* 有理数を扱うプログラムはコンストラクタmake-ratとadd-rat等だけを使えば実装できる
* このようにして、有理数を扱う「パッケージ」を作ることができた
* 有理数を扱うプログラムは有理数パッケージの「公開手続き」だけを使って有理数を扱うことができ、make-ratやadd-rat等がどう実装されているか知る必要はない

つまり

* あるタイプのデータオブジェクトについて、あらゆる操作を表現できるような基本操作の組を探しだし
* そのタイプのデータオブジェクトを扱うときは、その基本操作だけを使うようにする
* つまり、手続きがインタフェースとなり、抽象されたデータオブジェクトを扱うプログラムと、そのデータオブジェクトを抽象する（実装する）プログラムの間に壁を作ることができる

これにより

* プログラムを複数のレベルに分割することができて、メンテナンスが容易になる
* プログラムの他の部分を修正することなくデータオブジェクトの表現方法を後で変更できる
* 設計時に表現方法を決定できない場合は先送りして他の部分の開発を進めることも容易

### Exercise 2.2. 

* 始点と終点のペアにより平面上の線分を表現せよ

わりあい親切に指示されているので指示されたとおりに書くだけです

```
(define (make-segment sp ep) (cons sp ep))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
```

他の言語のコンストラクタでは名詞を使うことが多いですけど
ここではmake-と動詞をつけてますね
`(make-segment (make-point 2 3) (make-point 4 5))`と書くより
`(segment (point 2 3) (point 4 5))`の方が自然な気がしなくもないですけど
手続きだと思えば不自然ではないってことかなあ

make-segmentはsegmentを作るって意味ですけど
start-segmentはstartのsegmentって意味ではないですね
start-pointの方が意味的には自然な気がしますけど
-segmentは型を表してるような感じ？

* 線分の中点を求める手続きを定義せよ

始点と終点の座標の平均を取ればいいので

```
(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))
```

### Exercise 2.3.

* 長方形の表現を実装せよ(2.2の結果を使いたくなるかもしれない)

これ、書いてないけど辺がx軸・y軸と平行な長方形ってことでいいのかな
傾いてもいいことにするとちょっと大変そう

2.2.の結果を使いたくなるかも、っていうのは使ったほうがいいってことなんだろうなあ
対角線上の2点で長方形を表すことにしようか
いや、segmentが対角線ってことにする手もある
でもちょっとそれは不自然かな
pointをふたつ覚えておくくらいが妥当？

とりあえずコンストラクタを作ります
なんか早くもセンスを問われている気分
pointで渡してもらうのかx1、y1、x2、y2を渡してもらうのか
pointで覚えておくのか、x1、y1、x2、y2で覚えておくのか
x1>x2、y1>y2のときは順番を変えて覚えておくか

まずは最もシンプルに書くならこうでしょうか

```
(define (make-rectangle p1 p2) (cons p1 p2))
(define (p1-rectangle r) (car r))
(define (p2-rectangle r) (cdr r))
```

* 周の長さと面積を求める手続きを書け

こうなります

```
(define (perimeter-rectangle r)
  (* 2 (+ (abs (- (x-point (p1-rectangle r)) (x-point (p2-rectangle r))))
          (abs (- (y-point (p1-rectangle r)) (y-point (p2-rectangle r)))))))
(define (area-rectangle r)
  (* (abs (- (x-point (p1-rectangle r)) (x-point (p2-rectangle r))))
     (abs (- (y-point (p1-rectangle r)) (y-point (p2-rectangle r))))))
```

ちょっと面倒
absを呼び出し側にやらせるというのはどうでしょうか
x1>x2、y1>y2のときは順番を変えて覚えておくように、
つまりp1が左下、p2が右上の点を指すようにしてみます

```
(define (make-rectangle P1 P2)
  (cons (make-point (min (x-point P1) (x-point P2))
                    (min (y-point P1) (y-point P2)))
        (make-point (max (x-point P1) (x-point P2))
                    (max (y-point P1) (y-point P2)))))
```

そうするとperimeter-rectangleとarea-rectangleではabsが不要になります

```
(define (perimeter-rectangle r)
  (* 2 (+ (- (x-point (p2-rectangle r)) (x-point (p1-rectangle r)))
          (- (y-point (p2-rectangle r)) (y-point (p1-rectangle r))))))
(define (area-rectangle r)
  (* (- (x-point (p2-rectangle r)) (x-point (p1-rectangle r)))
     (- (y-point (p2-rectangle r)) (y-point (p1-rectangle r)))))
```

同じことですがp1は左下、p2は右上と約束を決める手もあります
これならコンストラクタもセレクタもシンプルにしたまま使い方もシンプルになります
assertとかつけると同じことかもしれませんが

rectangle側が簡単になる代わりに呼ぶ側が引数に注意しないといけなくなります
呼び出し側の立場からするとx1>x2ならx1とx2を入れ替えるというのは毎回やるべきことだから
rectangle側で済ませてほしいところです

一方
make-rectangleしたときのp1、p2と
p1-rectangle、p2-rectangleで取得したp1、p2が異なるというのは
いかがなものかという気もします

呼び出し側がx-pointとかy-pointを使わないといけないというのもどうでしょうか
p1、p2はそのまま覚えておくとして、こういうセレクタを使うことにしたら？

```
(define (x1-rectangle r)
  (min (x-point (car r)) (x-point (cdr r))))
(define (x2-rectangle r)
  (max (x-point (car r)) (x-point (cdr r))))
(define (y1-rectangle r)
  (min (y-point (car r)) (y-point (cdr r))))
(define (y2-rectangle r)
  (max (y-point (car r)) (y-point (cdr r))))

(define (max a b) (if (> a b) a b))
(define (min a b) (if (< a b) a b))
```

そうすると周と面積はこうなります

```
(define (perimeter-rectangle r)
  (* 2 (+ (- (x2-rectangle r) (x1-rectangle r))
          (- (y2-rectangle r) (y1-rectangle r)))))

(define (area-rectangle r)
  (* (- (x2-rectangle r) (x1-rectangle r))
     (- (y2-rectangle r) (y1-rectangle r))))
```

呼び出す側からするとこれでかなりすっきりしました
辺の長さを出す手続きを作ればもっとすっきりするかな

でもこれでは半端かもしれません
引数はpointならpointで、x1、x2、y1、y2ならx1、x2、y1、y2で押し通すのがスジ？
x1、x2、y1、y2で押し通すならこうなります

```
(define (make-rectangle x1 x2 y1 y2)
  (cons (make-point (min x1 x2) (min y1 y2))
        (make-point (max x1 x2) (max y1 y2))))
```

内部ではpointを使っています
consとcar、cdrだけで書いてもすぐに書けますが
もしかするとpointがとても便利な手続きを実装しているかもしれませんし
ポジティブに捉えれば、内部ではpointを使って表現しているけれども
外部からはその表現を隠している、とも言えそうです

今ひとつ決め手にかけますが
見た目もすっきりしましたしこれを回答とします

* 異なる表現を実装せよ
* 辺の長さと面積を求める手続きはそのまま使えるか？

すでにいろいろ試しました
インタフェースまで変わるとそのままでは使えなくなりますが
内部だけで表現を変えるならそのまま使えます

x1、x2、y1、y2の順番を先に変えておいたり

```
(define (make-rectangle p1 p2)
  (cons (make-point (min (x-point p1) (x-point p2))
                    (min (y-point p1) (y-point p2)))
        (make-point (max (x-point p1) (x-point p2))
                    (max (y-point p1) (y-point p2)))))

(define (x1-rectangle r) (x-point (car r)))
(define (x2-rectangle r) (x-point (cdr r)))
(define (y1-rectangle r) (y-point (car r)))
(define (y2-rectangle r) (y-point (cdr r)))
```

pointを使わずに書いてみたり（実質同じですが）

```
(define (make-rectangle x1 x2 y1 y2)
  (cons (cons (min x1 x2) (min y1 y2))
        (cons (max x1 x2) (max y1 y2))))
```

インタフェースが定まっていれば実装は後で考えることができると言っても
やってみるとこんな簡単な例でさえインタフェースを決めるのがなかなか大変

上で挙げたいろいろの他にも
make-rectangle-from-pointとmake-rectangle-from-xyとかして
両方のインタフェースを持っておいてもいいかもしれない
いやそれは「最小の仮定」に反するかも知れない
などといろいろ悩んで結論なし

指針がないよりはずっと考えやすくはなってるんでしょうが実践はそう甘くはない感じ
