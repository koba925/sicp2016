# SICPを読む(1)
というわけでSICPを読んでみようかと思います
Lambda the Ultimateシリーズはまたそのうちに

どんな本かっていうと以前書いたような気がするので割愛
最後までちゃんと読めたら自己満足に浸れるという期待で

今回は英語で読んでみようかなと思ってます それほど意味はありません
Structure and Interpretation of Computer Programs
by Harold Abelson and Gerald Jay Sussan with Julie Sussman
https://mitpress.mit.edu/sicp/full-text/book/book.html

今日本語で読むならこれかなあ 少々物議を醸してますが
https://github.com/hiroshi-manabe/sicp-pdf
ですます調が少々体質に合わないので困ったときのガイド程度に
紙の本も持ってるんですけど第１版だし

## Foreword

挫折するたびに最初から読むのでこのへんはおなじみ
飛ばせばいいじゃないかという話もありますがまえがきあとがきが好きなんです
読み終わってあとがきがないとちょっとがっかりしたりします

英語的にはたぶんこのForewordが一番難易度が高いんじゃないでしょうか
格調高いというかなんというか
そんなニュアンスがわかるわけじゃないんですけど長くて見慣れない単語がいっぱいでてきたり
本文はそこまでじゃありません
Forewordだけちょっと異質

Forewordを書いたAlan J. Perlisさんはうまいこと言うのがお好きなようで
Epigrams on Programming http://www-pu.informatik.uni-tuebingen.de/users/klaeren/epigrams.html
というのもちょっと有名な模様

## Preface to the Second Edition

> This edition emphasizes several new themes. The most important of these is the central role played by different approaches to dealing with time in computational models: objects with state, concurrent programming, functional programming, lazy evaluation, and nondeterministic programming. We have included new sections on concurrency and nondeterminism, and we have tried to integrate this theme throughout the book.

* 今回の改訂のもっとも重要なテーマは時間の取り扱い
* 状態を持つオブジェクトとか並列プログラミングとか関数型プログラミングとか
* 遅延評価とか非決定プログラミングとか

このへんに注目したい

## Preface to the First Edition

> First, we want to establish the idea that a computer language is not just a way of getting a computer to perform operations but rather that it is a novel formal medium for expressing ideas about methodology. Thus, programs must be written for people to read, and only incidentally for machines to execute. 

* プログラミング言語はコンピュータを操作するためだけのものじゃない
* 方法や手順を表現するための新しいメディアでもある
* プログラムは人間が読むために書かれるものであって、たまたま機械で実行されるだけ

ここんとこ小学校でプログラミングを教えるとかでいろいろ議論されてますが
先生方にはぜひこういう視点をもって教育していただきたいなと思う次第です
小学校の先生もいろいろやらされて大変だと思いますががんばってください

> They should feel secure about modifying a program, retaining the spirit and style of the original author.

* (この本を読んだ人は)自信を持ってプログラムを修正することができる
* しかも、元のコードを書いた人の考えやスタイルを保って修正できる

すばらしい

> These skills are by no means unique to computer programming.

* こういうスキルはコンピュータのプログラミングに限られたものではない

家に帰ったら手を洗って荷物を片付けて宿題して明日の準備をして
おやつがなかったらお母さんにおねだりする、みたいなのもプログラムですよねー

> Underlying our approach to this subject is our conviction that "computer science" is not a science and that its significance has little to do with computers. The computer revolution is a revolution in the way we think and in the way we express what we think. 

* 計算機科学は科学ではないし、コンピュータともほとんど関係ない
* コンピュータによって、私たちの考え方や考えの表現方法に革命が起こった

> Mathematics provides a framework for dealing precisely with notions of "what is." Computation provides a framework for dealing precisely with notions of "how to."

* 数学は「それは何か」を正確に表現するための枠組みを提供する
* プログラミングは「どうやるか」を正確に表現するための枠組みを提供する

数学は宣言的でプログラミングは手続き型だと言ってますが
中では手続き型・関数型・宣言型いろいろ登場します

いいことが書いてあると思うので先生方もPreface to the First Editionだけは
読んでいただきたい（半分は本気）

## Acknowledgements

知ってる名前だけ挙げてみる

* Dan Friedman (Scheme手習い・Scheme修行の著者)
* Guy Steele Jr. (SussmanといっしょにSchemeを開発した人)
* Marvin Minsky (人工知能の父)
* Seymour Papert (LOGOとかLEGO MINDSTORMを作った人)
* Richard Stallman (EmacsとかGNUの人)

貢献の形や程度は様々だと思いますがさすがのそうそうたるメンバー

## Chapter 1 Building Abstractions with Procedures

> Master software engineers have the ability to organize programs so that they can be reasonably sure that the resulting processes will perform the tasks intended. They can visualize the behavior of their systems in advance. 

* 熟練したソフトウェアエンジニアは、プログラムが意図通りに実行されることを確信できるようにプログラムを構成することができる
* 彼らは、システムのふるまいを前もってイメージすることができるのだ

イメージできないと動かしてみたら動きましためでたしめでたしみたいになりますね
そりゃ俺か
(Y Y)もイメージできますか（無理

> Well-designed computational systems, like well-designed automobiles or nuclear reactors, are designed in a modular manner, so that the parts can be constructed, replaced, and debugged separately.

* システムをうまく分割して設計すれば部分ごとに作ったり置き換えたりすることができる

こういう記事がありました

PROGRAMMING BY POKING: WHY MIT STOPPED TEACHING SICP
http://www.posteriorscience.net/?p=206

* SICPでは、シンプルでよく理解されたパーツを組み合わせることによるプログラミングを扱った

たぶんこれが、「うまく分割して設計し」「システムのふるまいを前もってイメージする」やり方
一方

* いまどきのプログラミングはそうじゃない
* "More like science"だ
* 理解しきれないフレームワークやライブラリを使ってみてはうまくいくやり方を探している

わからないものを観察して調べる、という意味でscienceっぽい、てことでしょうか
めでたしめでたし方式プログラミングですね
Prefaceでプログラミングは科学じゃない、って言ってるのもそういうことなんでしょう
ということは科学じゃなくて数学っぽいってことでしょうか

この本でSchemeを使う理由が説明されます

* Lispは自由だ！

（要約しすぎ）

* もっとも重要なのは、Lispのプログラムが、それ自身Lispのデータとして扱えること

## 1.1 The Elements of Programming

* プログラミング言語は単純なアイデアを組み合わせて複雑なアイデアを表現する手段を提供する
  * プリミティブ：もっとも単純なモノ
  * 複合の手段：シンプルなモノを組み合わせる
  * 抽象の手段：複合したモノに名前をつけてひとつのモノとして扱えるようにする
* モノにはデータと手続きの２種類がある（後で境目があいまいになるけど

訳しにくいと思った言葉はあっさり挫折してテキトーな言葉にしています

```
> 486
486
```

プリミティブ（数）です
数と言うか数字と言うかちょっとだけ微妙な問題

```
> (+ 137 349)486
```

プリミティブ（数と＋）を複合しました

```
> (define pi 3.14159)> (define radius 10)> (define circumference (* 2 pi radius))> circumference> 62.8318
```

プリミティブや複合式に名前をつけて抽象しました

## 1.1.3 Evaluating Combinations

複合式っていうと何やら複雑そうですが上で出てきた`(* 2 pi radius)`だって複合式
演算子(`*`)と引数(`2`、`pi`、`radius`)があれば複合式です

* 複合式を評価するには
* まず複合式の各項目を評価し

`*`と`2`と`pi`と`radius`を評価します
`*`は「かけ算」に、`2`は2に、`pi`は3.14159に、`radius`は10に評価されます

* 一番左の項目の値を、他の項目の値に適用する

「かけ算」を2と3.14159と10に適用します
つまり2\*3.14159\*10を計算するってこと

`(* (+ 1 2) 4)`みたいに複合式が入れ子になっているときも
上のルールがそのまま適用できます

1. `*`と`(+ 1 2)`と`3`を評価します
2. `*`は「かけ算」に、`4`は4に評価されます `(+ 1 2)`も評価します
3. `(+ 1 2)`は複合式なので同じルールを適用します
4. `+`は「足し算」に、`1`は1に、`2`は2に評価されます
5. 「足し算」を1と2に適用して3になります
6. `(+ 1 2)`は3に評価されました
7. 「かけ算」を3と4に適用して12になります
8. `(* (+ 1 2) 4)`は12に評価されました

式の構造が再帰的なので手続きも再帰的になるのが自然かつシンプル、て話

* `define`は複合式ではない
* `(define x 3)`は`x`と`3`に`define`を適用するのではなく
* ただ`x`を`3`に結びつけているだけ
* こういうのを特殊形式という

複合式のルールに従うと、まず`define`と`x`と`3`を評価するわけですが
`x`はただ`x`であってほしくて、評価してみたら実は`x`は5でしたーなんてことになっても困ります

`(define 'x 3)`と書くことにするなら複合式として扱えるかもしれませんが
初期のSchemeには現在のset!にあたるASETというのがありましたが、
これは(ASET 'X 3)と書くお約束で、面倒なら(ASET' X 3)とも書けるよ、って話でしたし
（でも関数じゃなくてプリミティブとして実装されてましたが）
Common Lispなどで使われる`setq`も起源をたどれば
`(SET (QUOTE xxx))`と書くのが面倒なのでQUOTE付きの`SET`ができた
という話のようです

## 1.1.4 Compound Procedures

CompoundもCombinationも複合と言いたくなってしまって多少もやっとしています
Compoundの方がCombinationよりもくっつき具合が強い印象があるんですけど
くっついてるというよりは混じってる感じ
manabe氏の訳ではCombinationを組み合わせと訳しているけどもうちょっといい言葉がありそう
英語のまま置いておこうかな

Compound Procedureの定義です
ただの関数定義

> ```
> (define (square x) (* x x))
> ```

Combinationに名前をつけたものがCompound Proceduresてとこですかね
Combinationはそれぞれ部品が見えてるけど
Compoundだと混じっちゃって見えない、みたいな感じでしょうか

あと引数がつきました
英語ではargumentとかparameterと言わずoperandと言ってます
そのへん使い分けがあるんでしょうか
日本語版にしたがってoperandは被演算子と訳しておきます

Scheme手習い式ならこうです

> ```
> (define square (lambda (x) (* x x))
> ```

こっちのほうがピュアな感じがしますけどちょっと文字数が多い

続いて英語圏の人にはわかりやすそうな説明が出てきます
日本人ハンデ大きいかなと思いました

> ```
> (define (square x)        (*        x     x))
>  ↑       ↑      ↑          ↑        ↑     ↑
>  To      square something, multiply it by itself.
> ```

日本人なら逆ポーランド記法で対抗？

## 1.1.5 The Substitution Model for Procedure Application

* 演算子がcompound procedureである複合式も評価の方法は同じ
* 演算子と被演算子をそれぞれ評価してから（演算子を評価した結果である）compound procedureを被演算子に適用する

そしてCompound procedureを被演算子に適用するには

* procedureの本体の仮引数を、対応する引数で置き換える

仮引数＝formal parameter、引数＝argumentです
ちょっとめんどい

この辺、今まではなんとなくしかわかってませんでしたが
Scheme手習いやScheme論文でカンペキに理解できたはず

* `(f 5)`を評価するにはまずfの本体を持ってくる
* `(sum-of-squares (+ a 1) (* a 2))`
* 仮引数(a)を引数(5)で置き換える
* `(sum-of-squares (+ 5 1) (* 5 2))`
* 演算子と引数を評価する

手習い式のdefineなら

```
(define f
  (lambda (a)
    (sum-of-squares (+ a 1) (* a 2))))
```

だから

```
  (f 5)
= ((lambda (a) (sum-of-squares (+ a 1) (* a 2))) 5)
= (sum-of-squares (+ 5 1) (* 5 2))
```

とすっきりしますね

`sum-of-squares`は `(+ (square x) (square y))`になり
`(+ 5 1)`は6になり（演算子がプリミティブの場合の評価はさっきやったのでできる）
`(* 5 2)`は10になります

* `(+ (square 6) (square 10))`
* `(+ (* 6 6) (* 10 10))`
* `(+ 36 100)`
* `136`

バッチリ

* これが手続き適用の置換モデル
* 当面はこれが手続き適用の「意味」と思ってていい
* ただしインタプリタが実際文字列を置換して評価しているわけじゃない

Schemeの論文ではSubstitution Semanticsのところに書いてあった話

初心者（そういう人も対象読者）にこういう話を
それもこんな初めのほうでやる必要ってあるのかと思ってましたが
考えてみるとまったく初めての人は何かモデルがないと考えようがないですね

慣れるまでは1行1行丹念に読まないとわからない、ということを
改めて認識するのにもいいかもしれません
小説を読むのとコードを読むのとは全然違うんだと
「システムのふるまいを前もってイメージ」するにはモデルに基づいて1行1行読むんだと

といってもそういういうことを意識できたのはわりと最近
もっと具体的にいうと数学ガールを読んでからです
コードじゃなくて数式だけどまあ似たようなもの
乱択アルゴリズムの巻なら実際にコードを1行1行追ってくれてるし
数学はちょっとなあ、と思う人にもおすすめです
乱択アルゴリズムの巻が読めれば他の巻も読めると思いますよ

自分はBASICをいじってる間に頭の中にモデルができていった世代
だから頭の中にあるのは完全に手続き的なモデルだと思われます
置換モデルから入った人とはプログラミングに対する考え方が根本的に違ってくるかもしれません
すごく重要な気がしてきました

## Applicative order versus normal order

* さっきは演算子と被演算子を評価してから被演算子の評価結果にに演算子の評価結果を適用してました
* でもそれがただひとつのやり方というわけではありません
* もうひとつのやりかたは、値が必要になるまで被演算子を評価しないというものです

慣れた書き方で書くとこう

```
  (f 5)
= (sum-of-squares (+ 5 1) (* 5 2))
= (+ (square (+ 5 1)) (square (* 5 2)))
= (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
```

これですべてがプリミティブにまで展開されたので演算子を被演算子に適用していきます

値が必要になる、というのとすべてがプリミティブにまで展開される、というのが
同じことなのかどうかは少しはっきりしない気もしますが
他にやることがなくなったことは間違いありません

```
= (+ (* 6 6) (* 10 10))
= (+ 36 100)
= 136
```

* (+ 5 1)や(* 5 2)が2回ずつ評価されてることに注意
* こちらのやりかたは「正規順評価」、さっきのやりかたは「適用順評価」と呼ばれる
* Lispは適用順評価を採用している
* 適用順のほうが効率がいいし、正規順では取り扱いが難しいケースが出てくるから
* でも正規順がとても便利なときもある

Haskellさんは正規順＋遅延評価（でいいんだったかな）
メジャーな言語で適用順じゃないのはそれくらい？

## 1.1.6 Conditional Expressions and Predicates

* 我々が表現できる手続きは今のところ非常に制限されている
* それは条件にしたがって異なる操作をすることができないから

正規順評価だったらよかったんですけどねー
というわけでcond、if、and、or、notの説明
not以外は演算子と被演算子を評価して、っていう動きじゃないので特殊形式です
それぞれにどういう動きをするか決められてます

### Exercise 1.1、1.2

ただの練習なので割愛

### Exercise 1.3.

* 3つの数を取り、大きな2つの数の2乗の和を返す手続きを書け

そろそろ動かしながら行きますか
SICPでちょっとやりにくいのは、ところどころで以前に定義した関数を使ってたりするところ
どこでファイルを分ければいいのかわかりづらいんですよね

```
(define (sum-of-squares-of-top-2 a b c)
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
        ((and (>= a b) (>= c b)) (sum-of-squares a c))
        (else (sum-of-squares b c))))
```

早くももっといい書き方があるかもと思わせるような問題を出すところがニクい

### Exercise 1.4.

> Observe that our model of evaluation allows for combinations whose operators are compound expressions. 

それさっきやったことじゃないの・・・？

> Use this observation to describe the behavior of the following procedure:
>
>  ```> (define (a-plus-abs-b a b)>   ((if (> b 0) + -) a b))
> ```

ああそういうことか
これまでの用語の使い方からしたら"combinations whose operators are combinations"じゃね？
演算子が複合式である複合式でもOKか、ってことでしょ？

しかしまあなんとここで早くも関数型の洗礼て感じで面白い
手続きモデル脳だとこうしか思いつけない

```
> (define (a-plus-abs-b a b)>   (if (> b 0) (+ a b) (- a b)))
```

ともあれ

```
  (a-plus-abs-b 2 -3)
; ((if (> b 0) + -) a b) ←関数本体を持ってきて
= ((if (> -3 0) + -) 2 -3) ←置き換えて
; 演算子と被演算子を評価する
; ifを評価するにはまず(> -3 0)を評価する
; 偽なので-を返す
= (- 2 -3)
= 5
```

おｋ

### Exercise 1.5.

* 次のプログラムを適用順・正規順で評価するとどうなるか

```
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))
(test 0 (p))
```

適用順だと`(test 0 (p))`を評価するにはまず`test`と`0`と`(p)`を評価する
`test`は`(if (= x 0) 0 y)`になり
`0`は`0`になり
`(p)`は`(p)`になる
`(p)`はまた評価しなければならなくてまた`(p)`になる・・・
で無限ループ

なんとなく程度しか理解してなかったり
`(define (p) (p))`の見かけの異様さに影響されたりで
以前に読んだ時はなんとなくそうなりそうだね、くらいで確信が持ててなかったけど
今は置換モデルにも慣れたしインタプリタの動作も知ってるし
今回は安心

正規順だと被演算子は必要になるまで評価しないので

`(test 0 (p))`の`test`だけを評価して`(if (= 0 0) 0 (p))`となり
`if`の評価規則にしたがい`(= 0 0)`を評価して
真なので`0`を返して終わる
以上

## 1.1.7 Example: Sqare Roots by Newton's Method

```
(define (sqrt x)
  (the y (and (>= 0)
              (= (square y) x))))
```

とは書けません
そこが数学との違い

* 数学ではモノの性質を記述しますがプログラムではどのようにやるかを記述する
* 別の言い方をすると、数学は宣言的だけれどもプログラムは手続き的

あとはニュートン法で平方根を求めます
