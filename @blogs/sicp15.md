# SICPを読む(15)

### Excercise 2.19.

* 1.2.2.の両替の仕方を数えるプログラムで、用いる硬貨を変更できるようにせよ

あのプログラムは関数とcondで効果の種類を表現してましたので
用いる硬貨の種類を変えるにはプログラムを修正する必要がありました
今はリストが使えるのでプログラムを変更することなく変更できます

本体はテキストに書いてあります

```
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))
```

1.2.2.のコードと比較してみると

* `kinds-of-coins`が`coin-values`に
* `(= kinds-of-coins 0)`が`(no-more? coin-values)`に
* `(- kinds-of-coins 1)`が`(except-first-denomination coin-values)`に
* `(first-denomination kinds-of-coins)`が`(first-denomination coin-values)`に

なってます

`con-values`は`(50 25 10 5 1)`のようなリストになってることを考えると

* no-more?はnull?で
* except-first-denominationはcdrで
* first-denominationはcarで

書けることが予想されます

```
(define (no-more? coin-values)
  (null? coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (first-denomination coin-values)
  (car coin-values))
```

これで

```
> (define us-coins (list 50 25 10 5 1))
> (cc 100 us-coins)
292
```

動きます
考えてみると

```
(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)
```

でも動きますね
でも学校で習ってません！（たぶん）

* coin-valuesの順番はccの値に影響するか？

しません

```
> (cc 100 '(10 5 50 1 25))
292
```

* それはなぜか？

総当りだから

で答えになってるかなあ
特にプログラム中でもリストの順番は仮定してないですしね
自分で数えるなら大きい方から数えるし大きい方から並んでる方が自然な気はしますけど

計算の回数は大きい方から並んでたほうが少なくて済むんじゃないかな？
こんな感じにしておいて

```
(define cc-count 0)
(define (cc amount coin-values)
  (set! cc-count (+ cc-count 1))
  (cond ((= amount 0) 1)
  ...
```

やってみる

```
> (set! cc-count 0)
> (cc 100 us-coins)
292
> cc-count
15499
> (set! cc-count 0)
> (cc 100 (reverse us-coins))
292
> cc-count
38901
```

大きい方から並べておいたほうが楽なようです

### Exercise 2.20

* dotted-tail記法を使うと、任意個数の引数を受け取ることができる
* `(define (f x y . z) <body>)`と定義して`(f 1 2 3 4 5 6)`と呼べば、fの中ではxが1、yが2、zが`(3 4 5 6)`となる
* `(define (g . z))`と定義して`(g 1 2 3 4 5 6)`と呼べば、gの中ではzが`(1 2 3 4 5 6)`となる

ほほう

```
> (define (f x y . z) (list x y z))
> (f 1 2 3 4 5 6)
'(1 2 (3 4 5 6))
> (define (g . z) z)
> (g 1 2 3 4 5 6)
'(1 2 3 4 5 6)
```

ほんとだ
lambda使って書くやり方でもできるのかな

```
> (define f-l (lambda (x y . z) (list x y z)))
> (f-l 1 2 3 4 5 6)
'(1 2 (3 4 5 6))
> (define g-l (lambda (. z) z))
read: illegal use of `.'
```

gのほうはダメな気がしてました
でもこんなところでlambdaに制限があるなんて腑に落ちない
これならどうかな

```
> (define g-l (lambda z z))
> (g-l 1 2 3 4 5 6)
'(1 2 3 4 5 6)
```

ヤマカン大当たり
というかヤマカンで当たるような仕様にしてくれてるってことですね
もしかしてRacket独自の仕様だったりしないか確かめておきます

R5RS http://www.schemers.org/Documents/Standards/R5RS/HTML/ の
4.1.4 Proceduresより

> (lambda \<formals\> \<body\>)
> Syntax: \<Formals\> should be a formal arguments list as described below, ...
> (\<variable1\> ...): The procedure takes a fixed number of arguments; ... 
> \<variable\>: The procedure takes any number of arguments; ...

OK

と思ったらちゃんと注に書いてあったよ（涙

* １個以上の引数を受け取り、ひとつ目の引数と偶奇が等しい数のリストを返す手続きを書け

楽勝！
と思ってこう書いたらエラー

```
(define (same-parity a . z)
  (cond ((null? z) (quote ())) 
        ((or (and (even? a) (even? (car z)))
            (and (odd? a) (odd? (car z))))
         (cons (car z) (same-parity a (cdr z))))
        (else (same-parity a (cdr z)))))
```

そうか
`(same-parity a (cdr z))`とは呼び出せないんだな
うーん

リストで受け付ける関数を作るしかないのかな

```
(define (same-parity a . z)
  (define (sp z)
    (cond ((null? z) (quote ())) 
          ((or (and (even? a) (even? (car z)))
               (and (odd? a) (odd? (car z))))
           (cons (car z) (sp (cdr z))))
          (else (sp (cdr z)))))
  (sp z))
```

偶奇の判定は`(= (reminder a 2) (reminder (car z) 2)`のほうが短くて済みますね
それに`(reminder a 2)`の値は変わらないから

```
(define (same-parity a . z)
  (let ((ra (remainder a 2)))
    (define (sp z)
      (cond ((null? z) (quote ())) 
            ((= ra (remainder (car z) 2))
             (cons (car z) (sp (cdr z))))
            (else (sp (cdr z)))))
    (sp z)))
```

まだ何やらの戒律が残ってる気もするけどこれくらいでいいや
ていうか人間には修正前のやつのほうがわかりやすい気がする
コンパイラがなんとかしてくれるかな？

## Mapping over lists

* リストの各要素に何らかの変換を施したリストを返す高階関数mapがとても便利

mapの単純な実装

```
(define (map proc items)
  (if (null? items)
      (quote ())
      (cons (proc (car items)) (map proc (cdr items)))))
```

mapを使うとたとえばリストの各要素を定数倍するこの手続きが

```
(define (scale-list items factor)
  (if (null? items)
      (quote ())
      (cons (* (car items) factor)
               (scale-list (cdr items) factor))))
```

こうなる

```
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
```

これは単に短く書けるようになったというだけではなくて

* リストを扱うための一段階上の抽象化を行っている
* mapにより、リストの要素をひとつひとつ処理する方法を気にする必要がなくなった
* コンピュータの処理方法が変わったのではなく、我々の考え方が変わっている
* mapはひとつの抽象化の壁を作っているといえる

### Exercise 2.21.

* リストの各要素を２乗する手続きを２通り書け

```
(define (square-list items)
  (if (null? items)
      (quote ())
      (cons (square (car items))
            (square-list (cdr items)))))
```

と

```
(define (square-list items) (map square items))
```

### Exercise 2.22.

* square-listを繰り返しプロセスにしようとして以下のように書き換えるとリストが逆順になってしまう

```
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons (square (car things)) answer))))
  (iter items (quote ())))
```

* なぜか？

リストの右にあるものほど後でconsされますがconsされるとリストの左にくっついてしまうので

* こうしてもうまくいかない

```
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons answer (square (car things))))))
  (iter items (quote ())))
```

* なぜか

どうなるかというと

```
> (square-list (list 1 2 3 4))
'((((() . 1) . 4) . 9) . 16)
```

最初のコードでは右にある要素が左にくっついてしまうということで左右を入れ替えてみましたが
`(cons (quote ()) 1)`は `(1)`にはなってくれず`(() . 1)`になってしまいます

前にもちょっとやりましたけど繰り返しプロセスでリストの右側にアトムをくっつける
うまい書き方ってあるんですかね
逆順で作っておいてreverseくらいがいいトコなんでしょうか

### Exercise 2.23.

* mapに似ているけれども、結果をリストにするのではなく、単に手続きを適用するだけの手続きを書け
* 手続きの値はなんでも可

こうかな

```
(define (for-each proc items)
  (if (null? items)
      #t
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))
```

値を使わないで別のことをするので、ifにふたつの関数を並べて書かないといけません
まだ出てきてないと思いますがbeginを使いました
letとかでも実現できますが

ところでテキストに出てくる例はこうなってます

```
(for-each (lambda (x) (newline) (display x))          (list 57 321 88))57
321
88```

でも実際やるとこう

```
> (for-each (lambda (x) (newline) (display x)) (list 57 321 88))

57
321
88#t
> 
```

あんまりキレイとは言えない出力結果ですね
どうしてこういう例じゃないのか

```
> (for-each (lambda (x) (display x) (newline)) (list 57 321 88))
57
321
88
#t
> 
```

他のLisp本でもnewlineが先に来てる例を見たことがある気がします
そのほうが見やすくなる処理系の方がメジャーなんでしょうか
プロンプトに式を入力してEnterしても画面上は改行されず、
値を表示する前に改行するようなREPLを持つ処理系ってことになると思うんですが
なんか不自然な気がするのは慣れてないからですかね

ところでifとbeginの組み合わせはなんとなく美しくない気がします
（そもそも値を使わないのが美しくないのかもしれない）
condは複数の関数を並べて書けるのでbeginが不要です
条件をひっくり返せば#tを返すっていう意味のない部分も書かなくて済みます

```
(define (for-each proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each proc (cdr items)))))
```

この例ではcondの方がずっとすっきりして見えますね
ただ、同じ構造を書くのに時にはif、時にはcondで書くというのも美しくないといえば美しくない
単純に真偽で別れるだけならifの方がいい気がする
ifが`(if ... (then ...) (else ...))`みたいな構文だったら複数書けただろうけど
それはそれでいさぎよくないし

