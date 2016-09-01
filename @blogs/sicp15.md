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


