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

## 2.2 Hierarchical Data and the Closure Property

* ペアは複合データオブジェクトを作るための糊である
* ペアを図示するときには「箱矢印表記」を使う
* オブジェクトは箱を指す矢印で表される
* プリミティブなデータは箱に入る
* ペアはcarとcdrのふたつの箱で表される

箱じゃなくて矢印がオブジェクトなんだ
そう思って図を見ると見方が変わるかも？

* consは数だけではなくペアも組み合わせることができる
* ペアを使ってあらゆるデータ構造を表すことができる
* このように、ある操作で要素を組み合わせた結果をまた同じ操作で組み合わせられるとき、その操作は'closure property'を持つと言う
* closureによって階層構造を表すことができる
* 手続きの階層構造もclosureである

このclosureはインタプリタとかで出てきたclosureと同じものなんでしょうか
一見同じには見えませんが深いところでは同じものかも

closureってもともと数学の言葉から来ていると思うんですが
そのつながりがよくわかってません
というか数学で言う閉包自体ピンときてません
そのへんわかるとなにかがすっきりしそうな気がするんですけど
いや、数学以前に普通の英語としてのclosureがわかったほうがいいのかな
・・・
閉店とか閉鎖とか終了とか
あんまり参考になりませんでした

## 2.2.1 Representing Sequences

* ペアを使ってsequence(順序のついたデータの集まり)を作ることができる
* consで書くと`(cons 1 (cons 2 (cons 3 (cons 4 nil))))`という形

箱矢印の図は省略
このへんでもどうぞ http://sicp.iijlab.net/fulltext/fig204.png

* これをリストと呼び、`(list 1 2 3 4)`とも書けるようになっている
* nilは要素のないリストとも読める

## List operations

* リストを扱うには、順番にcdrしていくのが普通
* リストを最後までcdrしたかどうかを判断するためにはnull?が使える
* cdrしながら、値となるリストをconsしていくこともある

このへんはさんざんやりました

ふたつのリストをつなぐappendは再帰的プロセス「でも」書けるよ、と言ってコードを載せておきつつ
繰り返しプロセスのコードを載せてないのは普通にやると逆順になるから？

### Exercise 2.17.

* リストの最後の要素のみを含むリストを返す手続きを定義せよ

空リストは渡されないと思ってていいのかな
手習い流だとそうなるけど

```
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))
```

ていねいにやるとこうかな

```
(define (last-pair l)
  (let ((m (cdr l)))
    (if (null? m)
        l
        (last-pair m))))
```

### Exercise 2.18. 

* リストを逆順にする手続きを定義せよ

さっきとは反対で、繰り返しプロセスならすんなり書ける・・・

いやまてよ
空リストってソース上どう書くことになってるんだ？
`(quote ())`とか`'()`とかの書き方ってまだ出てきてないよな
`nil`って書けるのかと思ったけどRacketでは定義されてないみたいでエラーだ
Schemeでは`nil`って書けたんだったかな？
`(list)`が空リストになるけどなんか変な感じ
`(quote ())`でいこう

```
(define (reverse l)
  (define (iter l ans)
    (if (null? l)
        ans
        (iter (cdr l) (cons (car l) ans))))
  (iter l (quote ()))
```

再帰プロセスで書くと力技感いっぱいなやり方しか思いつきません
なにもいいところがありません

```
(define (reverse-r l)
  (if (null? l)
      l
      (append (reverse-r (cdr l))
              (cons (car l) (quote ())))))
```

appendを繰り返しプロセスで書くのもやっぱり力技しか思いつかず
ふつうにやると逆順になるのであらかじめ逆順にしておきます

```
(define (append-i list1 list2)
  (define (iter l ans)
    (if (null? l)
        ans
        (iter (cdr l) (cons (car l) ans))))
  (iter (reverse list1) list2))
```

こっちはスタックを消費しないという意味がないでもないです
reverseを作らせたのもたぶんそういうつながり？


