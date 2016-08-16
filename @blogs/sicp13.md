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


