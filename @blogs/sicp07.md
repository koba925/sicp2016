# SICPを読む(7)

### Exercise 1.25.

* `(define (expmod base exp m) (remainder (fast-expt base exp) m))`ではなぜよくないのか

数学的にはまったく何の問題もないです
実行のオーダもどちらもΘ(log n)で同じ
細かく見るとremainderの実行回数はこちらのexpmodでは1回だけなのに対し
オリジナルのexpmodではΘ(log n)で増えていきます
それだけを見ればこっちの方がいいくらい

ただしコンピュータで実行する場合は数字が大きくなりすぎるとよろしくないです
たとえば100001が素数かどうかを確かめようとして
99999^100001 mod 100001を計算しようとすると
fast-expの値はだいたい50万桁の数になります
実行回数はΘ(log n)とは言え
50万桁のかけ算を実行するにはかなり時間がかかることは想像に固くありません

一方、オリジナルのexpmodは毎回remainderを取る必要がありますが
おかげで値が大きくなることはありません

どれくらい違うのかというと程度問題ですので実際やってみます

```
> (let ((s (current-inexact-milliseconds)))
    (expmod 99999 100001 7)
    (- (current-inexact-milliseconds) s))
0.00390625
> (let ((s (current-inexact-milliseconds)))
    (expmod-ex1-25 100000 100000 7)
    (- (current-inexact-milliseconds) s))
22.5341796875
```  

4桁ほど違います

ところで実行時間を計る関数がほしいですね
でも

```
(define (exp-time f)
  (let ((s (current-inexact-milliseconds)))
    f
    (- (current-inexact-milliseconds) s)))
```

ではうまくいかないんですよね

```
> (exp-time (expmod-ex1-25 99999 100001 7))
0.0
```

`exec-time`を呼ぶ前に`(exec-time (expmod-ex1-25 99999 100001 7))`を
評価してしまいますから
時間を図りたい式をいったんlambdaでくるむかな

こうして

```
(define (exp-time f)
  (let ((s (current-inexact-milliseconds)))
    (f)
    (- (current-inexact-milliseconds) s)))
```

こう

```
> (exp-time (lambda () (expmod-ex1-25 99999 100001 7)))
39.64501953125
```

うまくいった模様です
たぶんマクロで書くのが筋だと思われますが詳しくないので放置
ライブラリに定義されてそうな気はするんですけどすぐには見つかりませんでした


