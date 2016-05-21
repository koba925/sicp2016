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
Epigrams on Programming
http://www-pu.informatik.uni-tuebingen.de/users/klaeren/epigrams.html
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

まずはこれくらいで