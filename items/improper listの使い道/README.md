終端が `[]` で終わってないリストを improper list（不適切なリスト）と呼びます。

この improper list、こうやって説明されているドキュメントは沢山あるものの、一体こんなリストをどういう用途で使うのかが分からなかったので調べてみました。

## 標準ライブラリ

標準ライブラリの中を "improper" で検索し、データ構造として improper list を使ってそうな部分を調べてみたところ、１箇所だけ見つけました。[^1]

[^1]: `iolist_to_binary/1`や`list_to_binary/1`は improper list を受け取りますが、improper list のデータ構造を作っている訳ではないので省いてます。

```erlang:rand.erl
-type exsplus_state() :: nonempty_improper_list(uint58(), uint58()).

%% Advance xorshift116+ state for one step and generate 58bit unsigned integer
-spec exsplus_next(exsplus_state()) -> {uint58(), exsplus_state()}.
exsplus_next([S1|S0]) ->
    %% Note: members s0 and s1 are swapped here
    S11 = (S1 bxor (S1 bsl 24)) band ?UINT58MASK,
    S12 = S11 bxor S0 bxor (S11 bsr 11) bxor (S0 bsr 41),
    {(S0 + S12) band ?UINT58MASK, [S0|S12]}.
```

乱数アルゴリズム Xorshift116+ の実装のようです。
`exsplus_state/0`型が improper list になっています。
ただ、これって単純にタプル型でいいのでは？という気もします。

## データ構造を見る

タプルで良さそうな部分を improper list にするということは、メモリや速度上、きっと タプルよりもimproper list の方が効率がいいに違いないと思って、実装を見てみました。

### メモリ使用量

58bitの整数を improper list に格納すると、以下の様なデータ構造になるようです。[^2]

[^2]: 64bit環境でのデータ構造です。32bitの場合は上位32bitは使えません。

```
| pointer (62bit)     | 0 | 1 |   --->  | small value (60bit)      | 1 | 1 | 1 | 1 |
                                        | small value (60bit)      | 1 | 1 | 1 | 1 |
```

合計3ワードを使っています。
ルートの値は引き回す時に使う値なので、アロケーションしているのは２ワードだけです。

同様に58bitの整数を２要素のタプルに格納すると、以下の様なデータ構造になるようです。

```
| pointer (62bit)     | 1 | 0 |   --->  | arityval (58bit) | 0 | 0 | 0 | 0 | 0 | 0 |
                                        | small value (60bit)      | 1 | 1 | 1 | 1 |
                                        | small value (60bit)      | 1 | 1 | 1 | 1 |
```

合計4ワードを使っています。
ルートの値は引き回す時に使う値なので、アロケーションしているのは３ワードだけです。
タプルには先頭に要素数を含んでいるため、タプルより improper list の方が1ワード分少なくて済んでいます。

つまり **メモリ効率はタプルより improper list の方が良い** ということになります。

これは `erts_debug:size/1` を使っても同じ結果が得られます。

```erlang
1> erts_debug:size([1|2]).
2
2> erts_debug:size({1,2}).
3
```

### 処理速度

次は、処理速度を見てみましょう。
このデータ構造の下位2ビットはプライマリタグと呼ばれていて、以下の意味を持ちます。

- 0 0 - ヘッダ（header）
- 0 1 - リスト（list）
- 1 0 - ボックス化済み（boxed）
- 1 1 - 即値（immediate）

プライマリタグにリストがあるので、リストの場合は以下の処理を書くだけでリストかどうかを判別できます。

- 値の下位２ビットを取得し、0 1 であればリスト

タプルの場合、下位2ビットが 1 0 (0x2) になっています。これは「ボックス化済み」という意味で、残りの62ビットがポインタになっているという意味です。
詳細な意味はデリファレンスしてみないと分かりません。

これをデリファレンスした後、同様に下位2ビットを読みます。0 0 だった場合はヘッダという意味になり、この場合はヘッダ用のタグを更に4ビット持っています。
この4ビットに応じて値の意味が変わり、0 0 0 0 だった場合は「タプルの要素数」という意味になります。
この値はタプルかどうかを判別するためのタグにもなっているので、これを利用してタプルかどうかを判断します。

そのため、タプルかどうかを調べるためには以下の処理を書く必要があります。

- 値の下位２ビットを取得し、1 0 であるかを確認する
- ポインタをデリファレンスする
- その値の下位2ビットを取得し、0 0 であるかを確認する
- さらに4ビットを読み、0 0 0 0 であればタプル

[^3]: 単にタプルかどうかを判別するだけなら、最後の２つの手順については「下位6ビットを取得し、0 0 0 0 0 0 であればタプル」で可能なので、実際は３つの手順で出来ます。

このような手順を踏むため、少なくともリストかタプルかを判別する処理においては、**タプルよりリストの方が早い** ということになります。[^3]

標準ライブラリの `exsplus_state/0` 型が improper list で保存しているのは、メモリ使用量が少なく、高速に処理できるからだと考えられます。

なお、このデータ構造の各ビット意味に関しては [Erlang's Internal Data Representation - Detail oriented](https://blog.edfine.io/blog/2016/06/28/erlang-data-representation/) の画像がすごく分かりやすかったので、内部実装が気になる場合は見てみるといいかもしれません。

## まとめ

- ２要素を格納する場合、タプルより improper list の方がメモリ使用量が少ない
- 速度的にも２要素のタプルよりimproper listの方が早い

ただし、あるリストが improper list であることは、ユーザはあまり想定しない事態でしょう。
そのため、この構造はモジュール内で閉じ、外に公開しないようにしておいた方がいいと思います。

また、メモリ使用量が少ないとはいえ、各要素の値が10000バイトのバイナリだった場合なんかは、1ワードを省略したところで大した効果は見込めません。
速度的にも、よっぽど参照するコードを書かなければ、ここを改善した所でそこまで大きな効果は無いでしょう。

そのため、improper listを使うのは、以下の条件を満たした場合だけで良さそうです。

- 本当にカツカツまで高速化、メモリ使用量を減らしたい要求がある
- そのデータ構造は外に公開するものではない

## 参考

- [How is the Erlang list represented in memory?](https://www.quora.com/How-is-the-Erlang-list-represented-in-memory)
- [10.1  Memory](http://erlang.org/doc/efficiency_guide/advanced.html#id70479)
- [otp/erl_term.h at OTP-19.1.6 · erlang/otp](https://github.com/erlang/otp/blob/OTP-19.1.6/erts/emulator/beam/erl_term.h)
- [Practical use of improper lists in Erlang (perhaps all functional languages) - Stack Overflow](http://stackoverflow.com/questions/5088575/practical-use-of-improper-lists-in-erlang-perhaps-all-functional-languages)
