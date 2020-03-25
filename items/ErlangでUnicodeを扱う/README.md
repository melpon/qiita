ErlangでUnicode文字列を扱うのは、そんなに難しくありません。

ただ、いくつかハマった点があるので、それを書いていきます。

## 文字列はコードポイントになる

```erlang
1> "あいうえお".
[12354,12356,12358,12360,12362]
```

文字の `$あ` はUnicodeのコードポイントで U+3042 になり、これは10進数で `12354` になります。

256以上の値は、`iodata()`や`iolist()`を受け取る関数に渡すとエラーになるため、注意が必要です。

なお `[12354,12356,12358,12360,12362]` という表示を日本語にしたいなら、起動オプションに `+pc unicode` を渡せばいい感じにやってくれます。

```erlang
$ erl +pc unicode
1> "あいうえお".
"あいうえお"
```

## バイナリ文字列ではエンコーディングを指定する

バイナリ文字列では、エンコーディングを指定しないと正しいバイト列になりません。

```erlang
1> <<"あいうえお">>.
<<"BDFHJ">>
2> <<"あいうえお"/utf8>>.
<<227,129,130,227,129,132,227,129,134,227,129,136,227,129,138>>
```

`<<"あいうえお">>` と書いた時、`<<12354,12356,12358,12360,12362>>`と書いたのと同じ意味になります。
特にオプションを指定しなかった場合、バイナリで256以上の値を指定しても下位8ビットの値しか使われないため、それぞれ`<<66, 68, 70, 72, 74>>`となり、上記のように`<<"BDFHJ">>`と表示されてしまいます。

このようにするメリットは多分無いはずなので、ちゃんとエンコーディングは指定しましょう。

なおUnicode関連で指定可能なエンコーディングは `utf8`, `utf16`, `utf16-little`, `utf32`, `utf32-little` の5種類です[^1]。

[^1]: `utf8-little` や `utf32-bit` 等の指定も可能だけど、`utf8`, `utf32` 等と同じ意味なので紹介しない方向で

## バイナリ文字列にはエンコーディング情報が入っていない

ある関数の引数でバイナリ文字列を受け取る時、その値にはエンコーディング情報は入っていません。
つまりそのバイナリ文字列が UTF-8 なのか UTF-16 LE なのかを知ることができません。

```erlang
%% BinStrはUnicodeのバイナリ文字列とする
-spec f(binary()) :: ok.
f(BinStr) ->
  %% UTF-8とは限らないのでこれではエラーになる可能性がある
  UniStr = unicode:characters_to_list(BinStr),
  ...
```

その関数で有効なエンコーディングをドキュメントに記述しておくか、引数にエンコーディングの情報を入れてあげましょう。

```erlang
%% BinStrはUTF-8のバイナリ文字列とする
-spec f(binary()) :: ok.
f(BinStr) ->
  UniStr = unicode:characters_to_list(BinStr),
  ...

%% BinStrはUnicode文字列をEncodingでエンコードしたバイナリ文字列とする
-spec f(binary(), Encoding) :: ok.
f(BinStr) ->
  UniStr = unicode:characters_to_list(BinStr, Encoding),
  ...
```

これはファイルからデータを読み取った際も同様で、読んだバイナリがどのエンコーディングであるかについては、仕様として決めておくか、プログラム側でエンコーディングを指定するか、あるいはBOMを付けるという仕様があるなら`unicode:bom_to_encoding/1`等を使って判別してやるようにしましょう。

## 文字列を`iodata()`や`iolist()`として扱わない

`iodata()`や`iolist()`は、データを表す型です。

```erlang
-type iodata() :: iolist() | binary().
-type iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []).

-type byte() :: 0..255.
```

文字列とデータは、実際の型は似ていますが、その意図は全然違います。
そのため、`iodata()`や`iolist()`を引数に取る関数に、文字列を渡してはいけません。

渡すとこういうことになります。

```erlang
1> iolist_to_binary("あいうえお").
** exception error: bad argument
     in function  iolist_to_binary/1
        called as iolist_to_binary([12354,12356,12358,12360,12362])
```

前のセクションで書いたように、文字列はUnicodeのコードポイントで扱われるため、256以上になります。
しかし`byte()`型は`0..255`という値の範囲しか許していません。
そのため256以上の値が入った文字列を`iolist_to_binary/1`で変換しようとするとエラーになります。

文字列とデータは全く別の存在であることを意識し、もし文字列をバイナリに変換したいなら、`unicode:characters_to_binary/{1,2,3}`を使うようにしましょう。

```erlang
1> unicode:characters_to_binary("あいうえお").
<<227,129,130,227,129,132,227,129,134,227,129,136,227,129,138>>
```

ただ、これは型でうまく分けれないため、ごっちゃにして扱ってることに気づくのは結構難しいと思います。
型の別名を付けてあげて、コードレビューする際に気を付けるしか無いんじゃないかなと思います。

## フォーマット文字列で`~s`を使わない

フォーマット文字列で `~s` を使う場合、引数に指定可能なのは `iolist() | binary() | atom()` です。
`iolist() | binary()` は `iodata()` 型であるので、`iodata() | atom()` である、と言ってもいいでしょう。

`iodata()`は文字列を表す型ではないので、`~s`に文字列を渡すのは間違いです。
もし渡すと、以下のようになります。

```erlang
1> io:format("~s~n", ["あいうえお"]).
** exception error: bad argument
     in function  io:format/3
        called as io:format(<0.50.0>,"~s~n",[[12354,12356,12358,12360,12362]])
```

代わりに`~ts`を使います。

```erlang
2> io:format("~ts~n", ["あいうえお"]).
あいうえお
ok
```

正直`~s`を使う場面というのがいまいち分からないので、ここで使えるよ的なのがあれば教えてください。

なお、`erl +pc unicode`で起動した状態で、`~p`の代わりに`~tp`を使うと、日本語を渡した際に綺麗な表示になります。

```erlang
$ erl
1> io:format("~p~n", ["あいうえお"]).
[12354,12356,12358,12360,12362]
ok

2> io:format("~tp~n", ["あいうえお"]).
[12354,12356,12358,12360,12362]
ok

$ erl +pc unicode
1> io:format("~p~n", ["あいうえお"]).
[12354,12356,12358,12360,12362]
ok

2> io:format("~tp~n", ["あいうえお"]).
"あいうえお"
ok
```

ただし、任意の値（ただの整数のリストとか）を渡した際に変な文字列として表示される可能性が上がるので、扱いは少し慎重になった方がいいかもしれません。

## `io_lib:format/2`の戻り値に気を付ける

`io_lib:format/2`の戻り値は `chars()` 型です。
`chars()`型は以下のように定義されています。

```erlang
char() :: 0..16#10ffff
chars() :: [char() | chars()]
```

`char()` は Unicode のコードポイントの範囲を取り、`char()`の値か、そのリストを要素とするリストが `chars()` 型になっています。

この戻り値を`binary()`型に変換する方法を調べると、`iolist_to_binary/1`や`list_to_binary/1`を使っているのが見つかったりしますが、前のセクションで書いたように、文字列を単なるデータ列として扱うのは間違いです。

ちゃんと `unicode:characters_to_binary/1` を使って変換するようにしましょう。

```erlang
$ erl +pc unicode
1> unicode:characters_to_binary(io_lib:format("~ts ~ts~n", ["foo", "あいうえお"])).
<<"foo あいうえお\n"/utf8>>
```

なお `io_lib:format/2` の結果をコードポイントのリスト（つまり文字列）にする際には `lists:flatten/1` を使えばいいです。

```erlang
$ erl +pc unicode
1> lists:flatten(io_lib:format("~ts ~ts~n", ["foo", "あいうえお"])).
"foo あいうえお\n"
```

## 参考

- [Erlang -- Using Unicode in Erlang](http://erlang.org/doc/apps/stdlib/unicode_usage.html)
- [Erlang -- unicode](http://erlang.org/doc/man/unicode.html)
- [Erlang -- io](http://erlang.org/doc/man/io.html)
- [Erlang -- io_lib](http://erlang.org/doc/man/io_lib.html)
