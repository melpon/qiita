大体の Elixir 使いにとって、[`inspect`](https://hexdocs.pm/elixir/Kernel.html#inspect/2) 関数は「どんな term でも文字列にしてくれる便利な奴」という認識だと思います。
実際それはそれで正しいのですが、もうちょっと詳しく調べてみると、めちゃめちゃ奥が深いことが分かりました。
調べたバージョンは Elixir 1.5.2 です。

## 出力フォーマットについて

`inspect/2` は、普通は何もせず文字列に変換するだけですが、オプションを指定すると、もっといい感じにやってくれます。
詳細は [`Inspect.Opts` のドキュメント](https://hexdocs.pm/elixir/Inspect.Opts.html) に書いてる……と言いたいのですが、全然詳細に書いてくれてないので、ここに書いている内容の方が細かいです。
ここでは紹介していないオプションもあるので、一覧としては `Inspect.Opts` のドキュメントの方を見た方がいいでしょう。

### `:limit`

タプル、ビットストリング、マップ、リストやその他コレクション全体で表示する要素数を指定します。
**`:infinity` を指定すると全て表示します** 。
デフォルトは50個です。

```elixir
# 要素数３個までしか表示しない
iex> inspect [1, 2, 3, 4, 5], limit: 3
"[1, 2, 3, ...]"
```

「`inspect/2` したけど要素が途中で `...` になっててデバッグできない！」という場合は `:infinity` を指定するのがいいでしょう。
ただしログの量が激増して動作が大変なことになる可能性があるので注意して下さい。

また、本当に正確に `:limit` の要素数しか表示しないという訳ではありません。
各要素を表示する時に `:limit` を1つずつ減らし、0以下になったら表示しないというのをひたすら再帰して実行していくというロジックなので、ネストしたデータの場合は以下のようになります。

```elixir
iex> inspect [{1, 2, 3}, {4, 5, 6}, {7, 8, 9}, {10, 11, 12}], limit: 4
"[{1, 2, 3}, {4, 5, ...}, {7, ...}, {10, ...}]"
```

`limit: 4` ですが、7要素表示されていることが分かります。
なぜこうなるのか、詳細に説明すると長くなりすぎるので省略します。
気になる人は [この辺](https://github.com/elixir-lang/elixir/blob/05418eaa4bf4fa8473900741252d93d76ed3307b/lib/elixir/lib/inspect/algebra.ex#L556-L594) を見てみるといいでしょう。

### `:printable_limit`

文字列や文字のリストを表示するコードポイント数を指定します。[^bytes]
**`:infinity` を指定すると全て表示します** 。
デフォルトは4096文字です。

[^bytes]: Elixir 1.5.2 の `Inspect.Opts` のドキュメントにはバイト数って書いてるんだけど、[コードを読んでみた](https://github.com/elixir-lang/elixir/blob/05418eaa4bf4fa8473900741252d93d76ed3307b/lib/elixir/lib/inspect.ex#L170-L176) らコードポイントで計算してた。ただし UTF-8 として無効な文字列であってもエスケープして表示しようとするので、コードポイントで数えるというのは正確な表現ではない。

```elixir
iex> inspect ["123456789", "あいうえおかきく"], printable_limit: 5
"[\"12345\" <> ..., \"あいうえお\" <> ...]"
```

「`inspect/2` したけど文字列が途中で ... になっててデバッグできない！」という場合は `:infinity` を指定するのがいいでしょう。
 ただしログ（ｒｙ

上記の２つのオプションを合わせてやれば常に全ての term が出力されるので、覚えておくと便利でしょう。
ただし（ｒｙ

```elixir
def dump(value) do
  inspect value, limit: :infinity, printable_limit: :infinity
end
```

### `:width`

表示する横幅を指定します。
ただし **`inspect/2` 関数の場合は `:pretty` が `true` の時しか効果がありません**。
逆に **[`IO.inspect/2`](https://hexdocs.pm/elixir/IO.html#inspect/2) 関数の場合は常に `:pretty` を無視します**。

`:infinity` を指定すると横幅を気にせず表示します。
そのため、`IO.inspect/2` を使って `IO.puts "#{inspect value}"` と同じ表示にするなら `IO.inspect value, width: :infinity` のように `:infinity` を指定するのがいいでしょう。

0 を指定した場合は、各要素毎に改行します。
デフォルトは80文字です。

```elixir
# inspect で :pretty を指定しない場合、:width は無視される
iex> inspect [1, 2, 3], width: 0              
"[1, 2, 3]"

# pretty: true にすると width: 0 が有効になる
iex> inspect [1, 2, 3], pretty: true, width: 0
"[1,\n 2,\n 3]"

# IO.inspect は pretty オプションを無視して常に pretty print する
iex> IO.inspect [1, 2, 3], pretty: false, width: 0
[1,
 2,
 3]
"[1,\n 2,\n 3]"
```

### `:binaries`

渡されたバイナリをどのように扱うかを指定します。
文字列も単なるバイナリなので、`inspect/2` ではこのオプションを見てどのように表示するかを決めます。
`:as_strings` を指定すると文字列として、`:as_binaries` を指定するとビット構文として表示します。

デフォルトでは `:infer` になっていて、これは [`String.printable?/2`](https://hexdocs.pm/elixir/String.html#printable?/2) が `true` の場合には文字列として、それ以外の場合はビット構文として出力します。

```elixir
iex> inspect("olá")
"\"olá\""

iex> inspect("olá" <> <<0>>)
"<<111, 108, 195, 161, 0>>"

iex> inspect("olá" <> <<0>>, binaries: :as_strings)
"\"olá\\0\""

iex> inspect("olá", binaries: :as_binaries)
"<<111, 108, 195, 161>>"
```

### `:charlists`

渡されたリストをどのように扱うかを指定します。
文字のリストも単なるリストなので、`inspect/2` ではこのオプションを見てどのように表示するかを決めます。
`:as_charlists` を指定すると文字のリストとして、`:as_lists` を指定するとリストとして表示します。

デフォルトでは `:infer` になっていて、これは [`Inspect.List.printable?/2`](https://github.com/elixir-lang/elixir/blob/05418eaa4bf4fa8473900741252d93d76ed3307b/lib/elixir/lib/inspect.ex#L309-L324) が `true` の場合には文字のリストとして、それ以外の場合はリストとして出力します。
実装を見る限り、アルファベットや改行やタブ以外、例えば日本語が含まれている場合はリストとして表示されるようです。
[Erlang と似たような挙動](https://qiita.com/melpon/items/ff11acc6a64268f78a90#%E6%96%87%E5%AD%97%E5%88%97%E3%81%AF%E3%82%B3%E3%83%BC%E3%83%89%E3%83%9D%E3%82%A4%E3%83%B3%E3%83%88%E3%81%AB%E3%81%AA%E3%82%8B) ですね。

```elixir
iex> inspect('bar')
"'bar'"

iex> inspect('barバー')
"[98, 97, 114, 12496, 12540]"

iex> inspect('barバー', charlists: :as_charlists)
"'barバー'"

iex> inspect('bar', charlists: :as_lists)
"[98, 97, 114]"
```

### `:syntax_colors`

出力する文字列に色を付けることもできます。

```elixir
iex> inspect %{x: 10, y: 20}, syntax_colors: [atom: :cyan, map: :magenta, number: [:black, :light_blue_background]]
"\e[35m%{\e[0m\e[36mx: \e[0m\e[30m\e[104m10\e[0m\e[35m,\e[0m \e[36my: \e[0m\e[30m\e[104m20\e[0m\e[35m}\e[0m"
```

これは、atom を `:cyan` で、マップを `:magenta` で、数値を `:black` かつ背景色を `:light_blue_background` で表示しています。

見ての通り [ANSI エスケープコード](https://en.wikipedia.org/wiki/ANSI_escape_code) を使って色を付けていて、ANSI エスケープコードに対応したコンソールでこれを表示させると、以下のようになります。

<img width="522" alt="スクリーンショット 2017-11-21 10.59.12.png" src="https://qiita-image-store.s3.amazonaws.com/0/64060/15eea137-b412-e72c-d5ed-d765ab3c74cb.png">

`:syntax_colors` に渡すデータは、キーには任意の atom を入れられますが、標準では `:number`, `:atom`, `:regex`, `:tuple`, `:map`, `:list`, `:reset` しか使っていないので、独自に拡張しない限りはこれ以外を入れても効果はありません。それ以外のキーは独自に拡張した時に利用することになります。
値には [IO.ANSI](https://hexdocs.pm/elixir/IO.ANSI.html) に定義されている色の関数名（atom）か、任意の文字列、あるいはそれらのリスト（ネスト可）を指定可能です。

## `Inspect` プロトコルについて

自分で作った構造体は、デフォルトだと以下のように表示されます。

```elixir
defmodule MyStruct do
  defstruct [:x, :y]
end

iex> IO.puts(inspect %MyStruct{x: 10, y: 20})
%MyStruct{x: 10, y: 20}
```

基本的にはこれで問題ないと思いますが、この表示をカスタマイズすることもできます。
その場合、`Inspect` プロトコルを自分で実装することになります。

### `Inspect` プロトコルを実装する

```elixir
defimpl Inspect, for: MyStruct do
  def inspect(term, opts) do
    "(#{term.x}, #{term.y})"
  end
end

iex> IO.puts(inspect %MyStruct{x: 10, y: 20})
(10, 20)
```

`term` には `MyStruct` 型の構造体が渡されるので、それらを文字列化するだけで実装できます。
これで `MyStruct` 構造体を独自のフォーマットで表示できました。

### 適切に改行を入れる

ただし、これだと改行の処理が適切に行われません。

```elixir
iex> IO.puts(inspect %MyStruct{x: 10, y: 20}, pretty: true, width: 0)
(10, 20)

# 本当は
# (10,
#  20)
# になって欲しい
```

適切な場所で改行してもらうには、どこが値の区切りであるかを `inspect/2` の戻り値で伝える必要があります。
区切り情報やネストの情報といったメタ情報を付けて返すために使うのが [`Inspect.Algebra`](https://hexdocs.pm/elixir/Inspect.Algebra.html) です。

`Inspect.Algebra` を使うと、`:width` や `:limit` といったオプションによって適切に表示できるようになります。
複数のデータをうまいこと表示するために、丁度 [`Inspect.Algebra.surround_many/6`](https://hexdocs.pm/elixir/Inspect.Algebra.html#surround_many/6) という便利な関数があるので、これを使いましょう。[^1]

[^1]: なお `Inspect.Algebra.surround_many/6` は Elixir 1.6 で Soft Deprecated になり、Elixir 1.8 で Deprecated になる予定です。Elixir 1.6 以降は `Inspect.Algebra.container_doc/6` を使いましょう。

```elixir
defimpl Inspect, for: MyStruct do
  def inspect(term, opts) do
    Inspect.Algebra.surround_many("(", [term.x, term.y], ")", opts, &Inspect.inspect/2)
  end
end

iex> IO.puts(inspect %MyStruct{x: 10, y: 20})
(10, 20)

# 幅を小さくすれば適切に改行される
iex> IO.puts(inspect %MyStruct{x: 10, y: 20}, pretty: true, width: 0)
(10,
 20)

# ネストしててもちゃんと動作する
iex> IO.puts(inspect %MyStruct{x: 10, y: %MyStruct{x: 100, y: 200}}, pretty: true, width: 0)
(10,
 (100,
  200))

# limit もうまいこと動作する
iex> IO.puts(inspect %MyStruct{x: 10, y: %MyStruct{x: 100, y: 200}}, pretty: true, width: 0, limit: 2)
(10,
 (100,
  ...))
```

これで横幅によって適切に改行が入り、しかもネスト時の行頭の空白も正しく動作しています。

### 色を付ける

もう少し頑張れば、色を付けることもできます。

```elixir
defimpl Inspect, for: MyStruct do
  def inspect(term, opts) do
    Inspect.Algebra.surround_many(
      Inspect.Algebra.color("(", :my_struct, opts),
      [term.x, term.y],
      Inspect.Algebra.color(")", :my_struct, opts),
      opts,
      &Inspect.inspect/2,
      Inspect.Algebra.color(",", :my_struct, opts))
  end
end

iex> inspect %MyStruct{x: 10, y: 20}, syntax_colors: [my_struct: :red_background]
"\e[41m(\e[0m10\e[41m,\e[0m 20\e[41m)\e[0m"
```

独自のキー `:my_struct` の色を使うようにしています。
そのため `:syntax_colors` オプションに `:my_struct` を定義することでいい感じに表示できます。

コンソールで表示すると以下のようになります。

<img width="687" alt="スクリーンショット 2017-11-22 5.26.25.png" src="https://qiita-image-store.s3.amazonaws.com/0/64060/2e8f9cd5-3937-48a7-4da0-4f7833be63f9.png">

## `IO.inspect/2` について

出力フォーマットの説明でも書きましたが、`IO.puts(inspect value)` と `IO.inspect value` は同じ出力にはなりません。
`IO.inspect/2` は、`IO.puts(inspect value)` と以下の点で異なります。

- `IO.inspect/2` は `:pretty` オプションを無視して常に pretty print する。
- `IO.inspect/2` は `:label` オプションが使える。使うと `"my_label: value"` のように表示される。
- `IO.inspect/2` は戻り値として、与えられた値を返す。そのため、パイプラインの途中でちょっと値を見るということが簡単にできる。

ドキュメントに書いているように、`IO.inspect/2` の `:label` と戻り値を利用することで、パイプライン途中の値をいい感じに覗き見できます。

```elixir
[1, 2, 3]
|> IO.inspect(label: "before")
|> Enum.map(&(&1 * 2))
|> IO.inspect(label: "after")
|> Enum.sum
```

以下のように出力されます。

```
before: [1, 2, 3]
after: [2, 4, 6]
```

## まとめ

`inspect/2` の深淵に飲み込まれないように注意しましょう。
