Elixir には文字列補間(string interpolation)の機能があります。

```elixir
iex> x = 10
iex> "foo_#{x}"
"foo_10"
```

文字列 `""` の中で `#{}` を使うことで、その中にある式を評価し、文字列として結合してくれます。

ところで、atom を記述するには `:foo` のように書く訳ですが、`:"it is an atom!"` のように `:""` を使うことで記号や空白を含む場合でも atom を書くことができます。

同じように `""` で囲んでるんだから、文字列補間でatomを作れるのではと思ってやってみると、できました。

```elixir
iex> x = 10
iex> :"foo_#{x}"
:foo_10
```

なんか危険な臭いが漂ってきたので、どうやって実現しているのか気になり、どんな式に展開されているか確認してみました。
こういう時には `quote` がとても便利です。

```elixir
iex> quote do
...>   :"foo_#{x}"
...> end
{{:., [], [:erlang, :binary_to_atom]}, [],
 [{:<<>>, [],
   ["foo_",
    {:::, [],
     [{{:., [], [Kernel, :to_string]}, [], [{:x, [], Elixir}]},
      {:binary, [], Elixir}]}]}, :utf8]}
```

ここで出てきた式をゴリゴリ読みやすくしていくと、最終的にこんな式になっていることが分かります。

```elixir
:erlang.binary_to_atom(<<"foo_", to_string(x) :: binary>>, :utf8)
```

見ての通り、文字列補間した結果を [`:erlang.binary_to_atom/2`](http://erlang.org/doc/man/erlang.html#binary_to_atom-2) [^4]しています。これは非常に危険です。
[binary_to_atom/2 は使わない！！ #Erlang](https://qiita.com/KOU_CHANG/items/c5eef2c5e6dc35b3fd65) にあるように、atom は一度生成されると二度と解放されないし、atom テーブルのサイズはデフォルトだとそこまで大きくないため、動的に生成してると割とすぐに死にます。[^1]

[^4]: Elixir で言う [`String.to_atom/1`](https://hexdocs.pm/elixir/String.html#to_atom/1) 関数のこと
[^1]: すぐ死んでくれるならまだマシで、本番で稼働して一週間経ってから死ぬとかだと大分つらいと思う

なので、atom文字列補間[^2]は `:erlang.binary_to_atom/2` と同程度に慎重に扱った方がいいでしょう。
大体の場合は [`String.to_exsisting_atom/1`](https://hexdocs.pm/elixir/String.html#to_existing_atom/1) で問題ないし、これが使えなくてatomが無限に増える可能性があるなら、設計から見直すべきです。

[^2]: 「atom文字列補間」は正式に使われてる用語ではなく、自分が適当に付けました。むしろ正式には何て呼べばいいの。

atom文字列補間を利用する正当な理由としては、例えば大量の同じような関数を定義する時です。[^3]

[^3]: ただしマクロを使う正当な理由にはならないので、ほんとにこのマクロが必要かは別途考える必要があります。

```elixir
for {name, value} <- @name_values do
  for {name2, value2} <- @name_values2 do
    def unquote(:"get_#{name}_#{name2}")() do
      unquote(value <> value2)
    end
  end
end
```

これなら実行時には atom を生成しないので問題ないでしょう。

atom文字列補間を使う場合には気を付けて利用しましょう。
