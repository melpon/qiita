Elixir を使ったことのあるほとんどの人は [`Enum` モジュール](https://hexdocs.pm/elixir/Enum.html) を使ったことがあると思います。

`Enum` モジュールに渡す Enumerable な値は、大抵の場合はリストだったりマップだったりといったデータ構造を渡すと思いますが、実はこれ、関数も渡せます。

```elixir
defmodule Foo do
  def enum_fun(acc, reducer) do
    enum_fun_impl(0, acc, reducer)
  end
  
  def enum_fun_impl(count, {:cont, acc}, reducer) when count < 5 do
    Enumerable.reduce(&enum_fun_impl(count + 1, &1, &2), reducer.(count, acc), reducer)
  end
  def enum_fun_impl(_count, {:cont, acc}, _reducer) do
    {:done, acc}
  end
end

# Enumerable として関数を渡す
IO.inspect Enum.to_list(&Foo.enum_fun/2)
IO.inspect Enum.map(&Foo.enum_fun/2, &(&1 * 10))
```

出力:

```
[0, 1, 2, 3, 4]
[0, 10, 20, 30, 40]
```

これで動作するのは、Elixir が関数に対して `Enumerable` プロトコルを実装しているからです。
[ソースを見てみる](https://github.com/elixir-lang/elixir/blob/v1.5.2/lib/elixir/lib/enum.ex#L3202-L3218
) と、以下の実装になっています。

```elixir
defimpl Enumerable, for: Function do
  ...

  def reduce(function, acc, fun) when is_function(function, 2),
    do: function.(acc, fun)

  ...
```

すごいシンプルな実装ですが、これを実装するのは結構大変です。
`Enumerable.reduce/3` の型情報とか見ながらうまく実装していくことで、`Foo.enum_fun/2` のような 0 から 4 までの整数を列挙する `Enumerable` な関数を実装できます。

これをもう少し汎用的にすると、`[n, m)` な範囲の整数を列挙する関数が作れます。

```elixir
defmodule OreoRange do
  def enum(first, last) do
    &enum_fun_impl(first, last, &1, &2)
  end
  
  def enum_fun_impl(count, last, {:cont, acc}, reducer) when count < last do
    Enumerable.reduce(&enum_fun_impl(count + 1, last, &1, &2), reducer.(count, acc), reducer)
  end
  def enum_fun_impl(_count, _last, {:cont, acc}, _reducer) do
    {:done, acc}
  end
end

IO.inspect Enum.to_list(OreoRange.enum(10, 20))
```

出力:

```
[10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
```

リストと違ってメモリ上に置かないので、省メモリで列挙できます。
ただ、これらは普通に `Range` を使えばいいだけだし、実際には関数に対する `Enumerable` プロトコルの実装をユーザが直接利用することはまず無いでしょう。

これが実際何に使われているかというと、`Stream` や `Task` の内部実装です。
例えば `Stream.flat_map/2` が返すのは `Enumerable` プロトコルを満たした関数です。

```elixir
iex(1)> xs = Stream.flat_map([1, 2, 3], fn(x) -> [x, x * 10] end)
#Function<58.40091930/2 in Stream.transform/3>
```

`Stream.flat_map/2` の戻り値が関数になっていることが分かります。
この関数は `Enumerable` プロトコルを満たしているため、これを `Enum.to_list/1` に渡すと正常にリストに変換できます。

```elixir
iex(2)> Enum.to_list(xs)
[1, 10, 2, 20, 3, 30]
```

ということで、`Stream` モジュールが返す値がなぜか関数になっていても焦らず、とりあえず `Enum.to_list/1` 等に入れてみて `Enumerable` として動作するかを確認するのがいいでしょう。
