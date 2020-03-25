ETS の `select/{1-3}` や `match/{1-3}` などの select, match 系の関数は、基本的に線形時間が掛かります。
ただし、キーが完全に一致する場合には最適化が掛かり、定数時間で済むようです。

この最適化について、具体的にどこまでが定数時間で済んで、どこまで線形時間が掛かるのかを調べてみました。

調べたのは `:set` の ETS だけです。
`:ordered_set` は全く違う実装になっているので、そちらだと全く異なる結果になるでしょう。

また、`:set` の select, match 系関数は、最終的に全て [`match_traverse`](https://github.com/erlang/otp/blob/OTP-20.0.1/erts/emulator/beam/erl_db_hash.c#L1209) を呼んでいるため、どの関数を呼んでも傾向としては同じだと思います。
そのため、とりあえず `select_delete/2` を使って時間を計測しています。

## 準備

ETS に 100万要素ほど詰め込みます。

```elixir
:ets.new(@tab, [:set, :public, :named_table])
for n <- 1..1_000_000 do
  :ets.insert(@tab, {{n, :key}, {0, :value}})
end
```

大体 900 ミリ秒程度掛かりました。

## :ets.delete: 4 μs

まず定数時間で消せる `:ets.delete/2` の時間を調べます。

```elixir
:ets.delete(@tab, {100, :key})
```

これは 4 **マイクロ秒** でした。
この程度の時間であれば定数時間ということです。

## :ets.select_delete with key: 9 μs

`:ets.select_delete/2` で、キーを指定して計測してみます。

```elixir
# :ets.fun2ms(fn {{100, :key}, :_} -> true end)
:ets.select_delete(@tab, [{{{100, :key}, :_}, [], [true]}])
```

これは 9 **マイクロ秒** でした。
`:ets.delete/2` と同じような時間なので、定数時間でしょう。
キーを指定すれば、ちゃんと定数時間になるようです。

## :ets.select_delete without key: 427 ms

キーを直接は指定せず、ガード句でそのキーかどうかを調べてみました。

```elixir
# :ets.fun2ms(fn {key, :_} when key == {100, :key} -> true end)
:ets.select_delete(@tab, [{{:"$1", :_}, [{:==, :"$1", {{100, :key}}}], [true]}])
```

これは 427 **ミリ秒** も掛かっています。
ガード句で条件を書いても、キーを `:"$1"` 等のワイルドカードにすると線形時間が掛かるようです。

## :ets.select_delete partial key: 206 ms

キーの `{100, :key}` の内、部分的にパターンマッチさせた場合にどうなるかを試しました。

```elixir
# :ets.fun2ms(fn {{100, :_}, :_} -> true end)
:ets.select_delete(@tab, [{{{100, :_}, :_}, [], [true]}])
```

これは 206 **ミリ秒** も掛かっています。
どうやらキーを完全に指定しないと線形時間が掛かるようです。

## :ets.select_delete with key and value: 9 μs

キーと値の両方を完全に指定した場合です。

```elixir
# :ets.fun2ms(fn {{100, :key}, {0, :value}} -> true end)
:ets.select_delete(@tab, [{{{100, :key}, {0, :value}}, [], [true]}])
```

これは 9 **マイクロ秒** で済みました。
キーを完全に指定すれば、それで検索した後、値を比較してくれるようです。

## :ets.select_delete with key and partial value: 5 μs

キーは完全に指定して、値は部分的に一致させた場合です。

```elixir
:ets.select_delete(@tab, [{{{100, :key}, {0, :_}}, [], [true]}])
```

これは 5 **マイクロ秒** でした。
先程と同様、キーで検索した後に値をパターンマッチしてくれているようです。

## まとめ

- select 系に渡す `[{MatchHead, [Guard], [Result]}]` の `MatchHead` 部分に、ワイルドカードを使わずにキーを指定すれば定数時間で済む
- キーを正しく指定していれば、値に関しては、ワイルドカードを指定していても、してなくても定数時間になる

普通は、キーが分かっているなら `lookup/2` や `delete/2` を使えばいいだけです。
ただし、`select_replace/2` はそれに相当する処理が無いので、これを覚えておけばうまく最適化できるかもしれません。
