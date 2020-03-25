Erlang/OTP 20 の ETS で [CAS が使えるようになった](http://erlang.org/doc/man/ets.html#select_replace-2) のが嬉しかったので、Elixir でメモ化ライブラリを作ってリリースしてみました。

## 動機

Elixirで、高速で、汎用的で、簡単に使えるキャッシュ処理を書くのはかなり難しいと考えています。

関数呼び出しの結果をキャッシュしようと思った時、一番最初に考えるのは以下のようなコードです。

```elixir
def f() do
  case :ets.lookup(@tab, @key) do
    [] ->
      result = load_heavy_data_from_database()
      :ets.insert(@tab, {@key, result})
    [{_, result}] ->
      result
  end
end
```

しかしこの書き方には問題があります。
`:ets.lookup/2` してから `:ets.insert/2` するまでに他のプロセスが割り込む余地があるため、この関数が複数のプロセスから同時に呼ばれた場合、`load_heavy_data_from_database/0` が同時に実行されることがあるのです。
ファイルやデータベース、外部APIなどの重い処理をキャッシュしたいのに、その重い処理が並列に呼ばれてしまうのでは意味がありません。

そこで、複数プロセスから同時に呼ばれないようにすることを考える訳ですが、Elixir の場合はプロセスを使うことになるでしょう。

```elixir
defmodule Foo do
  def init() do
    Agent.spawn_link(fn -> nil end, name: __MODULE__)
  end

  def f() do
    Agent.get_and_update(__MODULE__,
                         fn
                           nil -> result = load_heavy_data_from_database()
                                  {result, result}
                           result -> {result, result}
                         end)
  end
end

# どこかで呼んでおく
Foo.init()

# 1000プロセスから並列に呼んでいるが、
# load_heavy_data_from_database/0 が呼ばれるのは１回だけ
for _ <- 1..1000 do
  Process.spawn(fn -> Foo.f() end, [])
end
```

これで無事並列に呼ばれるのを防げるようになりました。
しかし今度は以下のような問題があります。

- 最初に `Foo.init/0` を呼び出しておく必要があるので面倒
- 全ての取得がプロセス経由になるので、複数プロセスから取得しようとした時に、このプロセスがネックになる可能性がある
- `Foo.f/0` を呼ばなかった場合は作っておいたプロセスが無駄になる
- 排他制御が必要なのは初回呼び出しだけで、以降は必要ないはずなのにプロセスが残り続けている
- 作っておいたプロセスがクラッシュした際のハンドリングを考える必要がある

いくつかの問題は頑張れば解決できますが、全て解決するのは厳しいと考えていました。

ETS でトランザクションを使えたり、せめて CAS (compare-and-swap) に相当する操作ができればプロセスを使わなくても実現できるんだけど、と考えていたところで Erlang/OTP 20 のリリースです。

## CAS on ETS

Erlang/OTP 20 で、`:ets.select_replace/2` という関数が増えました。
「条件を満たした要素を、指定した値に置き換える」という関数で、これはもしかして CAS ができるんじゃないかと[ドキュメント](http://erlang.org/doc/man/ets.html#select_replace-2)を見てみると…

> A generic single object compare-and-swap operation:
> 
> ```erlang
> [Old] = ets:lookup(T, Key),
> New = update_object(Old),
> Success = (1 =:= ets:select_replace(T, [{Old, [], [{const, New}]}])),
> ```

まさにこれです。
CAS があれば、プロセスを使わなくてもキャッシュが実現できそうです。

ということでプロセスを使わずちゃんと排他しながらキャッシュする機構を実現したのが、以下の `memoize` です。

- Hex: https://hex.pm/packages/memoize
- GitHub: https://github.com/melpon/memoize

`use Memoize` して `def` を `defmemo` に置き換えるだけで、関数の結果をキャッシュしてくれます。

例えば、

```elixir
defmodule Fib do
  def fibs(0), do: 0
  def fibs(1), do: 1
  def fibs(n), do: fibs(n - 1) + fibs(n - 2)
end
```

この `Fib.fib/1` をメモ化したいなら、以下のように変えるだけです。

```elixir
defmodule Fib do
  use Memoize
  defmemo fibs(0), do: 0
  defmemo fibs(1), do: 1
  defmemo fibs(n), do: fibs(n - 1) + fibs(n - 2)
end
```

これで自動的に関数呼び出しの結果をキャッシュしてくれます。

他にも一定時間が過ぎたら expire したり、キャッシュを部分的に消したりといった機能もあります。
詳細は [ドキュメント](https://github.com/melpon/memoize/blob/master/README.md) を参照して下さい。

## CAS でキャッシュを実現するには

CAS でキャッシュしている方法も説明しようと思ったのですが、説明するのが難しいので諦めました。
気になる方は頑張って [該当コード](https://github.com/melpon/memoize/blob/master/lib/memoize/cache.ex) を読んで下さい。
あとバグとか見つけた場合は報告して貰えると大変ありがたいです。
