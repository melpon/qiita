「使ってみた」じゃなくて「使ってみなかった」という話です。

## exredis とは

exredis は Elixir 製の Redis のクライアントライブラリです。

[hex.pm](https://hex.pm/) で、Elixir 製のよく使われている Redis のクライアントライブラリを探してみると、[Redix](https://hex.pm/packages/redix) と [exredis](https://hex.pm/packages/exredis) が見つかります。

exredis は信頼のおける Erlang 製の [eredis](https://hex.pm/packages/eredis) を使っていて、安定して動きそうなイメージがあります。

## exredis の酷い部分

しかしソースコードを見てみると酷くて、何が酷いかというと、[Redis にクエリを投げている部分](https://github.com/artemeff/exredis/blob/master/lib/exredis.ex#L134-L135)です。

```elixir
  def query(client, command) when (is_pid(client) or is_atom(client)) and is_list(command), do:
    client |> :eredis.q(command) |> elem(1)
```

`:eredis.q/2` の戻り値は `{:ok, value}` か `{:error, reason}` な訳ですが、**両方を纏めて `elem(1)` しています**。
つまり戻り値は `value` か `reason` になるので、**戻り値を見ても成功なのかエラーなのか区別が付かない** という最高に酷い動作になっています。

exredis は Redis を操作するために最終的にこの関数を呼んでいるので、ほとんどの関数は成功なのかエラーなのか区別が付かないということになります。

なおこれは2015年に [issue に登録](https://github.com/artemeff/exredis/issues/55) され、2017年12月現在、まだ直っていません。
マイルストーンが 1.0.0 になってますが、1.0.0 がいつリリースされるのかは分かりません。

## exredis を使わない

exredis は処理の根本がまともに書かれていないので、プロダクションでは使わない方がいいでしょう。
むしろ、これが Elixir の Redis クライアントライブラリとしてよく使われているのが驚きです。[^1]

[^1]: 今見てみると redix が 407,631 ダウンロード、exredis が 166,778 ダウンロードでした

よく使われているライブラリでも酷いコードがあったりするので、利用するライブラリには目を通しておきましょう、という話でした。
