[Plug](https://hexdocs.pm/plug/readme.html) には、起動時に任意の値を渡すことで、各ハンドラでそれを参照できるようになっています。

```elixir:my_router.ex
defmodule MyRouter do
  def init(opts) do
    opts
  end

  def call(conn, opts) do
    body = Keyword.fetch!(opts, :foo)
    conn |> Plug.Conn.send_resp(200, body)
  end
end
```

```elixir:application.ex
# ここで指定した値を MyRouter で使える
opts = [foo: "bar"]

# child_spec を使う場合
Plug.Adapters.Cowboy.child_spec(:http, MyRouter, opts, port: 8000)

# http を使う場合
Plug.Adapters.Cowboy.http(MyRouter, opts, port: 8000)
```

しかし `Plug.Builder` や `Plug.Router` を使っている場合、これは動きません。

```elixir:application.ex
defmodule MyRouter do
  use Plug.Builder

  plug :fun

  def fun(conn, opts) do
    # エラー！opts に :foo が入っていない
    body = Keyword.fetch!(opts, :foo)
    conn |> Plug.Conn.send_resp(200, body)
  end
end
```

`opts` の中を見てみると、空データ `[]` になっています。
実はこの `opts` は、起動時に渡した値ではなく、`plug :fun` の第二引数に渡した値になります。

```elixir:application.ex
  plug :fun, foo: "bar", hoge: "fuga"

  def fun(conn, opts) do
    assert [foo: "bar", hoge: "fuga"] == opts
  end
```

では起動時に渡した `opts` はどうなったかというと……[捨てられます](https://github.com/elixir-lang/plug/blob/v1.3.0/lib/plug/builder.ex#L134)[^1]。

[^1]: `plug_builder_call`の第二引数に渡されている `_` に起動時の `opts` が入っていますが、見事に捨てられています。

何でこんな設計にしたのか全く理解できませんが、とりあえず、このままでは`Plug.Builder`や`Plug.Router`利用時に`opts`が利用できません。

そこで、以下のように`call/2`をオーバーライドすることで`opts`を使えるようにします。

```elixir:application.ex
defmodule MyRouter do
  use Plug.Builder

  plug :fun

  def call(conn, opts) do
    # 起動時のoptsをconnに格納
    conn = Plug.Conn.assign(conn, :opts, opts)
    super(conn, opts)
  end

  def fun(conn, _opts) do
    # connに格納したoptsを取り出す
    opts = conn.assigns.opts
    body = Keyword.fetch!(opts, :foo)
    conn |> Plug.Conn.send_resp(200, body)
  end
end
```

`Plug.Builder` のデフォルト実装の `call/2` をオーバーライドして、`opts` を `conn` の中に入れておきます。
これで無事起動時の `opts` が `Plug.Builder` 利用時でも使えるようになりました。
`Plug.Router` 利用時でも同じように使えるはずです。
起動時の `opts` を使いたくなった場合はこのようにするといいでしょう。
