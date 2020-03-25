Elixir 1.5 で、新しい child spec の指定方法が[追加されました](https://elixir-lang.org/blog/2017/07/25/elixir-v1-5-0-released/#streamlined-child-specs)。
ここでは、この追加された機能の説明と、それによって何が出来るようになったかを説明します。

## 合理化された child spec[^streamlined]

[^streamlined]: 「合理化された」は Streamlined を訳したもので、Streamlined には「無駄を無くして良くしていく」みたいな意味があるらしく、一言で言うと「合理化」になるらしい。


今までは、スーパーバイザを起動する時の child spec は以下のように書いていました。

```elixir
children = [
  Supervisor.Spec.supervisor(MyApp.Repo, []),
  Supervisor.Spec.worker(MyApp.MyServer, [:foo]),
]

Supervisor.start_link(children, strategy: :one_for_one)
```

このように `Supervisor.Spec.supervisr/2` や `Supervisor.Spec.worker/2` を使うことで child spec を書いていました。

ただ、これは間違って記述する可能性があります。
`MyApp.Repo` をスーパーバイザでなくワーカーとして起動したり、`MyApp.MyServer` をワーカーでなくスーパーバイザとして起動したりといったミスをする可能性があります。
通常、各モジュールはどちらのタイプで起動するか決まっていて、子を起動する段階で決めるものではないのです。

子のタイプを指定する `:type` オプションだけでなく、`:start` や `:restart` オプションも大体同様で、多くの場合はこれらはモジュールを書く段階で決めることであり、子を起動する段階で決めることではありません。
つまり多くの場合 **child spec はそのモジュールを構成する一部分である** ということです。

そこで、モジュールに `child_spec/1` という関数を書いておけば、子を起動するのは以下のように書くだけで済むようになりました。

```elixir
children = [
  MyApp.Repo,
  {MyApp.MyServer, [:foo]},
]

Supervisor.start_link(children, strategy: :one_for_one)
```

このように書いておけば、`MyApp.Repo.child_spec([])` と `MyApp.MyServer.child_spec([:foo])` を呼んで child spec を取得して、それぞれのプロセスを起動するようになりました。
当然ですが、指定したモジュールが `child_spec/1` を実装していない場合はエラーになります。

また、この合理化された child spec により **`Supervisor.Spec` は deprecated になりました**。子の起動時に child spec をカスタマイズしたいなら、 `Supervisor.child_spec/2` を使うといいでしょう。

```elixir
Supervisor.child_spec({MyApp.MyServer, [:foo]}, shutdown: 10_000)
```

また、そのモジュールが `child_spec/1` を実装していない場合は、[マップ形式の child spec](http://erlang.org/doc/design_principles/sup_princ.html#id78910) を指定するといいでしょう。
タプル形式で渡すよりいろいろと省略が可能なので、楽ができます。

```elixir
# MyApp.MyWorker.start_link([:foo]) でプロセスを起動する
%{
  id:       MyApp.MyWorker,                          # 必須
  start:    {MyApp.MyWorker, :start_link, [[:foo]]}, # 必須
  restart:  :permanent,       # 省略可
  shutdown: 5_000,            # 省略可
  type:     :worker,          # 省略可
  modules:  [MyApp.MyWorker], # 省略可
}
```

## `use GenServer` が `child_spec/1` を実装するようになった

毎回 `child_spec/1` を定義するのは面倒なので、`use GenServer` で自動的に `child_spec/1` が実装されるようになりました。

```elixir
defmodule MyServer do
  use GenServer
end

IO.inspect MyServer.child_spec([:foo])
# %{
#   id: MyServer,
#   restart: :permanent,
#   shutdown: 5000,
#   start: {MyServer, :start_link, [[:foo]]},
#   type: :worker,
# }
```

`use GenServer` の場合、必ず `type: :worker` になります。
`child_spec/1` の定義をカスタマイズしたい場合、`use GenServer` に引数を追加するだけです。

```elixir
defmodule MyServer do
  use GenServer, restart: :temporary, shutdown: 10_000
end

IO.inspect MyServer.child_spec([:foo])
# %{
#   id: MyServer,
#   restart: :temporary,
#   shutdown: 10_000,
#   start: {MyServer, :start_link, [[:foo]]},
#   type: :worker,
# }
```

詳細は [Child specification](https://hexdocs.pm/elixir/Supervisor.html#module-child-specification) を読んで下さい。

また、`GenServer` と同様、`use Supervisor` する時にも自動的に `child_spec/1` が定義されるようになりました。

```elixir
defmodule MySupervisor do
  use Supervisor
end

IO.inspect MySupervisor.child_spec([:foo])
# %{
#   id: MySupervisor,
#   restart: :permanent,
#   start: {MySupervisor, :start_link, [[:foo]]},
#   type: :supervisor,
# }
```

`use Supervisor` の場合、必ず `type: :supervisor` になります。
モジュールベースのスーパーバイザを定義する時には良さそうです。

## 既存のモジュールが `child_spec/1` を実装した

既存の `Agent` や `Registry`、 `Task` といったライブラリも `child_spec/1` を実装するようになりました。
そのため以下のように書けるようになっています。

```elixir
children = [
  {Registry, keys: :unique, name: MyApp.Registry},
  {Agent, fn -> :ok end},
]
Supervisor.start_link(children, strategy: :one_for_one)
```

わざわざ `Supervisor.Spec.worker/2` とか呼ばなくていいので少し楽ができます。

## まとめ

合理化された child spec によって、モジュール側に child spec を書けるようになりました。

モジュール側に child spec を定義するべき場合、それを定義しておくのがいいでしょう。
幸いなことに `use GenServer` で `child_spec/1` が自動的に定義されるので、適切に引数を渡すだけで構いません。

こうすることで、適切な場所に child spec が定義されて、child spec を間違えて指定してしまうということが無くなります。
有効に活用していきましょう。
