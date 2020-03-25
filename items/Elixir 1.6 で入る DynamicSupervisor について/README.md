Elixir 1.6 で [`DynamicSupervisor`](https://hexdocs.pm/elixir/master/DynamicSupervisor.html) というモジュールが入る予定です。

## `DynamicSupervisor` とは

端的に言えば **`:simple_one_for_one` を殺すためのモジュール** です。

`Supervisor` モジュールの再起動戦略として `:one_for_one`, `:rest_for_one`, `:one_for_all`, `:simple_one_for_one` がありますが、この `:simple_one_for_one` の代替となる機能を `DynamicSupervisor` は提供します。
実際 Elixir 1.6 で `:simple_one_for_one` は *非推奨(deprecated)* になります。[^deprecated]

[^deprecated]: `:simple_one_for_one` は、非推奨になるしドキュメントからも消えるものの、機能としてはずっと残り続けます（OTP の機能なので）。

`Supervisor` では child spec を書いて子と一緒に起動させることが多いですが、`DynamicSupervisor` は必ず子が空の状態で起動します。
起動後、`DynamicSupervisor.start_child/2` で動的に追加する形になります。まさに `:simple_one_for_one` の機能です。

## なぜ `:simple_one_for_one` を使わないのか

`:simple_one_for_one` が、Erlang のスーパーバイザの中でかなり特殊だからです。

スーパーバイザのドキュメントには「`:simple_one_for_one` でない場合は〜となる。`:simple_one_for_one` の場合は〜となる」というのが多くあり、スーパーバイザを理解するのが難しくなっています。
実際、例えば [`Supervisor.start_child/2`](https://hexdocs.pm/elixir/1.5.2/Supervisor.html#start_child/2) は、`:simple_one_for_one` 以外なら child spec を渡すのに、`:simple_one_for_one` の場合は起動する引数のリストを渡す必要があります。
また、`:simple_one_for_one` には各プロセスに対して一意な ID が不要で、そのために [`Supervisor.terminate_child/2`](https://hexdocs.pm/elixir/1.5.2/Supervisor.html#terminate_child/2) で PID しか指定できないし、[`Supervisor.delete_child/2`](https://hexdocs.pm/elixir/1.5.2/Supervisor.html#delete_child/2) はサポートしていません。

この問題を解決するため、`:simple_one_for_one` を別のモジュールとして独立させて利用することにしたようです。[^1]

[^1]: あとは [GenStage](https://hex.pm/packages/gen_stage) で子の数を制限したりできるスーパーバイザが欲しかったというのもあったみたいだけれども、GenStage が Elixir に合流する予定が無くなったのでそこはあまり理由にならない

実際、`DynamicSupervisor` を導入することによって、`Supervisor` のドキュメントは [結構スッキリしました](https://github.com/elixir-lang/elixir/commit/7f8db39ce27854c34b7d722783f5e89a9058fbe1#diff-9d432a14620b20769d459b1041935cf8)。

## `DynamicSupervisor` を現実的な方法で利用する

簡単な利用方法は [Elixir 1.6.0 の新機能の紹介](https://qiita.com/kdxu/items/5e11a9b26700ac55fed2#dynamic-supervisor) に書かれています。
`DynamicSupervisor` を起動するには、以下のように `start_link/1` を使えばいいようです。

```elixir
{:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)
```

ただ、実際にこういうコードを書くことは無く、大抵の場合はアプリケーションのスーパーバイザにぶら下げることになるでしょう。
ここでは、現実的に `DynamicSupervisor` を利用したコードを書くならどのようになるのかを説明します。

`DynamicSupervisor` をモジュールベースで利用するなら、以下のように書いてアプリケーションのスーパーバイザにぶら下げることになります。

```elixir
defmodule MyApp.Client.Supervisor do
  use DynamicSupervisor

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl DynamicSupervisor
  def init([]) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end

defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      MyApp.Client.Supervisor,
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
  end
end
```

あるいは `MyApp.Client.Supervisor` モジュールを定義するのが面倒なら以下のように書くという手もあります。

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      %{
        id: MyApp.Client.Supervisor,
        start: {
          DynamicSupervisor,
          :start_link,
          [[name: MyApp.Client.Supervisor, strategy: :one_for_one]]
        }
      },
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
  end
end
```

詳細については [スーパーバイザの下にスーパーバイザをぶら下げる簡単な方法](https://qiita.com/melpon/items/63c4baf9aae82a2821f2) を参照して下さい。この記事と同様の方法でスーパーバイザの下に `DynamicSupervisor` をぶら下げています。

これで `DynamicSupervisor` を起動できたので、あとは子を起動するだけです。
`:simple_one_for_one` では `init/1` 時に指定した1種類のモジュールしか起動できませんでしたが、`DynamicSupervisor` では **何種類のモジュールでも起動できます**。
が、通常は1種類しか使わないので、今回の例も1種類のモジュールだけでやります。

```elixir
defmodule MyApp.Client do
  use GenServer, restart: :temporary

  def start_link([]) do
    GenServer.start_link(__MODULE__, [])
  end
end

# 子を生成
DynamicSupervisor.start_child(MyApp.Client.Supervisor, MyApp.Client)
```

`DynamicSupervisor.start_child/2` の第一引数にはスーパーバイザの名前か pid を、第二引数には child spec を渡します。
モジュール名を渡せば、そのモジュールの `child_spec/1` を呼び出して起動します。
詳細については [Elixir 1.5 の合理化された child spec](https://qiita.com/melpon/items/bae4105c68f8da50e3c7) を参照して下さい。

`use GenServer` がデフォルトで定義する `child_spec/1` は `restart: :permanent` になっているので、大体の場合は `:temporary` に変えておいた方がいいでしょう。
`DynamicSupervisor` の子は動的に大量に作ることになり、大抵の場合は再起動したら困るものが殆どになるはずです。

また、`DynamicSupervisor` の性質上、任意の文字列をキーにして子プロセスを検索したくなるということがあります。
例えばセッションならセッションID、何らかのルームならルームIDで pid を検索、といった具合です。
`:one_for_one` で作ったプロセスなら、大体 `GenServer.start_link/3` 時に `name: __MODULE__` とか書いて名前を付けて利用するだけで済みますが、`DynamicSupervisor` では子を動的に作るので atom を動的に作ることになり、これは死ぬ未来しかありません。
そのため、子プロセスを整数や文字列で検索したくなるのです。

このような用途のために、丁度 [`Registry`](https://hexdocs.pm/elixir/Registry.html) というライブラリがあります。
これを使えば、以下のように書けます。

```elixir
defmodule MyApp.Client do
  use GenServer, restart: :temporary

  def start_link([client_id]) do
    name = {:via, Registry, {MyApp.Client.Registry, client_id}}
    GenServer.start_link(__MODULE__, [], name: name)
  end
end

client_id = "..."

# 子プロセスを生成
DynamicSupervisor.start_child(MyApp.Client.Supervisor, {MyApp.Client, [client_id]})

# Registry を経由して client_id のプロセスを呼び出す
name = {:via, Registry, {MyApp.Client.Registry, client_id}}
value = GenServer.call(name, :get_value)
```

`MyApp.Client.start_link/1` で、`name` の指定をよく分からない形式で書いていますが、これは `Registry` のドキュメントにある通りの書き方です。
このよく分からない形式は、`GenServer.call/2` 時にも使えて、このように書くことでプロセス ID や atom 形式の名前の代わりに利用できます。
詳細は後日書きます。→ @kenichirow が書いてくれました [Registry の via_tuple について](https://qiita.com/kenichirow/items/d9c08b30da81f7fa2364)

なお、ここまで書くことになったら、`MyApp.Client.Supervisor` をモジュールベースにして、`MyApp.Client.Supervisor.start_child(client_id)` のように書いて構築できるようにしておいた方がいいでしょう。
また、`MyApp.Client.get_value()` を定義して、その中で `GenServer.call/2` するようなコードにしておいた方が良いです。[^2]

[^2]: これは `DynamicSupervisor` とは関係なく、普通の抽象化の話です。

最終的に、`DynamicSupervisor` を利用した一般的なアプリケーションは、以下のように書くことになるでしょう。

```elixir:lib/my_app/client.ex
defmodule MyApp.Client do
  # restart: :temporary にする
  use GenServer, restart: :temporary

  # 一意な ID を受け取り Registry に登録して起動する
  def start_link([client_id]) do
    name = {:via, Registry, {MyApp.Client.Registry, client_id}}
    GenServer.start_link(__MODULE__, %{value: 100}, name: name)
  end

  @impl GenServer
  def handle_call(:get_value, _from, state) do
    {:reply, state.value, state}
  end

  # Registry 経由で call する処理を自身のモジュールで提供する
  def get_value(client_id) do
    name = {:via, Registry, {MyApp.Client.Registry, client_id}}
    GenServer.call(name, :get_value)
  end
end
```

```elixir:lib/my_app/client/supervisor.ex
defmodule MyApp.Client.Supervisor do
  use DynamicSupervisor

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl DynamicSupervisor
  def init([]) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  # 子を起動する処理を関数で提供する
  def start_child(client_id) do
    DynamicSupervisor.start_child(__MODULE__, {MyApp.Client, [client_id]})
  end
end
```

```elixir:lib/my_app/application.ex
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      MyApp.Client.Supervisor,
      # MyApp.Client 用の Registry を起動しておく
      {Registry, keys: :unique, name: MyApp.Client.Registry},
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
  end
end
```

これによって、以下のようにシンプルに利用できるようになります。

```elixir
client_id = "..."

# 子プロセスを起動する
MyApp.Client.Supervisor.start_child(client_id)

# 子プロセスからデータを取得する
value = MyApp.Client.get_value(client_id)
```

## まとめ

`:simple_one_for_one` のことは忘れて、Elixir 1.6 からは良い感じに `DynamicSupervisor` を活用していきましょう。

## 参考

- [Proposal for DynamicSupervisor · Issue #12 · elixir-lang/gen_stage](https://github.com/elixir-lang/gen_stage/issues/12)
- [The future of GenStage and Flow - Elixir News - Elixir Forum](https://elixirforum.com/t/the-future-of-genstage-and-flow/3323)
- [DynamicSupervisor – Elixir v1.6.0-dev](https://hexdocs.pm/elixir/master/DynamicSupervisor.html)

