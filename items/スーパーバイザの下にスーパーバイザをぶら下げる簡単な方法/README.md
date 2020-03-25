スーパーバイザの下にスーパーバイザを作ることはよくあるのですが、毎回定義が面倒なので何とかならないか考えてみた話です。

例えば以下のような構成でスーパーバイザツリーを作りたいとします。

- アプリケーションスーパーバイザ(one_for_one)
  - MyChannelスーパーバイザ(one_for_one)
     - １つのMyChannelワーカー
  - Connスーパーバイザ(simple_one_for_one)
     - 沢山の動的に追加されるConnワーカー

[モジュールベースのスーパーバイザ](https://hexdocs.pm/elixir/Supervisor.html#module-module-based-supervisors) を作れば、一応このスーパーバイザツリーを構築することはできます。

```elixir
defmodule MyApp.MyChannel.Supervisor do
  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl Supervisor
  def init([]) do
    Supervisor.init([MyApp.MyChannel], strategy: :one_for_one)
  end
end

defmodule MyApp.Conn.Supervisor do
  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl Supervisor
  def init([]) do
    Supervisor.init([MyApp.Conn], strategy: :simple_one_for_one)
  end
end

defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      MyApp.MyChannel.Supervisor,
      MyApp.Conn.Supervisor,
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
  end
end
```

素直に `MyApp.MyChannel.Supervisor` と `MyApp.Conn.Supervisor` を作って、その下にワーカーをぶら下げています。
子の指定で [`Supervisor.Spec`](https://hexdocs.pm/elixir/Supervisor.Spec.html) を使っていないのは、Elixir 1.5 で入った [合理化された child spec](https://qiita.com/melpon/items/bae4105c68f8da50e3c7) で *非推奨* になったからです。

ただ、こんな単純なスーパーバイザの為に新しいモジュールを作るのも面倒です。
何とかならないか考えたところ、以下のように書くことで解決しました。

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      %{
        id: MyApp.MyChannel.Supervisor,
        start: {
          Supervisor,
          :start_link,
          [
            [MyApp.MyChannel],
            [strategy: :one_for_one, name: MyApp.MyChannel.Supervisor]
          ]
        }
      },
      %{
        id: MyApp.Client.Supervisor,
        start: {
          Supervisor,
          :start_link,
          [
            [MyApp.Conn],
            [strategy: :simple_one_for_one, name: MyApp.Client.Supervisor]
          ]
        }
      },
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
  end
end
```

以下詳細です。

## なぜこれで動くのか

このコードは、さっきのコードと全く同じ内容でスーパーバイザツリーを構築します。
というのも、`Supervisor` モジュールがおおよそ以下のような実装になっているからです。

```elixir
defmodule Supervisor do
  ...

  def start_link(children, options) when is_list(children) do
    {sup_opts, start_opts} = Keyword.split(options, [:strategy, :max_seconds, :max_restarts])
    start_link(Supervisor.Default, {children, sup_opts}, start_opts)
  end
end

defmodule Supervisor.Default do
  def init({children, opts}) do
    Supervisor.init(children, opts)
  end
end
```

`Supervisor.start_link/2` に渡した値が、ほぼそのまま `Supervisor.Default.init/1` に渡るような実装になっていることが分かります。
そのため、`Supervisor.start_link/2` にうまく値を渡せば任意の子を作るスーパーバイザが作れます。

child spec の `:start` で、起動時に呼び出す MFA を指定するので、`%{start: {Supervisor, :start_link, [children, opts]}}` と書くことで `Supervisor.start_link/2` でスーパーバイザを起動できます。
`MyApp.MyChannel.Supervisor.init/1` では `Supervisor.init([MyApp.MyChannel], strategy: :one_for_one)` と書いていたので、`children` として `[MyApp.MyChannel]` を、`opts` として `[strategy: :one_for_one]` を渡せばいいことが分かります。
これを合わせると以下のようになります。

```elixir
%{
  start: {
    Supervisor,
    :start_link,
    [
      [MyApp.MyChannel],
      [strategy: :one_for_one]
    ]
  }
},
```

ただし `MyApp.MyCannel.Supervisor.start_link/1` で `:name` を指定していたことを忘れてはいけません。
先程の `Supervisor.start_link/2` の実装を見れば分かりますが、以下のように書くことで `Supervisor.start_link/3` の３引数目に `:name` が渡ります。

```elixir
%{
  start: {
    Supervisor,
    :start_link,
    [
      [MyApp.MyChannel],
      [strategy: :one_for_one, name: MyApp.MyChannel.Supervisor]
    ]
  }
},
```

これで動きそうに見えますが、child spec の ID[^1] は必須なので、これを指定する必要があります。
そのため `:id` も指定してやります。

[^1]: child spec の ID は、各プロセスを起動する時に `:name` で付ける名前とは別です。この ID は１つのスーパーバイザの中で一意であれば良くて、[`Supervisor.terminate_child/2`](https://hexdocs.pm/elixir/Supervisor.html#terminate_child/2) とかを呼び出す時に利用します。

```elixir
%{
  id: MyApp.MyChannel.Supervisor,
  start: {
    Supervisor,
    :start_link,
    [
      [MyApp.MyChannel],
      [strategy: :one_for_one, name: MyApp.MyChannel.Supervisor]
    ]
  }
},
```

これで無事スーパーバイザの下に `MyApp.MyChannel.Supervisor` をぶら下げることができました。
同様のことを `MyApp.Conn.Supervisor` にも適用すれば、最初に紹介したようなコードになります。

## まとめ

スーパーバイザの下にスーパーバイザをぶら下げるためにモジュールを作るのは結構面倒なのですが、この方法を使えば簡単に構築できます。
ただ、コードを見てもすぐには理解できなくなってしまうので、どうするかはよく考えた方がいいかもしれません。
