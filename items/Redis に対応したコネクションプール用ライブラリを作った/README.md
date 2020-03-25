自分の欲しい要件を満たしたコネクションプールのライブラリが無かったので、コネクションプール用ライブラリ [Connex](https://hex.pm/packages/connex) を作りました。[^1]

[^1]: お手伝い先の会社（株式会社gumi）の仕事として作ったので、gumi オーガナイゼーション下にあります。

## 使い方

Redis を利用する場合、`config :connex, Connex.Redis` にコネクションプーリングの設定を書きます。

```elixir:config/config.exs
config :connex, Connex.Redis,
  pools: [
    # <プール名>: {<poolboyの設定>, <Redix.start_link/2の引数>}
    default: {[size: 20], {[database: 0], []}},
    auth: {[size: 10], {[database: 1], []}},
  ],
  shards: []
```

[poolboy](https://github.com/devinus/poolboy) は Erlang/Elixir でよく使われているプロセスプールのライブラリで、[Redix](https://github.com/whatyouhide/redix) は Redis の Elixir クライアントライブラリです。

コメントに書いてあるように、`:pools` には `<プール名>: {<poolboyの設定>, <Redix.start_link/2の引数>}` という形式で書きます。
事前に起動しておくプロセスを20個にして、Redis の接続先を `localhost:6379` の 0 番データベースにするなら `{[size: 20], {[database: 0], []}}` と書きます。[^2]
このコネクションプールに `:default` という名前を付けています。

[^2]: Redix は [poolboy なんて使わず手動プール使えばいいよ](https://hexdocs.pm/redix/real-world-usage.html#pooling-strategies) とか言っているのですが、MULTI や EXEC なんかは同じコネクションで実行する必要があり、かつ MULTI/EXEC 間で他の意図しない命令が発行されたら困るので、その辺りを考えると checkout する仕組みは必要であるため poolboy を使うのが最適です。

`:shards` はシャーディングを行うために利用しますが、今は使わないので空リストにしておきます。

このアプリケーションが他のアプリケーションから利用される可能性があるなら、どのプールを利用するかというのを必ず設定ファイルから読むようにする必要があります。
プログラムで直接プール名を書いてはいけません。

```elixir
config :my_app,
  redis: :default
```

次に、アプリケーション起動時に、コネクションプール用のスーパーバイザを起動します。
child spec に `Connex.Redis.child_specs/0` を追加するだけです。

```elixir
  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      ...
    ]
    children = children ++ Connex.Redis.child_specs()

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
```

これで準備は完了です。あとは `Connex.Redis` を使って Redis にアクセスするだけです。

```elixir
# 設定ファイルからプール名を取得
pool_name = Application.fetch_env!(:my_app, :redis)

# 戻り値がどうなるかに関しては Redis のドキュメントを参照
"OK" = Connex.Redis.flushdb!(pool_name)
nil = Connex.Redis.get!(pool_name, "key")
# ! の無いバージョンを使うと {:ok, value} | {:error, reason} になる
{:ok, nil} == Connex.Redis.get(pool_name, "key")

"OK" = Connex.Redis.set!(pool_name, "key", "value")
# atom は文字列に変換される
"OK" = Connex.Redis.set!(pool_name, :key, :value)
"value" = Connex.Redis.get!(pool_name, "key")

# 必須の引数の後ろに追加の引数を追加できる
nil = Connex.Redis.set!(pool_name, "key", "value_nx", [:nx])
"value" = Connex.Redis.get!(pool_name, "key")
```

## 利点

このライブラリを使うことで何が嬉しいかというと、**複数のアプリケーションを同じ方法で設定できる** ことです。

Redis を利用したライブラリを複数利用する場合、通常はライブラリごとに異なる設定を書く必要があります。

```elixir
config lib1,
  redis: [host: "localhost", port: 6379, database: 0],
  pool: [size: 10]

config lib2,
  redis_host: "localhost",
  redis_port: 6379,
  redis_database: 1
```

ライブラリによってはプールに対応していない可能性もあります。ここでは lib2 がプールに対応していません。
また、場合によっては同じデータベースを利用したくなるかもしれません。
しかしこのまま同じデータベースを指定した場合、プールの設定がバラバラになるため、同じデータベースなのに片方のプールだけ詰まるといったことがあるかもしれません。

そこで Connex の出番です。もし lib1 と lib2 が Connex を導入すれば、以下のような設定になるでしょう。

```elixir
config :connex, Connex.Redis,
  pools: [
    default: {[size: 10], {[database: 0], []}},
  ],
  shards: []

config lib1,
  redis: :default

config lib2,
  redis: :default
```

lib1 と lib2 を利用するアプリケーションは、Connex の設定と、各ライブラリのプール名を設定するだけで済みます。
今回は lib1 と lib2 で同じデータベースを使うようにしました。
キーが被らない前提なら、このように１つのデータベースに纏めることもできます。

また、lib1 や lib2 は、接続に関することを考える必要が無くなります。
単純に `Application.fetch_env!(:lib1, :redis)` でプール名を取得し、`Connex.Redis.get!(pool_name, :key, :value)` のようにアクセスするだけです。
プールの起動や管理については、そのライブラリを利用するアプリケーションがやることになります。

## Connex.Redis について

基本的には、`Connex.Redis.get!/5` といった、各 Redis 命令と対応している関数を使えばいいだけです。
これらの関数は、プールからコネクションを１つ借りてきて、命令を実行し、コネクションを返す、ということをしています。

基本的にはこれらの関数だけ使っていれば問題ありません。
ただ、MULTI や EXEC といった命令は同じコネクションで実行する必要があるので、以下のように `Connex.Redis.run/2` を使います。

```elixir
Connex.Redis.run(:default, fn client ->
  assert "OK" == Connex.Redis.multi!(client)
  assert "QUEUED" == Connex.Redis.set!(client, "key", "value")
  assert "QUEUED" == Connex.Redis.set!(client, "key", "value2")
  assert "QUEUED" == Connex.Redis.get!(client, "key")
  assert ["OK", "OK", "value2"] == Connex.Redis.exec!(client)
end)
```

## シャーディング

Connex はシャーディングに対応しています。

```elixir
config :connex, Connex.Redis,
  pools: [
    pool1: {[size: 10], {[host: "endpoint1", database: 0], []}},
    pool2: {[size: 10], {[host: "endpoint2", database: 0], []}},
    pool3: {[size: 10], {[host: "endpoint3", database: 0], []}},
  ],
  shards: [
    myshard: [:pool1, :pool2, :pool3],
  ]
```

これは以下のように使います。

```elixir
# シャードキー "key" を使って :pool1, :pool2, :pool3 のどれかのプールを選択
# 同じシャードキーなら必ず同じプールが選択される
Connex.Redis.get!({:myshard, "key"}, "key")
```

## 他のライブラリへの対応

このライブラリはコネクションプール用のライブラリなので、Redis 以外のライブラリにも対応できます。
例えば Memcached あたりに対応できそうです。ただ、今のところ Redis しか必要としていないので、ほぼ Redis 専用ライブラリになっている状態です。

今後必要になれば実装していくと思います。
issue 報告や pull req お待ちしてます。
