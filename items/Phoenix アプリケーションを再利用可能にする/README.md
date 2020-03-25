[Phoenix Framework](http://phoenixframework.org/) を使って Web API や Web サイトを作る時、よく使うページやロジックを再利用可能な形で提供したいことがあります。
ここで言う再利用可能というのは、deps にアプリケーションを追加すると利用可能になるということを指します。
つまり、認証画面を表示したり、Admin サイトを表示したり、特定サービスの課金処理を行う API を提供したりといったことが、deps に追加していくつか設定するだけで動作して欲しいのです。[^1]

[^1]: Django で言う [`django.contrib.auth`](https://docs.djangoproject.com/en/1.11/ref/contrib/auth/) とか [`django.contrib.admin`](https://docs.djangoproject.com/en/1.11/ref/contrib/admin/) あたりを想定しています。

Phoenix でアプリケーションを作る場合は、それ単体で動作すること前提になっているので、いくつか弄ってあげる必要があります。
ここでは、どのようにすれば再利用可能になるのかを説明します。

## エンドポイントを削除する

Phoenix で再利用しにくい一番の理由は、それがアプリケーションとして起動し、指定したポートを Listen し始めるからです。
なのでそこを除ければ、かなり再利用可能に近づきます。

そのために、まずアプリケーションとして起動するのを止めます。

```elixir:mix.exs
  def application do
    [
      #この行を削除
      #mod: {MyApp.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end
```

これによって `lib/my_app/application.ex` が不要になるので削除します。

```bash
rm lib/my_app/application.ex
```

`MyApp.Application` では Repo の起動とエンドポイントの起動を行っていました。
これらの起動は依存先のアプリケーションに任せて、ここでは何も行わないようにしましょう。

Repo とエンドポイントを起動しないようにしたので、それらのモジュールも不要になります。削除しましょう。

```bash
rm lib/my_app/repo.ex
rm lib/my_app_web/endpoint.ex
```

なお、エンドポイントだけでなく Repo も削除する理由は、Repo をアプリケーション内で直接指定すると、別アプリケーションから再利用できなくなってしまうからです。

これで、`:my_app` は自分で起動することは無く、別のアプリケーションから呼ばれるだけの存在になりました。
`:my_app` のコントローラを利用するなら、このライブラリを利用するアプリケーションのルータから `MyApp.Router` に forward するだけです。

```elixir:lib/other_app_web/router.ex
defmodule OtherAppWeb.Router do
  use OtherAppWeb, :router

  ...

  # /my_app 以下の URL は全部 MyAppWeb.Router に処理してもらう
  forward "/my_app", MyAppWeb.Router
end
```

## スキーマは Yacto を利用する

スキーマとマイグレーションに関しては、Ecto は再利用をあまり考えてくれていないので、[Yacto](https://qiita.com/melpon/items/5c9b0645d5240cd22d0f) を使いましょう。

これによって、このライブラリで Repo を直接触る必要が無くなります。
Repo を定義したり、プロセスを起動したりするのは、このライブラリを利用するアプリケーション側がやるべきことです。

こうすることで、このライブラリを利用するアプリケーションでは、設定ファイルを適切に書いた上で Yacto のマイグレーションを実行するだけです。

```bash
mix yacto.migrate --app my_app
```

Yacto を使えば、複数の依存先アプリケーションのスキーマを全て同じデータベースに入れて利用可能です。

## テストを考慮する

このライブラリをテストする時、モジュール単体でテストするのは可能ですが、可能であればサーバとして起動してテストしたいところです。
そのためには、テスト時のみアプリケーションとして起動するように設定しておきます。
今まで消したファイルを `test/support/` の下に復活させてモジュール名を変えるだけです。

```elixir:mix.exs
  def application do
    mod =
      case Mix.env() do
        # テスト時のみアプリケーションとして起動する
        :test -> [mod: {MyApp.Test.Application, []}]
        _ -> []
      end
    mod ++ [
      extra_applications: [:logger, :runtime_tools]
    ]
  end
```

```elixir:test/support/application.ex
defmodule MyApp.Test.Application do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(MyApp.Test.Repo, []),
      supervisor(MyAppWeb.Test.Endpoint, []),
    ]

    opts = [strategy: :one_for_one, name: MyApp.Test.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

```elixir:test/support/repo.ex
defmodule MyApp.Test.Repo do
  use Ecto.Repo, otp_app: :my_app

  def init(_, opts) do
    {:ok, Keyword.put(opts, :url, System.get_env("DATABASE_URL"))}
  end
end
```

```elixir:test/support/endpoint.ex
defmodule MyAppWeb.Test.Endpoint do
  ...

  plug MyAppWeb.Router

  ...
end
```

テスト用のアプリケーション、テスト用のエンドポイント、テスト用の Repo を作っています。
これらのファイルはテスト時にしか利用せず、実際はこのライブラリを利用するアプリケーションが用意するものです。

Repo やエンドポイントの名前を変更したので、`config/test.exs` の設定もそれに追従します。

```elixir:config/test.exs
use Mix.Config

config :my_app, MyAppWeb.Test.Endpoint,
  http: [port: 4001],
  server: true

config :my_app,
  ecto_repos: [MyApp.Test.Repo]

config :my_app, MyApp.Test.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "my_app_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox
```

あと `MyApp.Repo` を使っている部分がいくつかあるので `MyApp.Test.Repo` を使うように変えておきましょう。
それが終われば、ちゃんとテスト時にサーバを起動してテストできるようになっているはずです。[^2]

[^2]: なお、Phoenix は実際にサーバを起動しなくてもエンドポイント経由で実行してテストすることは可能です。が、end-to-end のテストの方がより正確なので、自分はそちらを積極的に使うようにしています。

これでデータベース用のスキーマも無事利用可能です。

## まとめ

- エンドポイントあたりを消せば大体再利用可能になる
- スキーマに関しては Yacto を使おう
