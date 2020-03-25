Plug について説明します。

## Plug とは

Elixir の HTTP サーバの実装の１つです。
内部では Erlang の信頼できる HTTP サーバ [Cowboy](https://ninenines.eu/) を利用してますが、アダプターを切り替えることで他のライブラリにもできます。[^1]

[^1]: ただし標準では Cowboy にしか対応してないので、他の HTTP サーバに切り替えたいなら自前で実装する必要があります。

2017年12月19日現在、Cowboy 2 に対応した 1.5.0-rc.0 が出ています。
が、rc.0 なので、ちゃんとした 1.5.0 がリリースされるまでは 1.4.3 を使っておくのが無難でしょう。

## Phoenix との違い

Phoenix は Plug を利用して作っています。
Phoenix に渡ってくる `conn` は `Plug.Conn` なので、Phoenix を弄るには Plug の知識が必要になってきます。
また、Phoenix はプラグのインターフェースである `init/1` と `call/2` をうまく扱えるようにしているので、`Plug` ライブラリに定義された各モジュールプラグをそのまま利用できるようになっています。

ただし、`Plug.Router` のルーティングの仕組みは利用していません。
そのため Phoenix のルーティングは Plug の `get "/foo/:bar", do: ...` や `forward ...` みたいな書き方とは全く関係がありません。

## Plug の仕組み

Plug の仕組みは超簡単です。
まず、お約束どおり以下の様に書けば、8000 番ポートで HTTP のリクエストを待ち受けるようになります。

```elixir:application.ex
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      Plug.Adapters.Cowboy.child_spec(:http, MyApp.Router, [], [port: 8000]),
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

`MyApp.Router` という指定が重要で、このモジュールの `call/2` 関数が全ての HTTP リクエストの起点になります。
つまり、全ての HTTP リクエストはまず `MyApp.Router.call(conn, opts)` に渡されることになります。

これが分かっていれば、以下の様なコードを書くだけで HTTP サーバが作れます。

```elixir
defmodule MyApp.Router do
  # init/1 も定義する必要がある
  def init(opts), do: opts

  def call(conn, _opts) do
    case conn.path_info do
      [] -> Plug.Conn.send_resp(conn, 200, "hello")
      [name] -> Plug.Conn.send_resp(conn, 200, "hello, #{name}!")
      _ -> Plug.Conn.send_resp(conn, 404, "")
    end
  end
end
```

```bash
$ curl http://localhost:8000/
hello
$ curl http://localhost:8000/foo
hello, foo!
```

つまり Plug がやっているのは、

1. HTTP リクエストを受けとるためにポートを開いて待ち受ける
2. リクエストを受けたら、そのリクエストをパースして `Plug.Conn` 構造体を作る
3. 指定されたモジュールの `call/2` 関数を呼ぶ

だけです。

## プラグ構築周りのモジュール

もちろん、これだけだとまともにルーティングすらできないので、Plug は便利な機能を用意しています。

ここでは [Plug.Builder](https://hexdocs.pm/plug/Plug.Builder.html) と [Plug.Router](https://hexdocs.pm/plug/Plug.Router.html) を紹介します。

### Plug.Builder

複数のプラグを連結するためのモジュールです。

通常、HTTP リクエストを処理する時には、薄いミドルウェアを何層にも重ねて処理することが多くなっています。
`MyApp.Router.call/2` で静的ファイルの配信、セッションのロード、CSRF トークンのチェックを行って処理する場合、以下の様になるでしょう。

```elixir
def call(conn, _opts) do
  # 静的ファイルのリクエストだったら配信する
  case serve_static_files(conn) do
    {:handled, conn} ->
      # 配信したので終了
      conn
    {:not_handled, conn} ->
      # 静的ファイルのリクエストでなかったので次の処理
      # セッションをロードする
      conn = load_session(conn)
      # CSRF トークンのチェック
      case verify_csrf(conn) do
        {:ok, conn} ->
          # CSRF トークンが正しかったのでメインの処理を行う
          ...
        {:error, conn} ->
          # CSRF トークンが間違っていたのでエラーにする
          Plug.Conn.send_resp(conn, 400, "")
      end
  end
end
```

こんなのは書いていられないので、共通のインターフェースを用意して、それを満たすことでこれらを簡単に呼び出せるようにしています。

この共通のインターフェースを満たしたモジュールを、ここでは *プラグ* と呼びます。
プラグには *モジュールプラグ* と *関数プラグ* の2種類があります。

モジュールプラグは `init/1` と `call/2` 関数を実装した、つまり Plug ビヘイビアを実装したモジュールです。
関数プラグは、自身のモジュールにある、`conn` と `opts` を受け取り、新しい `conn` を返す関数の名前です。

`Plug.Builder` と標準のプラグを利用すると、上記のコードは以下の様になります。

```elixir
defmodule MyApp.Router do
  use Plug.Builder

  # 静的ファイルの配信
  plug Plug.Static, from: "priv/static"
  # セッションのロード
  plug Plug.Session, store: :ets, key: "_my_app_session", table: :session
  # CSRF トークンのチェック
  plug Plug.CSRFProtection

  def call(conn, opts) do
    conn = super(conn, opts)
    # 上記のチェックを抜けた後の処理
  end
end
```

とても簡単です。
このコードは、`use Plug.Builder` によって以下の様なコードになっています。

```elixir
defmodule MyApp.Router do
  def call(conn, opts) do
    try do
      # 静的ファイルの配信
      conn = Plug.Static.call(conn, Plug.Static.init(from: "priv/static"))
      # 中断するかどうかのチェック
      if conn.halted, do: throw conn

      # セッションのロード
      conn = Plug.Session.call(conn, Plug.Session.init(store: :ets, key: "_my_app_session", table: :session))
      # 中断するかどうかのチェック
      if conn.halted, do: throw conn

      # CSRF トークンのチェック
      conn = Plug.CSRFProtection.call(conn, Plug.CSRFProtection.init())
      # 中断するかどうかのチェック
      if conn.halted, do: throw conn

      conn
    else
      conn ->
        # 上記のチェックを抜けた後の処理
        ...
    catch
      conn -> conn
    end
  end
end
```

`use Plug.Builder` を使うことで、それぞれの `plug ...` を連結して上記のように呼び出すように実装してくれます。[^init]
ここで使っているプラグは、全てモジュールプラグになります。

[^init]: 正確には `init/1` はリクエストが来る度に呼ぶわけではなく、コンパイル時に一回呼ぶだけです。あとはその結果を毎回 `call/2` の第二引数に入れています。また、`throw/1` はソースコードの見通しを良くするために使っているだけで、実際に `throw/1` が使われている訳ではありません。

また、`use Plug.Builder` を使うと、そのモジュール内で `init/1` と `call/2` を自動的に定義してくれます。
そのため `use Plug.Builder` を呼び出したモジュールもプラグになります。

### Plug.Router

`use Plug.Router` は、`use Plug.Builder` することに加えて、`match/2` 関数と `dispatch/2` 関数を定義します。

Plug では URL 別のルーティングもプラグの一部分として定義されていて、`match/2` と `dispatch/2` はどちらも関数プラグになっています。

`Plug.Router.get/3` や `Plug.Router.post/3` で定義したルーティングに対して、どれを呼び出すかを決定するのが `:match` プラグ、実際に呼び出すのが `:dispatch` プラグになります。

```elixir
defmodule MyApp.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    Plug.Conn.send_resp(conn, 200, "hello")
  end
  get "/:name" do
    Plug.Conn.send_resp(conn, 200, "hello, #{name}!")
  end
end
```

```bash
$ curl http://localhost:8000/
hello
$ curl http://localhost:8000/foo
hello, foo!
```

このように書くことで、最初に書いた `MyApp.Router` の実装とほぼ同じになります。違いは、こちらの実装は `GET` リクエストしか処理しないことぐらいでしょう。[^macro-must-go]

[^macro-must-go]: `get/3` の中で、どこからともなく `conn` 変数が使えていますが、これがマクロの力です。滅びて欲しい。

この動作を見て分かるように、`use Plug.Router` によって自動的に定義された `match/2` や `dispatch/2` 関数は、`Plug.Router.get/3` によって定義されたルーティング情報を見て、適切に分岐して呼び出す実装になっています。

`:dispatch` の部分を改造すれば、全てのリクエストに対する共通処理が書けます。

```elixir
defmodule MyApp.Router do
  use Plug.Router

  plug :match
  plug :my_dispatch

  get "/" do
    Plug.Conn.send_resp(conn, 200, "hello")
  end
  get "/:name" do
    Plug.Conn.send_resp(conn, 200, "hello, #{name}!")
  end

  def my_dispatch(conn, opts) do
    # 共通の処理
    do_something()
    # ルーティング別の処理
    dispatch(conn, opts)
  end
end
```

自作の関数プラグ `:my_dispatch` を定義し、そこの中で共通処理を実装し、その後で元の `:dispatch` プラグを呼び出しています。
このように、`plug ...` を適切に書くことで、いろいろと共通処理をうまく分離できるようになります。

### forward を活用する

ある URL 以下だけ特定のプラグを適用したいこともあります。
その場合には `Plug.Router.forward/2` を使うのがいいでしょう。

```elixir:my_app/router.ex
defmodule MyApp.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    ...
  end

  # /api 以下は別のルータに任せる
  forward "/api", to: MyApp.Router.API
end
```

```elixir:my_app/router/api.ex
defmodule MyApp.Router.API do
  use Plug.Router

  # JSON のパースを行う
  plug Plug.Parsers, parsers: [:json]

  plug :match
  plug :dispatch

  post "/" do
    # JSON のリクエストボディが conn.body_params に入っている
    conn.body_params["name"]
  end
end
```

`/api` 以下の URL を、全て `MyApp.Router.API` に転送しています。
`MyApp.Router.API` でモジュールプラグ `Plug.Parsers` を使うことで、`/api` 以下だけ JSON をパースする処理にしています。

あるいは、URL だけでなく、中のデータを見て、特定の条件だった場合だけ転送したいとか、URL を弄って転送したいという場合には `Plug.Router.Utils.forward/4` を使えば、好きなタイミングで転送できます。

このように Plug では、やろうと思えば結構柔軟な処理ができます。

## Plug 標準のモジュールプラグ

Plug が標準で用意しているモジュールプラグについて説明します。
現在は、全部で以下のモジュールプラグが用意されています。

- [Plug.CSRFProtection](https://hexdocs.pm/plug/Plug.CSRFProtection.html)
- [Plug.Head](https://hexdocs.pm/plug/Plug.Head.html)
- [Plug.Logger](https://hexdocs.pm/plug/Plug.Logger.html)
- [Plug.MethodOverride](https://hexdocs.pm/plug/Plug.MethodOverride.html)
- [Plug.Parsers](https://hexdocs.pm/plug/Plug.Parsers.html)
- [Plug.RequestId](https://hexdocs.pm/plug/Plug.RequestId.html)
- [Plug.SSL](https://hexdocs.pm/plug/Plug.SSL.html)
- [Plug.Session](https://hexdocs.pm/plug/Plug.Session.html)
- [Plug.Static](https://hexdocs.pm/plug/Plug.Static.html)

### Plug.CSRFProtection

CSRF を防ぐためのモジュールプラグです。
セッション情報が必要になってくるので、通常は `Plug.Session` と一緒に利用します。

クライアントに HTML を返す時に `Plug.CSRFProtection.get_csrf_token/0` を使って CSRF トークンを設定しておくと、このモジュールがその情報を自動でセッションに入れておいてくれます。
そして、次のリクエストではクライアントに `"_csrf_token"` パラメータか `"x-csrf-token"` リクエストヘッダに CSRF トークンを入れてもらうようにし、このプラグでは、その CSRF トークンが、直前でセッションに保存した CSRF トークンと同じかどうかを判断するようになっています。

### Plug.Head

HEAD リクエストを GET リクエストに変換します。ほんとにそれだけです。

### Plug.Logger

リクエスト毎にログとステータスと処理時間を出してくれます。
ログレベルは `opts` で調整できます。

### Plug.MethodOverride

`Plug.Parsers` あたりでリクエストボディを処理した結果、`conn.body_params["_method"]` に `"DELETE"`, `"PUT"`, `"PATCH"` が設定されてたら、そのメソッドに置き換えるモジュールプラグです。

HTML のフォームが GET と POST しかサポートしていないので、それ以外のメソッドを使えるようにするための機能のようです。

### Plug.Parsers

リクエストボディをパースするためのモジュールプラグ。
標準では `:json` を指定すると `"application/json"` ヘッダ、`:urlencoded` を指定すると `"application/x-www-form-urlencoded"` ヘッダ、`:multipart` を指定すると `"multipart/form-data"` か `"multipart/mixed"` ヘッダの付いたボディをパースしてくれます。
結果は `conn.body_params` と `conn.params` に設定されます。

これらのパーサは自分で追加することも可能です。詳細はドキュメントで。

### Plug.RequestId

リクエスト毎にリクエストIDを発行して、自動でレスポンスヘッダに付けてくれるプラグです。
Loggerのメタデータに `:request_id` を設定してくれるので、Loggerでそれを出力するようにしておけば、リクエスト毎の処理を追いやすくなります。

また、HTTP リクエストヘッダに `"x-request-id"` が入っていると、それをリクエストIDとして利用します。
クライアントも含めた一連の流れを追いたい場合に使えます。

### Plug.SSL

HTTP リクエストのスキームが `https` じゃなかった場合にリダイレクトすることで SSL での接続を強制するプラグです。

Nginx の後ろなんかにある場合は全て http になってしまうので、`opts` に `rewrite_on: [:x_forwarded_proto]` を指定することで、Nginx が自動で付ける `"x-forwarded-proto"` ヘッダを見て http か https かを判断してくれるようになります。

### Plug.Session

セッション情報を取得したり格納したりするプラグです。

ユーザはこのプラグを追加した後、`Plug.Conn.get_session/2` や `Plug.Conn.put_session/3` あたりを使って取得・格納したりします。

セッションを格納する方法として、標準では ETS を使う `Plug.Session.ETS` とクッキーを使う `Plug.Session.COOKIE` の2種類を用意しています。

### Plug.Static

静的ファイルを配信するためのプラグです。

特定のリクエストパス以下の場合だけ配信する `:at` や、どこのディレクトリを起点にするかという `:from` を指定して使います。

キャッシュシステムがあったり、gzip 圧縮されたファイルを配信したり、配信するファイルやディレクトリを制限したりといった機能があって、結構いろいろできるようです。

## まとめ

マクロに惑わされず、心の目で `init/1` と `call/2` を見ましょう。
そうすれば Plug が凄く簡単に見えてきます。
