Phoenix では、様々な場所で [Plug](https://hexdocs.pm/plug/readme.html) ライブラリの仕組みが使われています。
この仕組みを知っておくことで、Phoenix を理解しやすくなると思います。

## 伝えたいこと

書いていくうちに雑多になってきたので、まとめだけ書いておきます。

- エンドポイント、ルータ、コントローラは、Plug から見ればどれも `init/1` と `call/2` が実行されただけのミドルウェアである。
- Phoenix はそれらに意味を持たせて、エンドポイント、ルータ、コントローラという名前を付けているだけ。

## Plug ライブラリについて

Plug ライブラリは、HTTP のリクエストを受けて返すだけ、単純な Web フレームワークの１つです。
Plug は簡単にミドルウェアを作って組み合わせられるような仕組みになっているのが特徴で、Phoenix はこの機能を便利に使っています。

Plug のミドルウェアを作るには、２種類あります。
`fun(Plug.Conn.t, any) :: Plug.Conn.t` な関数を作るか、`@callback init(any) :: any` と `@callback call(Plug.Conn.t, any) :: Plug.Conn.t` というビヘイビアを定義したモジュールを作るかです。

例えば以下の様に書きます。

```elixir
defmodule Foo do
  defmacro __using__(_) do
    quote do
      # 関数 Plug
      def foo(conn, opts) do
        IO.puts "foo plug"
        conn
      end
    end
  end
end

# モジュール Plug
defmodule Bar do
  def init(opts) do
    opts
  end
  def call(conn, opts) do
    IO.puts "Bar plug"
    conn
  end
end
```

このミドルウェアを作っておくと、以下の様に書くことができます。

```elixir
defmodule Mod do
  use Plug.Builder
  use Foo

  plug :foo
  plug Bar
end
```

`plug` マクロによって、ミドルウェアを組み合わせています。
また、`use Plug.Builder` によって、このモジュールに `init/1` と `call/2` が展開されます。
そのため `Mod.foo/2` 関数や `Bar` モジュールだけでなく、`Mod` モジュールもミドルウェアになります。

この時、このコードは大体以下の様に展開されます。

```elixir
defmodule Mod do
  # use Plug.Builder
  def init(opts) do
    opts
  end

  def call(conn, opts) do
    plug_builder_call(conn, opts)
  end

  defoverridable [init: 1, call: 2]

  # use Foo
  def foo(conn, opts) do
    IO.puts "foo plug"
    conn
  end

  # Plug.Builder.__before_compile__/1
  defp plug_builder_call(conn, _) do
    # 各plugの呼び出し。実際はもうちょい複雑。
    conn
    |> foo([])
    |> Bar.call([])
  end
end
```

見ての通り、`plug :foo` や `plug Bar` のマクロから、`conn |> foo([]) |> Bar.call([])` という感じのコードを生成するようになっていて、これによってミドルウェア呼び出しを連鎖させることができるようになっています。[^1]

[^1]: 実際は実行条件のチェックや例外が起きたときの処理や途中で中断した際の処理とかが入ってるのでもうちょい複雑。

このミドルウェア呼び出しの連鎖をするコードを生成するのが `Plug.Builder.compile/3` です。[^2]

[^2]: `init/1` は `Plug.Builder.compile/3` 実行時、つまりコンパイル時に呼ばれるようになっていて、`unquote(body)` 時には `call/2` しか残りません。

```elixir
# :foo と Bar からミドルウェア呼び出しのコードに変換
{conn, body} = Plug.Builder.compile(__ENV__, [{:foo, [], true}, {Bar, [], true}], [])

...

# これで conn |> foo([]) |> Bar.call([]) のようなコードに展開される
unquote(body)
```

`use Plug.Builder` をした場合には、`plug` によって集められた情報を `Plug.Builder.compile/3` でコードを生成し、`plug_builder_call/2` 内にそのコードを配置するようになっています。

## Phoenix の Plug

Phoenix では、エンドポイント、ルータ、コントローラの中で `plug` が使えるようになっています。
また、エンドポイントやルータそのものもミドルウェアになっていて、エンドポイントの `plug` の一部にルータが、ルータの `plug` の一部にコントローラが含まれています。
つまりミドルウェア呼び出しはネストするようになっています。

例えば、

```elixir
defmodule HelloWorld.Endpoint do
  use Phoenix.Endpoint, otp_app: :hello_world

  plug Plug.Static
  plug Plug.RequestId
  plug Plug.Logger

  ...

  plug HelloWorld.Router
end

defmodule HelloWorld.Router do
  use Phoenix.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    ...
  end

  scope "/", HelloWorld do
    pipe_through :browser

    get "/", PageController, :index
  end
end

defmodule HelloWorld.PageController do
  use Phoenix.Controller

  plug :authentication
  plug :admin_required
  ...

  def index(conn, _params) do
    render conn, "index.html"
  end
end
```

というコードがある時、`HelloWorld.Endpoint.call/2` を呼び出した場合のコールグラフは、以下の様になります。

- `HelloWorld.Endpoint.call/2`
    - `Plug.Static.call/2`
    - `Plug.RequestId.call/2`
    - `Plug.Logger.call/2`
    - ...
    - `HelloWorld.Router.call/2`
        - `accepts/2`
        - `fetch_session/2`
        - `fetch_flash/2`
        - ...
        - `HelloWorld.PageController.call/2`
            - `accepts/2`
            - `HelloWorld.MyAuth.call/2`
            - `action/2`
                - `index/2`

まず、エンドポイントの `call/2` が実行されます。
次に、そのエンドポイントに書かれた `plug` を次々と実行し、最後の `plug` としてルータが実行されます。
次に、そのルータに書かれた `plug` を次々と実行し、最後の `plug` としてコントローラが実行されます。
次に、そのコントローラに書かれた `plug` を次々と実行して、最後に `action/2` を呼び、これによって自分のハンドルしたい関数 `index/2` が呼ばれることになります。

## `plug` の使い分け

エンドポイントに `plug` を追加すると、そのエンドポイントに対する全てのリクエストに対してそのミドルウェアが実行されます。

ルータ（のパイプライン）に `plug` を追加すると、特定の URL の下にある場合だけそのミドルウェアが実行されるようになります。

コントローラに `plug` を追加すると、そのコントローラを実行する場合だけそのミドルウェアが実行されるようになります。
また、コントローラの特定のアクション（`:index` とか `:show` とか）だけ実行されるようにしたいなら `when` を使ってガード条件が書けます。
[Phoenixのコントローラ#plug](http://qiita.com/melpon/items/9530c52ab1350acb4409#plug) を参照。

## `plug` の実体

エンドポイント、ルータ、コントローラでそれぞれ単に `plug` と書いてミドルウェアを追加してますが、実のところ、これらの `plug` マクロはそれぞれ違う実装を使っています。

- エンドポイント: `Plug.Builder.plug`
- ルータ: `Phoenix.Router.plug`
- コントローラ: `Phoenix.Controller.Pipeline.plug`

エンドポイントはオリジナルの `plug` を使っていますが、それ以外は独自の実装になっています。
ただ、最終的には全部 `Plug.Builder.compile/3` を呼び出してコードを生成しているので、一応 Plug を使っているとは言える。
