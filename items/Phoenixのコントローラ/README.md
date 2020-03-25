Phoenixのコントローラが、どのように呼ばれているのかが気になったので調べた。
[Phoenixのルーティング](http://qiita.com/melpon/items/b9a712e784e7b9919ce6) と同じ環境で試している。

前回では、ルーティングによって最終的に `HelloWorld.PageController` の `init/1` と `call/2` が呼ばれることが分かった。
ここから `HelloWorld.PageController.index/2` が呼ばれるところまで見ていく。

## HelloWorld.PageController

`hello_world/web/controllers/page_controller.ex`:

```elixir
defmodule HelloWorld.PageController do
  use HelloWorld.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
```

`use HelloWorld.Web, :controller` によって、`HelloWorld.Web.__using__/1` が展開される。

```elixir
defmodule HelloWorld.Web do
  ...

  def controller do
    quote do
      use Phoenix.Controller

      alias HelloWorld.Repo
      import Ecto
      import Ecto.Query

      import HelloWorld.Router.Helpers
      import HelloWorld.Gettext
    end
  end

  ...

  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
```

`apply(__MODULE__, which, [])` によって `controller/0` が呼び出され、この内容が `HelloWorld.PageController` 内に展開される。
その中で使っている `use Phoenix.Controller` によって、更にこれが展開される。

`phoenix/lib/phoenix/controller.ex`:

```elixir
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      import Phoenix.Controller

      # TODO v2: No longer automatically import dependencies
      import Plug.Conn

      use Phoenix.Controller.Pipeline, opts

      plug :put_new_layout, {Phoenix.Controller.__layout__(__MODULE__, opts), :app}
      plug :put_new_view, Phoenix.Controller.__view__(__MODULE__)
    end
  end
```

更に `use Phoenix.Controller.Pipeline, opts` が展開される。

`phoenix/lib/phoenix/controller/pipeline.ex`:

```elixir
  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @behaviour Plug

      require Phoenix.Endpoint
      import Phoenix.Controller.Pipeline

      Module.register_attribute(__MODULE__, :plugs, accumulate: true)
      @before_compile Phoenix.Controller.Pipeline
      @phoenix_log_level Keyword.get(opts, :log, :debug)
      @phoenix_fallback :unregistered

      @doc false
      def init(opts), do: opts

      @doc false
      def call(conn, action) when is_atom(action) do
        conn = update_in conn.private,
                 &(&1 |> Map.put(:phoenix_controller, __MODULE__)
                      |> Map.put(:phoenix_action, action))

        Phoenix.Endpoint.instrument conn, :phoenix_controller_call,
          %{conn: conn, log_level: @phoenix_log_level}, fn ->
          phoenix_controller_pipeline(conn, action)
        end
      end

      @doc false
      def action(%Plug.Conn{private: %{phoenix_action: action}} = conn, _options) do
        apply(__MODULE__, action, [conn, conn.params])
      end

      defoverridable [init: 1, call: 2, action: 2]
    end
  end
```

ここに `init/1` と `call/2` が定義されていた。
この定義によって、`HelloWorld.PageController` は `HelloWorld.Router` からの `init/1` と `call/2` 呼び出しを受けることができるようになっている。
また、`defoverridable` になっているので、`init/1` と `call/2` を自分で定義すれば、これらの関数をオーバーライドすることもできる。

`call/2` では、`conn.private` に値を入れて、`Phoenix.Endpoint.instrument/4` の中で `phoenix_controller_pipeline/2` を呼び出しているだけである。

`Phoenix.Endpoint.instrument` は時間計測するためのマクロで、[Phoenix.Endpoint](https://hexdocs.pm/phoenix/Phoenix.Endpoint.html) の Instrumentation に詳細が書かれてる。

`phoenix_controller_pipeline/2` は `__before_compile__/1` で定義される関数で、以下の様になっている。

`phoenix/lib/phoenix/controller/pipeline.ex`:

```elixir
  @doc false
  defmacro __before_compile__(env) do
    action = {:action, [], true}
    plugs  = [action|Module.get_attribute(env.module, :plugs)]
    {conn, body} = Plug.Builder.compile(env, plugs, log_on_halt: :debug)
    fallback_ast =
      env.module
      |> Module.get_attribute(:phoenix_fallback)
      |> build_fallback()

    quote do
      defoverridable [action: 2]
      def action(var!(conn_before), opts) do
        try do
          var!(conn_after) = super(var!(conn_before), opts)
          unquote(fallback_ast)
        catch
          :error, reason ->
            Phoenix.Controller.Pipeline.__catch__(
              var!(conn_before), reason, __MODULE__,
              var!(conn_before).private.phoenix_action, System.stacktrace()
            )
        end
      end

      defp phoenix_controller_pipeline(unquote(conn), var!(action)) do
        var!(conn) = unquote(conn)
        var!(controller) = __MODULE__
        _ = var!(conn)
        _ = var!(controller)
        _ = var!(action)

        unquote(body)
      end
    end
  end
```

`plugs  = [action|Module.get_attribute(env.module, :plugs)]` によって、各種 `plug` を実行した後に `action/2` を呼び出すように設定している。
これは最終的に `unquote(body)` によって、各種 `plug` の呼び出しと `action/2` の呼び出しが行われる。

この呼び出しで、`__before_compile__/1` で定義された `action/2` 関数が呼ばれ、`super(var!(conn_before), opts)` によって、`__using__/1` で定義された `action/2` 関数が呼ばれることになる。
`__using__/1` で定義された `action/2` 関数で `apply(__MODULE__, action, [conn, conn.params])` と呼び出しているため、これによって無事 `HelloWorld.PageController.index/2` が呼ばれることになる。

つまり `HelloWorld.PageController` は、必要な部分を抜き出して展開されると大体以下の様になる。

```elixir
defmodule HelloWorld.PageController do
  # Phoenix.Controller.__using__/1
  def init(opts), do: opts

  def call(conn, action) when is_atom(action) do
    conn = update_in conn.private,
             &(&1 |> Map.put(:phoenix_controller, __MODULE__)
                  |> Map.put(:phoenix_action, action))

    phoenix_controller_pipeline(conn, action)
  end

  def action(%Plug.Conn{private: %{phoenix_action: action}} = conn, _options) do
    apply(__MODULE__, action, [conn, conn.params])
  end

  defoverridable [init: 1, call: 2, action: 2]

  # HelloWorld.PageController
  def index(conn, _params) do
    render conn, "index.html"
  end

  # Phoenix.Controller.__before_compile__/1
  defoverridable [action: 2]

  def action(conn_before, opts) do
    try do
      conn_after = super(conn_before, opts)
      # ここにaction_fallbackの処理が入る
      conn_after
    catch
      ...
    end
  end

  defp phoenix_controller_pipeline(conn, action) do
    controller = HelloWorld.PageController
    # ここに各種plugの処理が入る
    action(conn, [])
  end
end
```

1. `call/2`
2. `phoenix_controller_pipeline/2`
3. `__before_compile__/1` 側の `action/2`
4. `__using__/1` 側の `action/2`
5. `index/2`

と呼ばれることが分かる。

## `action/2` のオーバーライド

`action/2` はオーバーライド可能である。

```elixir
defmodule HelloWorld.PageController do
  use HelloWorld.Web, :controller

  def action(conn, opts) do
    IO.puts "overridden action/2"
    super(conn, opts)
  end

  def index(conn, _params) do
    render conn, "index.html"
  end
end
```

こうした時、呼び出される順序は以下のようになる。

1. `call/2`
2. `phoenix_controller_pipeline/2`
3. `__before_compile__/1` 側の `action/2`
4. `HelloWorld.PageController` で定義した `action/2`
5. `__using__/1` 側の `action/2`
6. `index/2`

このように、「`__before_compile__/1` 側の `action/2`」と「`__using__/1` 側の `action/2`」の間で呼ばれることになる。

これは `defmodule` の一番下に `__before_compile__/1` の内容が展開されるため、「`__before_compile__/1` 側の `action/2`」が 「`HelloWorld.PageController` で定義した `action/2`」よりも後に定義されることになるからである。

つまり、ここでやっている `action/2` のオーバーライドというのは、「`__using__/1` 側の `action/2`」のオーバーライドだということを覚えておいたほうが良い。

また、外から `HelloWorld.PageController.action/2` を呼び出した場合、「`__before_compile__/1` 側の `action/2`」が呼び出されるということも覚えておいた方が良い。

もし「`__before_compile__/1` 側の `action/2`」をオーバーライドしたい場合には、以下の様に定義すれば良さそうだ。

```elixir
defmodule HelloWorld.PageController.Action do
  defmacro __before_compile__(env) do
    quote do
      defoverridable [action: 2]
      def action(conn, opts) do
        # 好きなように書く
        ...
      end
    end
  end
end

defmodule HelloWorld.PageController do
  use HelloWorld.Web, :controller
  # use より後に書くこと
  @before_compile HelloWorld.PageController.Action

  def index(conn, _params) do
    render conn, "index.html"
  end
end
```

## plug

v1.3.0 以降では、コントローラの `plug/2` には、ガード条件を付けることができる。

```elixir

defmodule HelloWorld.PageController do
  # Phoenix のドキュメントから引用
  plug :authenticate, [usernames: ["jose", "eric", "sonny"]] when action in [:show, :edit]
  plug :authenticate, [usernames: ["admin"]] when not action in [:index]
end
```

ガードの中では `conn`, `controller`, `action` の変数が利用可能で、`controller` には `HelloWorld.PageController` といったコントローラのモジュール名が、`action` には `:index` や `:edit` といったアクション名が入っている。

今のところ、ガード条件が使えるのはコントローラの `plug/2` だけであり、エンドポイントの `plug/2` とルータの `plug/2` では使えない。

## action_fallback

v1.3.0 以降では、`with` と組み合わせてうまくコントローラを書けるようになっている。

通常、コントローラの戻り値は `%Plug.Conn{}` にする必要があるが、実はそれ以外を返しても構わないようになっている。
もし `%Plug.Conn{}` 以外を返した場合、`Phoenix.Controller.action_fallback/1` で指定した関数やモジュールが呼ばれることになる。

```elixir
# Phoenixドキュメントから引用

defmodule MyController do
  use Phoenix.Controller

  action_fallback MyFallbackController

  def show(conn, %{"id" => id}, current_user) do
    with {:ok, post} <- Blog.fetch_post(post),
         :ok <- Authorizer.authorize(current_user, :view, post) do

      render(conn, "show.json", post: post)
    end
  end
end

defmodule MyFallbackController do
  use Phoenix.Controller

  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> render(MyErrorView, :"404")
  end

  def call(conn, {:error, :unauthorized}) do
    conn
    |> put_status(403)
    |> render(MyErrorView, :"403")
  end
end
```
