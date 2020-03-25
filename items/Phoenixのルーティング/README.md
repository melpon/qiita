Phoenix で、受け取った URL をどうやってディスパッチしているのか気になったので、`mix phoenix.new hello_world` で生成したプロジェクトから、順番に読んでいった。

## HelloWorld.start/2

`hello_world/lib/hello_world.ex`:

```elixir
defmodule HelloWorld do
  ...
  def start(_type, _args) do
    ...
    children = [
      # Start the Ecto repository
      supervisor(HelloWorld.Repo, []),
      # Start the endpoint when the application starts
      supervisor(HelloWorld.Endpoint, []),
    ]
    opts = [strategy: :one_for_one, name: HelloWorld.Supervisor]
    Supervisor.start_link(children, opts)
  end
  ...
end
```

`HelloWorld.Repo` と `HelloWorld.Endpoint` をスーパーバイザとして起動してる。
エンドポイントは、設定を書いてここに supervisor を追加すれば何個でも追加できるっぽい。

## HelloWorld.Endpoint

`HelloWorld.Endpoint.start_link/0` は単純に `Phoenix.Endpoint.Supervisor.start_link/2` を呼んでるだけで、`Phoenix.Endpoint.Supervisor` は以下の様になっている。

`phoenix/lib/phoenix/endpoint/supervisor.ex`:

```elixir
  def start_link(otp_app, mod) do
    case Supervisor.start_link(__MODULE__, {otp_app, mod}, name: mod) do
      {:ok, _} = ok ->
        warmup(mod)
        ok
      {:error, _} = error ->
        error
    end
  end

  ...

  def init({otp_app, mod}) do
    id = :crypto.strong_rand_bytes(16) |> Base.encode64
    conf = [endpoint_id: id] ++ config(otp_app, mod)

    conf =
      case Keyword.fetch(conf, :on_init) do
        {:ok, {mod, fun, args}} ->
          {:ok, conf} = apply(mod, fun, [conf | args])
          conf
        {:ok, other} ->
          raise ArgumentError, "invalid :on_init option for #{inspect mod}. " <>
                               "Expected a tuple with module, function and args, got: #{inspect other}"
        :error ->
          conf
      end

    server? = server?(conf)

    if server? and conf[:code_reloader] do
      Phoenix.CodeReloader.Server.check_symlinks()
    end

    children =
      pubsub_children(mod, conf) ++
      config_children(mod, conf, otp_app) ++
      server_children(mod, conf, server?) ++
      watcher_children(mod, conf, server?)

    supervise(children, strategy: :one_for_one)
  end
```

`Phoenix.Endpoint.Supervisor` は pubsub, config, server, watcher の4種類の子プロセスを作っている。

`server?` が `true` なら `server_children` によって `Phoenix.Endpoint.Handler` が起動するようになる。

`phoenix/lib/phoenix/endpoint/handler.ex`:

```elixir
  @doc false
  def init({otp_app, endpoint}) do
    handler  = endpoint.config(:handler)
    children =
      for {scheme, port} <- [http: 4000, https: 4040],
          config = endpoint.config(scheme) do
        handler.child_spec(scheme, endpoint, default(config, otp_app, port))
      end
    supervise(children, strategy: :one_for_one)
  end
```

`endpoint.config(:handler)` で得られるのは、特に設定して無ければデフォルトの `Phoenix.Endpoint.CowboyHandler` である。
`Phoenix.Endpoint.CowboyHandler.child_spec/3` で、受けたリクエストを `Plug.Adapters.Cowboy.Handler` に渡す設定をしている。

`phoenix/lib/phoenix/endpoint/cowboy_handler.ex`:

```elixir
  def child_spec(scheme, endpoint, config) do
    ...
    dispatches =
      dispatches ++ [{:_, Plug.Adapters.Cowboy.Handler, {endpoint, []}}]
    ...
  end
```

これで起動時にどういう設定をしているかが分かった。

次はリクエストを受けた際にどうやってディスパッチするかである。
リクエストを受けると `Plug.Adapters.Cowboy.Handler.{init/3, upgrade/4}` に渡され、そこで `HelloWorld.EndPoint.call/2` を呼ぶようになっている。

`plug/lib/adapters/cowboy/handler.ex`:

```elixir
  def init({transport, :http}, req, {plug, opts}) when transport in [:tcp, :ssl] do
    {:upgrade, :protocol, __MODULE__, req, {transport, plug, opts}}
  end

  def upgrade(req, env, __MODULE__, {transport, plug, opts}) do
    conn = @connection.conn(req, transport)
    try do
      %{adapter: {@connection, req}} =
        conn
        |> plug.call(opts)
        |> maybe_send(plug)

      {:ok, req, [{:result, :ok} | env]}
    catch
      ...
    end
  end
```

`HelloWorld.EndPoint.call/2` の実装は `Phoenix.Endpoint` にあり、以下の様になっている。

`phoenix/lib/phoenix/endpoint.ex`:

```elixir
  @doc false
  defmacro __before_compile__(env) do
    ...

    quote do
      defoverridable [call: 2]

      # Inline render errors so we set the endpoint before calling it.
      def call(conn, opts) do
        conn = put_in conn.secret_key_base, config(:secret_key_base)
        conn = put_in conn.script_name, script_name()
        conn = Plug.Conn.put_private(conn, :phoenix_endpoint, __MODULE__)

        try do
          super(conn, opts)
        catch
          kind, reason ->
            Phoenix.Endpoint.RenderErrors.__catch__(conn, kind, reason, @phoenix_render_errors)
        end
      end

      ...
    end
  end
```

実質、単純に `super(conn, opts)` を呼ぶだけである。
これによって Plug ライブラリの仕様に従って `plug ...` と書いている部分が次々と呼ばれることになる。

```elixir
defmodule HelloWorld.Endpoint do
  use Phoenix.Endpoint, otp_app: :hello_world

  ...

  plug Plug.Static,
    at: "/", from: :hello_world, gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt)

  ...

  plug Plug.MethodOverride
  plug Plug.Head

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  plug Plug.Session,
    store: :cookie,
    key: "_hello_world_key",
    signing_salt: "jGwspS0w"

  plug HelloWorld.Router
end
```

`Plug.Static.call/2`, `Plug.MethodOverride.call/2`, ..., `Plug.Session.call/2` と呼ばれていって、最終的には `HelloWorld.Router.call/2` が呼ばれる。

これで無事、リクエストが `HelloWorld.Router` に渡る。

## HelloWorld.Router

`hello_world/web/router.ex`:

```elixir
pipeline :api do
  plug :token_authentication, foo: "foo"
  plug Plug.Session, bar: "bar"
  plug :dispatch
end
```

は大体以下の様に展開される

```elixir
@phoenix_pipeline_scopes [:token_authentication, Plug.Session, :dispatch]

def api(conn, _opts) do
  try do
    conn
    |> token_authentication(foo: "foo")
    |> Plug.Session.call(bar: "bar")
    |> dispatch([])
  catch
    :error, reason ->
      Plug.Conn.WrapperError.reraise(conn, :error, reason)
  end
end
```

この `pipeline` を適用するには `pipe_through` を使う。

`hello_world/web/router.ex`:

```elixir
scope "/", HelloWorld do
  pipe_through :api
  get "/", PageController, :index
  get "/foo", PageController, :foo
end
```

これは大体以下の様に展開される。

```elixir
# pipe_through :api の展開結果
@phoenix_router_scopes [%Scope{...,
                               pipes: [:api],
                               ...},
                        %Scope{}]

# get ... の展開結果
@phoenix_routes [Scope.route(__MODULE__, :match, :get, "/", PageController, :index, []) | @phoenix_routes]
@phoenix_routes [Scope.route(__MODULE__, :match, :get, "/foo", PageController, :foo, []) | @phoenix_routes]
```

この時点では、`pipeline` で定義された関数を呼び出す処理は無く、単に attribute を定義しているだけである。

これらの attribute を使って、`Phoenix.Router.__before_compile__/1` は以下の様に展開される。

```elixir
def __match_route__(conn, "GET", [], _) do
  pipelines = fn conn -> api(conn, []) end
  dispatch = fn conn ->
               opts = PageController.init(:index)
               PageController.call(conn, opts)
             end
  {conn, pipelines, dispatch}
end

def __match_route__(conn, "GET", ["foo"], _) do
  pipelines = fn conn -> api(conn, []) end
  dispatch = fn conn ->
               opts = PageController.init(:foo)
               PageController.call(conn, opts)
             end
  {conn, pipelines, dispatch}
end
```

この `__match_route__` は、`Phoenix.Router.__using__/1` によって定義された `HelloWorld.Router.call/2` 内で使用されている。
`HelloWorld.Router.call/2` は、先程説明したように `HelloWorld.Endpoint` から呼び出される関数である。

`phoenix/lib/router.ex`:

```elixir
def call(conn, _opts) do
  conn
  |> prepare()
  |> __match_route__(conn.method, Enum.map(conn.path_info, &URI.decode/1), conn.host)
  |> Phoenix.Router.__call__()
end
```

`__match_route__/4` の結果を `Phoenix.Router.__call__/1` に渡している。
この実装は以下の様になっている。

`phoenix/lib/router.ex`:

```elixir
  def __call__({conn, pipeline, dispatch}) do
    case pipeline.(conn) do
      %Plug.Conn{halted: true} = halted_conn ->
        halted_conn
      %Plug.Conn{} = piped_conn ->
        try do
          dispatch.(piped_conn)
        catch
          :error, reason -> Plug.Conn.WrapperError.reraise(piped_conn, :error, reason)
        end
    end
  end
```

ここで `pipeline.(conn)` しており、これによって `pipeline` で定義された `api/2` 関数を呼び出している。
その後、`dispatch.(piped_conn)` によって `PageController` の `init/1` と `call/2` を呼び出している。
