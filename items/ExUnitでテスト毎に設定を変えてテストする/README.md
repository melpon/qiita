ExUnitで、テスト毎に設定を変えたいことがあります。

例えば以下のようなケースです。

- 設定ファイルで指定するモジュールを切り替えてテストしたい

データの保存先としてETSやDETS、RedisやDynamoDBを使うモジュールをそれぞれ用意することがあります。
これらのモジュールを設定で切り替えている場合、各モジュールをテストするためには、テスト毎に設定を変えてやる必要があります。

- ある処理がタイムアウトすることを確認するためのテストで、タイムアウトの時間を短くしてテストしたい

タイムアウトの時間が30秒とかだと、タイムアウトのテストをするのが難しいため、タイムアウトのテストだけ時間を短くしてテストをしたくなります。
タイムアウトの時間を設定から読んでいる場合、その時間をタイムアウトのテストだけ短くしてやる必要があります。

ExUnitで、これらを実現する方法を説明します。

## 何が問題か

例えば以下の様なアプリケーションがあったとします。

```elixir:lib/my_app.ex
defmodule MyApp do
  use Application

  @backend_procname :backend_process_name

  def start(_type, _args) do
    # バックエンドモジュールをenvから取得する
    module = Application.get_env(:my_app, :backend, MyApp.Backend.ETS)
    # バックエンドモジュールのプロセスをsupervisorに登録する
    children = [
      module.child_spec(@backend_procname),
    ]
    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def get_name() do
    # 実行しているバックエンドのモジュール名を取得する
    GenServer.call(@backend_procname, {:get_name, key})
  end

  def get_value(key) do
    # タイムアウトの時間をenvから取得する
    timeout = Application.get_env(:my_app, :timeout, 5000)
    GenServer.call(@backend_procname, {:get_value, key}, timeout)
  end

  def set_value(key, value) do
    GenServer.call(@backend_procname, {:set_value, key, value})
  end
end
```

このテストは、以下の様に書きたいと思うかもしれません。

```elixir:test/my_app_test.exs
defmodule MyAppTest do
  test "ets backend" do
    # バックエンドを DETS にする
    Application.put_env(:my_app, :backend, MyApp.Backend.DETS)
    assert MyApp.Backend.DETS == MyApp.get_name()
  end

  test "redis backend" do
    # バックエンドを Redis にする
    Application.put_env(:my_app, :backend, MyApp.Backend.Redis)
    assert MyApp.Backend.Redis == MyApp.get_name()
  end
end
```

しかしこれは正しく動きません。
`MyApp.get_name()`で、意図しないバックエンド名を取得し、`assert` に引っかかることになります。

これは、テスト開始時には、既にアプリケーションが起動しているからです。
`mix test` を実行した時、以下の順序で処理が行われています。

1. 設定ファイル `config/config.exs` をロード
2. 各種ソースのコンパイル
3. プロジェクトのアプリケーションを起動
4. ExUnitを起動して、`test/test_helper.exs`をロードし、`test/*_test.exs`ファイルを探す
5. それぞれのテストを実行する

テスト開始直後、つまり 5 の時点で設定を上書きしても手遅れです。
なぜなら、3 に書いてあるように、**テスト実行時には既にアプリケーションが起動している** からです。
3 のアプリケーション開始時に設定を読んで処理を実行している場合、5 のテスト実行時に設定を書き換えても、もうその設定が読まれることが無いため、手遅れなのです。

また、`Application.put_env/3` で設定を書き換えると次のテスト開始時にも影響が出るため、毎回値を戻す必要があります。

## 解決する

**アプリケーションを起動せずにテストを開始する** ことで、これらの問題を解決できます。

`mix test --no-start` のように `--no-start` を指定すると、アプリケーションを開始せずにテストを開始します。
つまり上記の 3. を省いてテストを開始します。

テスト開始時に自分で設定ファイルを読み込んで適用（設定の初期化）し、テストごとに異なる設定を適用し、その後にアプリケーションを起動することで、設定を変えて起動できるようになります。

```elixir:test/test_helper.exs
defmodule TestHelper do
  def setup(application, config) do
    # 初期設定を読み込んで適用する
    init(Mix.Config.read!("config/config.exs"))
    # テスト毎に異なる設定の上書き
    Enum.each(config, &override(&1))
    # アプリケーション開始
    {:ok, apps} = Application.ensure_all_started(application)

    # 終了時には全てのアプリケーションを止める
    ExUnit.Callbacks.on_exit(fn ->
      Enum.each(apps, &Application.stop(&1))
    end)
    :ok
  end

  defp init(config) do
    for {app, kvs} <- config do
      for {key, value} <- kvs do
        Application.put_env(app, key, value, persistent: true)
      end
    end
  end

  defp override({application, key, value}) do
    Application.put_env(application, key, value, persistent: true)
  end

  defp override({application, key, subkey, value}) do
    kw = Application.get_env(application, key, [])
    kw = Keyword.put(kw, subkey, value)
    Application.put_env(application, key, kw, persistent: true)
  end
end
```

これは以下のように使います。

```elixir:test/my_app_test.exs
defmodule MyAppTest do
  test "ets backend" do
    # バックエンドを DETS にして起動する
    TestHelper.setup(:my_app, [{:my_app, :backend, MyApp.Backend.DETS}])
    assert MyApp.Backend.DETS == MyApp.get_name()
  end

  test "redis backend" do
    # バックエンドを Redis にして起動する
    TestHelper.setup(:my_app, [{:my_app, :backend, MyApp.Backend.Redis}])
    assert MyApp.Backend.Redis == MyApp.get_name()
  end

  test "timeout" do
    # バックエンドをダミーモジュールにして、タイムアウトをテストする
    TestHelper.setup(:my_app, [{:my_app, :timeout, 1}])
    {:timeout, _} = catch_exit(MyApp.get_value())
  end
end
```

また、これによって、全てのテストはアプリケーションを開始せずに実行される前提になります。
毎回 `mix test --no-start` と入力するのは大変なので、`mix test` と入力するだけで同じ効果になるようにしましょう。

```elixir:mix.exs
defmodule Myapp.Mixfile do
  def project do
    [app: :myapp,
     ...,
     aliases: [test: "test --no-start"]]
  end

  ...
end
```

これで、いつも通り `mix test` と打つだけでテストができます。
