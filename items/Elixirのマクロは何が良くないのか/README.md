Elixirのマクロ、便利ですよね。
使ってますか？使ってる？じゃあ今すぐ **使うのをやめろ**。

# マクロの良くない点

一言で言えば **何が起きるか想像できない** という点に付きる。

```elixir
defmodule HelloWorld.Router do
  use HelloWorld.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end
end
```

こういう `pipeline` マクロと `plug` マクロの呼び出しがある時、これによって何が起きるか想像できるだろうか。
何かのモジュールを `import` しているかもしれないし、何らかの attribute を定義してるかもしれないし、便利で汎用的な関数を定義しているかもしれない。

もしこんなマクロがある場合、そのモジュール内で別の関数を定義する際に以下のことを考える必要がある。

- 自動で定義された関数と名前が被っていないか
- 自動で定義された関数の中で使っている関数名と名前が被っていないか
- 自動で定義された便利で汎用的な関数と同じような機能を提供してしまっていないか

例え **`pipeline` や `plug` とは全く無関係と思われる関数を書く場合であっても** 一度は上記のことを確認しておく必要がある。
そしてマクロのコードは **とても読みづらい** ので、無関係な関数を書くために大量の時間を使うことになる。

もしこれが、以下の様な定義だったらどうだろう。

```elixir
defmodule HelloWorld.Router do
  @behaviour Pipeline

  def pipelines() do
    [browser: [{:accepts, ["html"]},
               :fetch_session,
               :fetch_flash,
               :protect_from_forgery,
               :put_secure_browser_headers],
     api: [{:accepts, ["json"]}]]
  end
end
```

こう書いておけば、このモジュールで他の関数を定義する際に考えることは、「`pipelines/0` 関数を定義してはいけない」ということだけである。
例え **`pipelines/0` が何のために使われているのか分からなくても**、それだけ考えれば良い。

[Macroのドキュメント](http://elixir-lang.org/getting-started/meta/macros.html#foreword) にも、以下の様に書かれている。

> Elixir already provides mechanisms to write your every day code in a simple and readable fashion by using its data structures and functions. Macros should only be used as a last resort. Remember that **explicit is better than implicit**. **Clear code is better than concise code**.

要約すると「Elixir はマクロを使わなくても何とかなる機能を提供している。マクロは最後の手段として使うべきだ。**暗黙より明示**を、**簡潔なコードより明確なコード**を。」という感じだ。

「マクロを使わなくても何とかなる機能を提供している」は大嘘だと思っている（この記事の後半部分）が、マクロが最後の手段というのはほんとにそうだし、みんな気軽にマクロを使い過ぎだと思う。

# `use`は死ぬべき

`use` は、`require` してから `__using__/1` マクロを呼び出すだけの機能なので、マクロが良くない存在だから当然 `use` も良くない。
ただ `use` は正直 **マクロより質が悪い** 。

`use` は当然マクロなので **何でもできる**。
`use` は様々な用途に使われていて、自分が調べた限り、`use` で提供している機能は以下の様なものがある。

## ミックスイン

便利な関数や attribute をそのモジュールに埋め込む。

```elixir
defmodule AutoSource do
  defmacro __using__(_) do
    @source __MODULE__  |> Macro.underscore() |> String.replace("/", "_")

    def module_to_source(mod) do
      mod |> Macro.underscore() |> String.replace("/", "_")
    end
  end
end
```

## `@behaviour` のデフォルト実装

これは例えば [`GenServer`](https://hexdocs.pm/elixir/GenServer.html) でこの機能が使われている。

```elixir
defmodule GenServer do
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour GenServer

      def init(args) do
        {:ok, args}
      end

      def handle_call(msg, _from, state) do
        ...
      end

      def handle_info(msg, state) do
        ...
      end

      def handle_cast(msg, state) do
        ...
      end

      def terminate(_reason, _state) do
        :ok
      end

      def code_change(_old, state, _extra) do
        {:ok, state}
      end

      defoverridable [init: 1, handle_call: 3, handle_info: 2,
                      handle_cast: 2, terminate: 2, code_change: 3]
    end
  end
end
```

## 定数バインディング

コンパイル時に定数になる値をバインドして、少ない引数で呼べるようにする。

```elixir
defmodule MigrationRouter do
  def allow_migrate(app, schema, repo, opts) do
    ...
  end

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      otp_app = Keyword.fetch!(opts, :otp_app)
      @otp_app otp_app

      def allow_migrate(schema, repo, opts) do
        MigrationRouter.allow_migrate(@otp_app, schema, repo, opts)
      end
    end
  end
end
```

```elixir
defmodule MyRouter do
  use MigrationRouter, otp_app: :my_app
end

# MigrationRouterを使うなら4引数渡さないといけないところを…
MigrationRouter.allow_migrate(:my_app, MySchema, MyRepo, [])

# MyRouterを使えば3引数で済む
MyRouter.allow_migrate(MySchema, MyRepo, [])
```

## コード生成

ミックスインと似ているが、もっと複雑なDSL(domain-specific language)を用意して、それらの値を使ってめちゃめちゃ頑張ったコードを生成する。

これは [Phoenixのルーティング](http://qiita.com/melpon/items/b9a712e784e7b9919ce6) や [Phoenixのコントローラ](http://qiita.com/melpon/items/9530c52ab1350acb4409) にあるようなコードのことである。

## `require`や`import`をまとめて呼び出す

こういう感じの機能のこと。

```elixir
defmodule HelloWorld.Web.Controller do
  def __using__(_) do
    quote do
      use Phoenix.Controller

      alias HelloWorld.Repo
      import Ecto
      import Ecto.Query

      import HelloWorld.Router.Helpers
      import HelloWorld.Gettext
    end
  end
end

```

```elixir
defmodule HelloWorld.Router do
  use HelloWorld.Web.Controller
end
```

----

多分他にも `use` で提供している機能はあると思う。

`use Foo` という１行から、どういう動作をするのか全く想像がつかないし、何か１つ関数や attribute を定義しようと思ったときには、そこと影響が無いかを `__using__/1` を見に行かないと安心できない。[^1]

[^1]: ドキュメントを読めばいいと思うかもしれないけど、ドキュメントには「こう使うものだよ」とは書いているが、「この名前の関数と attribute が定義されるし、このモジュールをimportしているし、定義した関数内でこの関数を使っているよ」ということまでは書いてくれているのは見たことが無い

しかもこれらの機能は **組み合わせて提供される** し、**一部の機能だけを利用することができない**[^2]。オール・オア・ナッシングである。[^3]

[^2]: 提供者側がうまいこと書いていれば一部だけ使うように選択できるが、そんな親切な `__using__/1` は見たことが無い

[^3]: 大抵（無意味な）`import`とセットになっている。`import`死すべし派としてはほんとつらい

そして Elixir のマクロと `use` を除いた機能だけで普通にプログラミングするのは、不可能とは言わないが、かなり手間だし、正直ストレスが溜まる。
つまり `use` は、普通にプログラミングするためには、使わざるを得ないのが現状である。
マクロは使わなくても何とかなるが、`use` は使わざるを得ないという点で **マクロより質が悪い**。

Elixir は、ミックスインや `@behaviour` のデフォルト実装などを、それ単体の機能として提供するべきだと思う。
`use` は悪という文化を根付かせたいけど、今は `use` が無いとまともにプログラミングできないので、そう言う訳にもいかない。
ただ、マクロは良くないので、可能な限り使わないようにしよう。
