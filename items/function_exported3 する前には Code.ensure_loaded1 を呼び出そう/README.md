何らかのビヘイビアを実装したモジュールからそれらの関数を呼び出す時、通常は `function_exported?/3` を呼び出してその関数が実装されているかどうかを確認します。

例えば以下の様な `Foo` ビヘイビアを実装した `FooImpl` モジュールがある場合、

```elixir
defmodule Foo do
  @callback foo() :: :ok
end

defmodule FooImpl do
  @behaviour Foo

  @impl Foo
  def foo() do
    :ok
  end
end
```

これを呼び出す際にはこのように書きます。

```elixir
defmodule Bar do
  def call_foo(mod) do
    if not function_exported?(mod, :foo, 0) do
      raise "Foo behaviour is not implemented"
    end
    mod.foo()
  end
end

call_foo(FooImpl)
```

このように、通常は `function_exported?/3` で関数がエクスポートされているか、つまり `Foo` ビヘイビアが実装されているかを確認してから呼び出します。
これで全く問題のないコードに見えるかもしれません。

しかしこのコードは、`mix` から起動した時に **時々** 動作しない、ということが起こります。

## なぜ動作しないのか

[`function_exported?/3` のドキュメント](https://hexdocs.pm/elixir/Kernel.html#function_exported?/3) にも記載されていますが、`function_exported?/3` 関数は、その関数が実装されていない場合だけでなく、そのモジュールがそもそもロードされていないという場合にも `false` を返します。
あるモジュールを利用するには、事前にそのモジュールを Erlang VM にロードしておく必要があるのですが、状況によってロードされていないことがあるのです。

それを理解するにはモジュールのロード戦略を知る必要があります。
モジュールをロードする戦略は、**組み込みモード** と **対話モード** の２種類があります。

組み込みモードでは、アプリケーション起動時に全てのモジュールがロードされます。
対話モードでは、最初にそのモジュールの関数を呼び出す時に、そのモジュールがロードされます。

そして、`mix` からアプリケーションを動かした場合には **対話モード** で動作します。

そのため、`mix` から上記のコードを動かした時、`FooImpl` のモジュールにある関数を誰も事前に呼んでいない場合 `function_exported?/3` は `false` を返し、エラーになります。
`FooImpl` モジュールの関数を誰かが事前に呼んでいた場合、`function_exported?/3` は `true` を返し、正しく `mod.foo()` が呼ばれます。

つまり、`mix` から起動した場合、事前にどのコードを通っていたかによって動作が変わります。
これが「時々動作しない」と言った理由です。

なお、[Distillery](https://hex.pm/packages/distillery) で生成した場合、デフォルトで [組み込みモード](https://github.com/bitwalker/distillery/blob/7790d50913b766ffdbbb5ab46e44c9f615d6c249/priv/libexec/env.sh#L16) になります。

## 対応策

`function_exported?/3` のドキュメントにも書いてある通り、[`Code.ensure_loaded/1`](https://hexdocs.pm/elixir/Code.html#ensure_loaded/1) を使ってロードしておくのがいいでしょう。

```elixir
defmodule Bar do
  def call_foo(mod) do
    Code.ensure_loaded(mod)
    if not function_exported?(mod, :foo, 0) do
      raise "Foo behaviour is not implemented"
    end
    mod.foo()
  end
end

call_foo(FooImpl)
```

これで無事モジュールがロードされるので、時々ロードされてないという状況を防げます。

もっと厳密にやるなら、`Code.ensure_loaded/1` の戻り値をチェックして、エラーだった場合の分岐を書いた方がいいでしょう。
ただ、そこでエラーだったら次の `function_exported?/3` が `false` を返して `Foo` ビヘイビアを実装していないというエラーになるだけなので、まあいいか、と妥協しています。

## 参考

- [Erlang -- Compilation and Code Loading](http://erlang.org/doc/reference_manual/code_loading.html)
- [Erlang -- System Principles | 1.4  Code Loading Strategy](http://erlang.org/doc/system_principles/system_principles.html#code_loading)
