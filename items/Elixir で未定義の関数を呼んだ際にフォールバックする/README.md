Elixir では、モジュール内で `:"$handle_undefined_function"` 関数を定義すると、未定義の関数を呼び出した際にその関数にフォールバックしてくれるようになります。

```elixir
defmodule Foo do
  def f() do
    IO.puts "定義済み: f()"
  end

  def unquote(:"$handle_undefined_function")(function, args) do
    IO.puts "未定義: #{function}, #{inspect args}"
  end
end

Foo.f()
Foo.bar()
Foo.mokemoke(1, "hoge")
```

出力:

```
定義済み: f()
未定義: bar, []
未定義: mokemoke, [1, "hoge"]
```

## 仕組み

実はこれは Elixir の機能ではなく、Erlang の仕組みです。

[`apply/3` のドキュメント](http://erlang.org/doc/man/erlang.html#apply-3) を読んでみると、呼び出そうとした関数が無かった場合に、`:error_handler.undefined_function/3` を呼ぶと書いてあります。
そして [`:error_handler.undefined_function/3` のドキュメント](http://erlang.org/doc/man/error_handler.html#undefined_function-3) を読んでみると、`:"$handle_undefined_function"/2` が定義されている場合にはこの関数を呼ぶという仕様になっています。
`Foo.bar()` という呼び出しが `apply(Foo, :bar, [])` と等価であるという仕様がどこに書いてあるか分かりませんが、結果から考えるに、恐らく等価なのでしょう。

ということで、`Foo.bar()` と書いた時に `bar/0` 関数が未定義なら `:"$handle_undefined_function"(:bar, [])` が呼ばれることになります。

## 通常のアプリケーションで使ってはいけない

[`:error_handler.undefined_function/3` のドキュメント](http://erlang.org/doc/man/error_handler.html#undefined_function-3) には利用上の警告が書かれています。
それによると、デバッグが難しくなる、Dialyzer や Xref 等の静的解析が効かなくなる等の理由により、通常のアプリケーションコードにこれを使うことは推奨していません。
テスト時やデバッグ時に使う外部用のスタブやモックとして利用するとか、そういう用途には使えるでしょう。

実際、Ecto の Repo をテスト用に適当に定義してくれる [ecto_it](https://hex.pm/packages/ecto_it) ライブラリで `:"$handle_undefined_function"` 関数が[定義されています](https://github.com/xerions/ecto_it/blob/41b9885b3768efa184967ae8384443b9a4d74e01/lib/ecto_it/repo.ex#L12-L14)。
これは良い使い方だと言えます。

覚えておくと何かに使えるかもしれません。
