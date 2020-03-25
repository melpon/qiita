私は private な関数もテストしたい派なので、`defp` で定義した関数をテストしたくなることがよくあります。

`defp` で定義した関数をテストする方法は、調べてみるとすぐに見つかります。

- [Is there a way to test private functions in modules in ExUnit of Elixir? - Stack Overflow](https://stackoverflow.com/a/44125023)

に書いてあるように、[`@compile`](https://hexdocs.pm/elixir/Module.html#module-compile) 属性に `:export_all` を指定するだけです。

```elixir
defmodule Foo do
  @compile if Mix.env == :test, do: :export_all
  
  defp f() do
    ...
  end
end
```

これで `defp` の関数も含めて全て export されるようになります。

ただし、これをすると以下の警告が出ます。

```
warning: export_all flag enabled - all functions will be exported
  lib/my_app/my_app.ex:1
```

`:export_all` が指定されていると警告が出るようです。
この警告は、特に `:warnings_as_errors` オプションを有効にしてると、ずっとコンパイルが通らなくなってしまいます。[^1]

[^1]: `:warnings_as_errors` は警告をエラーとして報告する機能で、多人数で真面目なプロダクトを作るなら必ず付けておくべきオプションです。

何か良い方法が無いかということで、そもそも `@compile` には何を指定できるかというのを調べると、`Module` のドキュメントに [Compile options](https://hexdocs.pm/elixir/Module.html#module-compile) という項目がありました。
それによると `:debug_info` や `:inline` が指定できるらしいですが、更に [Erlang の数々のコンパイルオプション](http://erlang.org/doc/man/compile.html#file-2)も指定できるようです。

それをよく眺めてみると `:nowarn_export_all` というフラグがありました。
これを使って、以下のように書くことで `:export_all` の警告が消せます。

```elixir
defmodule Foo do
  if Mix.env == :test do
    @compile :export_all
    @compile :nowarn_export_all
  end
  
  defp f() do
    ...
  end
end
```

これで無事 private な関数がテストできるようになりました。
private な関数をテストする派の人はそこそこ居ると思うので、必要であればこのように書くといいでしょう。
