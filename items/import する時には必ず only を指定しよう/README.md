`import Foo` と書くと、`Foo` モジュールにある全ての関数が使えるようになります。
こうするとモジュール名を書かずに済むため便利ですが、私は**`import Foo` は使うべきではない**と考えています。

`import Foo` が良くない理由は簡単で、ソースコードを追うのがとても辛くなるからです。

```elixir
defmodule Hoge do
  import Foo
  import Bar

  def f() do
    encode(%{foo: :bar})
  end
end
```

と書いた時、`encode/1` は `Foo` モジュールと `Bar` モジュールのどちらにあるか分かるでしょうか？

これが分かる人は、`Foo` モジュールか `Bar` モジュールについて詳しい人だけで、それらを知らずにこのコードを見た人はまず分かりません。
いきなり `Foo` モジュールと `Bar` モジュールのドキュメントを開いて、どちらのモジュールに `encode/1` があるか探し回ることになる訳です。[^2]

[^2]: なお、`use Foo` とか `use Bar` の場合はもっと深刻で、`Foo` モジュールの `__using__/1` 内で import してる `Foo.Piyo` モジュールにあるかもしれないし、あるいは `AnotherFoo.Helper` モジュールにあるかもしれません。

そのため、以下のような書き方をするのがいいでしょう。

```elixir
defmodule Hoge do
  import Foo, only: [encode: 1]
  import Bar, only: [barcode: 1]

  def f() do
    encode(%{foo: :bar})
  end
end
```

あるいは `encode/1` が関数なら以下のように書いても構いません。

```elixir
defmodule Hoge do
  def f() do
    Foo.encode(%{foo: :bar})
  end
end
```

これらの方法であれば、`encode/1` がどちらのモジュールにあるかすぐに分かります。

つまり、読みやすいコードを書くために、**`import Foo, only: [...]` する**、あるいは **`import` せずにモジュール名を完全修飾して呼び出す** のがいいでしょう。[^3]

[^3]: これは Elixir の IDE あたりがあれば解決するかもしれません（現在これをちゃんと解決できる IDE があるのかどうかは知りませんが）。ただ、それをするならプロジェクト全体で IDE を使うことをルール上定めておく必要があるでしょう。
