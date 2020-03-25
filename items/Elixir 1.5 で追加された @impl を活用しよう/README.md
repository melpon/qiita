Elixir 1.5 で `@impl` という属性が追加されました。
今日はこの `@impl` について書きます。

## `@impl` とは

`@impl` は「この関数はコールバック関数の実装だよ」ということを **コードを読む人に** 伝えるための機能です。

例えば以下のコードがある時、

```elixir
defmodule MyApp do
  @behaviour Plug

  def init(opts) do
    opts
  end

  def call(conn, _opts) do
    Plug.Conn.send_resp(conn, 200, "hello")
  end
end
```

この `init/1` と `call/2` が `Plug` ビヘイビアのコールバック関数を実装したものであると気がつくでしょうか？
これに気がつくためには `Plug` ビヘイビアの要求するコールバック関数が何なのかを予め知っている必要があります。

そこで `@impl` です。これを使うと、コードを読む人にとって大変優しい状態になります。

```elixir
defmodule MyApp do
  @behaviour Plug

  @impl Plug
  def init(opts) do
    opts
  end

  @impl Plug
  def call(conn, _opts) do
    Plug.Conn.send_resp(conn, 200, "hello")
  end
end
```

これで `init/1` と `call/2` が `Plug` ビヘイビアが要求するコールバック関数の実装であることがすぐに分かります。

自分としては `@impl` の有用性としてはこれだけで十分[^1]なのですが、副次的な効果として以下が挙げられます。

[^1]: コメントでもいいじゃんと思うかもしれませんが、この統一された形式で書けることに意味があるので、公式がこういうのを用意してくれるのはとても重要です。

### 一貫性を保ってくれる

どれか１箇所でも `@impl` を使った場合、残りのコールバック関数も `@impl` を使わないと警告が出てくれます。

```elixir
defmodule MyApp do
  @behaviour Plug

  @impl Plug
  def init(opts) do
    opts
  end

  # わざと @impl Plug を使わない
  def call(conn, _opts) do
    Plug.Conn.send_resp(conn, 200, "hello")
  end
end
```

コンパイル結果:

```
warning: module attribute @impl was not set for callback def call/2 (callback specified in Plug). This either means you forgot to add the "@impl true" annotation before the definition or that you are accidentally overriding a callback
  lib/my_app.ex:13
```

また、不要な関数に `@impl` を使っても警告が出ます。

```elixir
defmodule MyApp do
  @behaviour Plug

  @impl Plug
  def init(opts) do
    opts
  end

  # call を間違えて ca11 と typo してしまった
  @impl Plug
  def ca11(conn, _opts) do
    Plug.Conn.send_resp(conn, 200, "hello")
  end
end
```

コンパイル結果:

```
warning: got @impl Plug for def ca11/2 but the behaviour does not specify this callback. The known callbacks are:

  * Plug.call/2 (def)
  * Plug.init/1 (def)

  lib/my_app.ex:15

warning: undefined behaviour function call/2 (for behaviour Plug)
  lib/my_app.ex:1
```

前者の警告が `@impl` を書いたことによる警告、後者の警告が `@behaviour Plug` をしているのに `call/2` が定義されていないことによる警告です。

見ての通り、今までも `@behaviour` を書いていれば、タイポした場合や引数の数が違っていた場合に警告が出ていたので、この辺のメリットはあまり無いかもしれません。
ただ、警告のメッセージがより分かりやすいものになっているので、そこはいい感じです。

また、`@behaviour` では防げないケースもあって、例えば `Foo` ビヘイビアが `foo/0` コールバック関数を要求してたとして、こんなモジュールがあったとします。

```elixir
defmodule MyApp do
  @behaviour Foo

  @impl Foo
  def foo() do
    "fooooo"
  end

  def bar() do
    "baaaar"
  end
```

`bar/0` は、コールバック関数ではない、ただの関数です。
この時、バージョンアップによって `Foo` ビヘイビアに `bar/0` コールバック関数を追加した場合、`bar/0` に `@impl` を書いていないという警告が出てくれます。
もし `@impl` を書いていなかった場合は何の警告も出ません。たまたま既存の関数と名前が一致していて動作するのは、恐らく意図した通りの動作にはならないでしょう。

このように一貫性を保ってくれるので、安心して（今後コールバック関数と名前が被ってしまうかもしれない）関数が書けます。

### 自動で `@doc false` してくれる

コールバック関数は自由に呼んでいい関数ではないので、ドキュメントを生成した際にコールバック関数の実装がドキュメントに載らないようにしてくれます。[^2]

[^2]: 個人的には、モジュールのドキュメントに「このビヘイビアを実装しているよ」というのを自動的に書いて欲しいのですが、そういう機能は特に無いようです。

## `@impl true` は使わない

`@impl true` という、どのビヘイビアの実装であるかを自動的に判別してくれる機能がありますが、これは使わない方がいいでしょう。
「`@behaviour` を書いてるんだから、`@impl true` って書いておけばどのビヘイビアかすぐに分かるのでは？」と思うかもしれません。

しかし、例えば以下のコードの場合、

```elixir
defmodule MyApp do
  use Foo.Bar

  @impl true
  def bun() do
    "cho"
  end
end
```

`bun/0` 関数がどのビヘイビアの実装なのか分かりません。
これを知るには `Foo.Bar.__using__/1` あたりから見ていって、どのビヘイビアを実装しているかを調べる必要があります。

読みやすくするのが目的なので、コールバック関数の実装であることは分かっても、どのビヘイビアの実装なのか分からないのでは片手落ちです。
そのため「`use` を一箇所でも使っているなら `@impl true` を使ってはならない」となるのですが、それをするぐらいなら常にビヘイビアの名前を書くというルールにしておいてもいいでしょう。

## パターンマッチする場合は全部の関数に `@impl` を付ける

パターンマッチすると、同じ関数の定義が何度も出てきますが、その関数がコールバック関数なら **全ての定義に** `@impl` を付けるべきです。

```elixir
# こっちに書いてるなら
@impl GenServer
def handle_call(:get_value, _from, state) do
  ...
end

# こっちにも書くべき
@impl GenServer
def handle_call({:set_value, value}, _from, state) do
  ...
end
```

`@impl` の機能としてはどちらか片方に付いていれば同じ効果になりますが、読みやすくするという目的の上では両方にあった方がいいし、コードを見た人が「こっちには `@impl` が付いてるのに、何でこっちには付いていないんだろう？」みたいな疑問を持たずに済みます。

## まとめ

- `@impl` はコードを読む人にとってとても分かりやすくなるので、どんどん使っていきましょう。副次的な効果もあるよ。
- `@impl true` は使わなくていいよ
- パターンマッチする時にも全部の関数に `@impl` を書いていこう
