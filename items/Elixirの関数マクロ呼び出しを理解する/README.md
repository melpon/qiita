Elixirを使っていて最初に詰まったのが、関数やマクロに渡す引数の解釈です。

例えば設定ファイルの書き方です。

```elixir
use Mix.Config

config :logger,
  backends: [:console],
  compile_time_purge_level: :info

config :logger, :console,
  level: :debug
```

最初はこの書き方は設定ファイル特有の構文だと思っていましたが、これは単なる`Mix.Config.config/{2,3}` マクロの呼び出しに過ぎないということを、大分後に知りました。

更に、if〜else〜endやdef〜endといった構文も単なるマクロの呼び出しであり、引数を一定のルールに従って解釈しているだけに過ぎないのです。

```elixir
if cond do
  :foo
else
  :bar
end

def f(a, b) do
  a + b
end
```

これらの書き方で、引数は一体どんな風に解釈されるのかを説明します。

## 括弧の省略

Elixirでは、関数呼び出しの括弧を省略した形で記述できます。

```elixir
defmodule X do
  def f(a, b) do
    a + b
  end
end

X.f(10, 20) # 普通の書き方
X.f 10, 20 # 省略した書き方
```

ただし、曖昧になるようなケースでは省略できません。

```elixir
X.f 10, X.f 10, 20 # コンパイルエラー！曖昧な呼び出し
X.f 10, X.f(10, 20) # OK
X.f(10, X.f(10, 20)) # OK
```

設定ファイルは `Mix.Config.config/{2,3}` マクロの呼び出しであるので、この括弧を省略せず、以下の様に書くことができます。

```elixir
config(:logger,
  backends: [:console],
  compile_time_purge_level: :info)
```

## キーワードリスト

Elixirにはキーワードリストという構文があります。

```elixir
x = [a: 10, b: 20]
```

これは以下の様に書いたのと同じ意味になります。

```elixir
x = [{:a, 10}, {:b, 20}]
```

これ自体は別にそんな便利でない機能に見えるのですが、実はElixirでは頻繁にこれを関数に渡しています。

```elixir
defmodule X do
  def f(args) do
    IO.inspect args
  end
end

X.f([a: 10, b: 20])
X.f(a: 11, b: 21)   # !
X.f a: 12, b: 22    # !!
```

出力:

```
[a: 10, b: 20]
[a: 11, b: 21]
[a: 12, b: 22]
```

キーワードリストは、関数に渡す場合にはコード中の `# !` の行で書いてるように **`[]`を省略できます** 。
先程説明したように、括弧も省略できるので、`# !!`の行のように書けます。

つまり関数呼び出しで `X.f a: 10, b: 20` という記述があった場合、`a: 10, b: 20`というのはキーワードリストであり、受け取る側は一引数として扱われるのです[^1]。

[^1]: 受け取ったキーワードリストを操作するには [`Keyword`](https://hexdocs.pm/elixir/Keyword.html) モジュールを利用します。

キーワードリストとそうでない引数を混ぜた場合、以下の様になります。

```elixir
defmodule X do
  def f(x, y, args) do
    ...
  end
end

# 両者とも同じ呼び出し
X.f 10, 20, a: 30, b: 40
X.f(10, 20, [a: 30, b: 40])
```

`X.f 10, 20, a: 30, b: 40` という書き方は、パッと見た感じだと4引数に見えますが、これは `X.f(10, 20, [a: 30, b: 40])` と解釈され、３引数の関数呼び出しになります。

つまり **関数の引数を区切るカンマより、キーワードリストのカンマの方が結合順位が高い** ということです[^2]。

[^2]: これは人間が見たときの解釈の話であり、コンパイラが実際にカンマの結合順位を見てるかどうかは知りません

また、キーワードリストで`[]`を省略可能なのは、引数の最後にある場合だけです。

```elixir
X.f a: 10, b: 20, 30, 40 # コンパイルエラー！構文が間違っている
X.f [a: 10, b: 20], 30, 40 # OK
```

つまり最初の設定ファイルの構文は、以下の様に解釈できます。

```elixir
# これは
config :logger,
  backends: [:console],
  compile_time_purge_level: :info

# このように解釈される
config(:logger, [backends: [:console],
                 compile_time_purge_level: :info])

# これは
config :logger, :console,
  level: :debug

# このように解釈される
config(:logger, :console, [level: :debug])
```

`backends: [:console], compile_time_purge_level: :info` というのは、キーワードリストであり、これらは１引数として解釈されます。
つまり最初の呼び出しでは `config/2` を、２番目の呼び出しでは `config/3` を使っているだけだということが分かります。

## doとelse

doとelseブロックは、特定のキーワードリストを生成する糖衣構文であると言えます。

```elixir
X.f 100 do
  :foo
else
  :bar
end
```

これは、以下の様に解釈されます。

```elixir
X.f(100, [do: :foo, else: :bar])
```

実際、上記のように書いても同じ意味になります。

これはキーワードリストとも組み合わせられます。

```elixir
# この呼び出しは
X.f 10 20 a: 30, b: 40 do
  100
else
  200
end

# 以下の様に書いたのと同じ
X.f(10, 20, [a: 30, b: 40], [do: 100, else: 200])
# X.f(10, 20, [a: 30, b: 40, do: 100, else: 200]) ではないことに注意
```

このように、キーワードリストとdo,elseの部分は別のキーワードリストになることに注意して下さい。

これらを理解すれば、`if`や`def`がどのように解釈されるのかが分かります。

```elixir
# これは
if cond do
  :foo
else
  :bar
end

# このように解釈される
if(cond, [do: :foo, else: :bar])

# これは
def f(a, b) do
  a + b
end

# このように解釈される
def(f(a, b), [do: a + b])
```

## ライブラリを適切に使う

これらのルールを知っていれば、ライブラリを使うのも楽になります。

例えば [`GenServer.start_link/3`](http://elixir-lang.org/docs/stable/elixir/GenServer.html#start_link/3) は以下の定義になっています。

```elixir
start_link(module, args, options \\ [])
```

もし`[]`が省略できることを知らなければ以下の様に書くでしょう。

```elixir
GenServer.start_link(__MODULE__, :ok, [name: :foobar, debug: :trace])
```

しかし、`options`が引数の最後でキーワードリスト形式になっているのは、以下の様にキーワードリストを省略して書くことを想定しているからだと想像できます。

```elixir
GenServer.start_link(__MODULE__, :ok, name: :foobar, debug: :trace)
```

`()`や`[]`を書くべきかどうかは個人やプロジェクトの判断に寄るでしょうけれども、もし上記のように書かれていたとしても、単に `GenServer.start_link/3` が呼ばれているだけだと分かっていれば、読む側も大分楽になるはずです。

これらのルールを理解して、うまくElixirのコードを読み書きしましょう。
