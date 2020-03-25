Elixirで例外が発生した時、あるいは発生していなくてもスタックトレースの情報が欲しいことがあったので調べました。

# 例外時

例外が起きた時は当然、詳細なログを出すために欲しくなります。

例外時にスタックトレースの情報を取得するには `System.stacktrace/0` を使用します。[^1][^2]

[^1]: `f() + 1` のように ` + 1` している理由は、末尾呼び出し最適化を妨害するためです。詳しくは [Elixirのスタックトレースと末尾呼び出し最適化](http://qiita.com/melpon/items/0eedc19d6aece782d13d) へ。

[^2]: Elixir 1.7 からは `System.stacktrace/0` は deprecated になりました。代わりに `__STACKTRACE__` を使ってください。

```elixir
defmodule X do
  def f() do
    throw 42
  end
  def g() do
    f() + 1
  end
  def h() do
    g() + 1
  end
end

try do
  X.h()
catch
  42 ->
    IO.inspect System.stacktrace()
end
```

出力:

```elixir
[{X, :f, 0, [file: 'prog.exs', line: 3]},
 {X, :g, 0, [file: 'prog.exs', line: 6]},
 {X, :h, 0, [file: 'prog.exs', line: 9]},
 {:elixir_compiler_0, :__FILE__, 1, [file: 'prog.exs', line: 14]},
 {:elixir_compiler, :dispatch_loaded, 6,
  [file: 'src/elixir_compiler.erl', line: 125]},
 {:elixir_lexical, :run, 3, [file: 'src/elixir_lexical.erl', line: 17]},
 {:elixir_compiler, :quoted, 3, [file: 'src/elixir_compiler.erl', line: 30]},
 {Code, :require_file, 2, [file: 'lib/code.ex', line: 370]}]
```

いい感じに整形した形で取得したいなら `Exception.format_stacktrace/1` を使います。

```elixir
try do
  X.h()
catch
  42 ->
    IO.puts Exception.format_stacktrace(System.stacktrace())
end
```

出力:

```
    prog.exs:3: X.f/0
    prog.exs:6: X.g/0
    prog.exs:9: X.h/0
    prog.exs:14: (file)
    (elixir) src/elixir_compiler.erl:125: :elixir_compiler.dispatch_loaded/6
    (elixir) src/elixir_lexical.erl:17: :elixir_lexical.run/3
    (elixir) src/elixir_compiler.erl:30: :elixir_compiler.quoted/3
    (elixir) lib/code.ex:370: Code.require_file/2
```

# 通常時

例外が発生していない場合でも、その関数がどこから呼ばれたか知りたいことがあります。
例えば共通処理の関数内で `Logger.info` を使ってログを出力する時、スタックトレースの情報があれば、どこに問題があるのかすぐに分かります。

通常時にスタックトレースの情報を取得するには `Process.info(self(), :current_stacktrace)` を使用します。

```elixir
defmodule X do
  def f() do
    {:current_stacktrace, t} = Process.info(self(), :current_stacktrace)
    IO.inspect t
    0
  end
  def g() do
    f() + 1
  end
  def h() do
    g() + 1
  end
end

X.h() + 1
```

出力:

```elixir
[{Process, :info, 2, [file: 'lib/process.ex', line: 521]},
 {X, :f, 0, [file: 'prog.exs', line: 3]},
 {X, :g, 0, [file: 'prog.exs', line: 8]},
 {X, :h, 0, [file: 'prog.exs', line: 11]},
 {:elixir_compiler_0, :__FILE__, 1, [file: 'prog.exs', line: 15]},
 {:elixir_compiler, :dispatch_loaded, 6,
  [file: 'src/elixir_compiler.erl', line: 125]},
 {:elixir_lexical, :run, 3, [file: 'src/elixir_lexical.erl', line: 17]}]
```

いい感じに整形した形で取得したいなら、例外と同様にこれを `Exception.format_stacktrace/1` に渡すだけです。

```elixir
...
  def f() do
    {:current_stacktrace, t} = Process.info(self(), :current_stacktrace)
    IO.puts Exception.format_stacktrace(Enum.drop(t, 1))
    0
  end
...
X.h() + 1
```

出力:

```
    prog.exs:3: X.f/0
    prog.exs:8: X.g/0
    prog.exs:11: X.h/0
    prog.exs:15: (file)
    (elixir) src/elixir_compiler.erl:125: :elixir_compiler.dispatch_loaded/6
    (elixir) src/elixir_lexical.erl:17: :elixir_lexical.run/3
```

`Process.info/1` もスタックトレースに含まれていて邪魔なので、`Enum.drop(t, 1)` で除けています。

なお [`Exception.format_stacktrace/1` のドキュメント](https://hexdocs.pm/elixir/Exception.html#format_stacktrace/1) を読むと、「引数が無いバージョンを使うと `Process.info/2` で現在のスタックトレース情報を取得して返してくれる」とありますが、普通に書くと、呼び出し元の関数の情報が落ちてしまいます。

```elixir
...
  def f() do
    IO.puts Exception.format_stacktrace()
    0
  end
...
X.h() + 1
```

出力:

```
    prog.exs:7: X.g/0
    prog.exs:10: X.h/0
    prog.exs:14: (file)
    (elixir) src/elixir_compiler.erl:125: :elixir_compiler.dispatch_loaded/6
```

`X.f/0` の情報がありません。

[コードを見てみる](https://github.com/elixir-lang/elixir/blob/v1.4.2/lib/elixir/lib/exception.ex#L388) と、`Enum.drop(t, 3)` で、3個のトレース情報を消していることが分かります。
1個は `Process.info/2` で、1個は `Exception.format_stacktrace/1` を除けているのは分かりますが、なぜか更にもう1個、つまり `X.f/0` の情報も消していることになります。

すごい微妙なので、これはもう1個無駄にスタック情報を積んでから呼び出すのが良いと思います。

```elixir
defmodule X do
  def format_stacktrace() do
    Exception.format_stacktrace() <> ""
  end
  def f() do
    IO.puts format_stacktrace()
    0
  end
  ...
end

X.h() + 1
```

出力:

```
    prog.exs:6: X.f/0
    prog.exs:10: X.g/0
    prog.exs:13: X.h/0
    prog.exs:17: (file)
```

ただ、スタックトレースの情報は7個までしか取得していないようで、ただでさえ少ないスタックトレースの情報が3個も削られてかなり勿体無いと感じます。
どのように取得するのかはちょっと考えた方が良さそうです。
