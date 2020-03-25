
## 末尾呼び出し最適化

Elixir（というか ErlangVM）は末尾呼び出し最適化を行います。

```elixir
defmodule X do
  def f(0) do
    IO.puts "success!"
  end

  def f(n) do
    if rem(n, 1000000) == 0 do
      IO.inspect n
    end
    f(n - 1)
  end
end

X.f(10000000)
```

出力:

```
10000000
9000000
8000000
7000000
6000000
5000000
4000000
3000000
2000000
1000000
success!
```

`X.f/1` は1000万回、再帰的な呼び出しをしています。
普通に考えれば、スタックが溢れそうなものですが、Elixirでは問題なく動きます。

これは Elixir の **末尾呼び出し最適化** によるものです。
末尾呼び出し最適化は、関数の末尾で次の関数を呼んだ場合、コンテキストをスタックに積まずにその関数を呼び出すという最適化です。
これによって Elixir はスタックを溢れさせずに再帰的な呼び出しができるようになっています。

末尾でない場合は最適化が効かないので、以下の様にするとメモリ不足でクラッシュします。[^1]

[^1]: 環境によって何回のループでクラッシュするかは変わります

```elixir
defmodule X do
  def f(0) do
    IO.puts "success!"
  end

  def f(n) do
    f(n - 1)
    # fの呼び出しの後に書くことで末尾呼び出し最適化を妨害する
    if rem(n, 1000000) == 0 do
      IO.inspect n
    end
  end
end

X.f(10000000)
```

出力:

```
eheap_alloc: Cannot allocate 212907632 bytes of memory (of type "heap").

Crash dump is being written to: erl_crash.dump...done
```

## スタックトレース

末尾呼び出し最適化は良い機能ですが、スタックトレースとの相性は悪いと言えます。
例えば以下のコードでスタックトレースを取ってみます。

```elixir
defmodule X do
  def f() do
    IO.puts "f()"
    throw 42
  end
  def g() do
    IO.puts "g()"
    f()
  end
  def h() do
    IO.puts "h()"
    g()
  end
end

try do
  X.h()
catch
  42 ->
    IO.puts Exception.format_stacktrace(System.stacktrace())
end
```

出力:

```
h()
g()
f()
    prog.exs:4: X.f/0
    prog.exs:17: (file)
    (elixir) src/elixir_compiler.erl:125: :elixir_compiler.dispatch_loaded/6
    (elixir) src/elixir_lexical.erl:17: :elixir_lexical.run/3
    (elixir) src/elixir_compiler.erl:30: :elixir_compiler.quoted/3
    (elixir) lib/code.ex:370: Code.require_file/2
    (elixir) lib/kernel/cli.ex:437: Kernel.CLI.wrapper/1
    (elixir) lib/enum.ex:1229: Enum."-map/2-lists^map/1-0-"/2
```

スタックトレースの情報から `X.h/0` や `X.g/0` の情報が消えていることが分かります。
これが末尾呼び出し最適化による影響です。

末尾呼び出し最適化は、スタックにコンテキストを積みません。
つまり呼び出し元の関数の情報を保存せず、次の関数を実行します。
そのためスタックトレースには情報が残らないのです。

コンパイルオプションやデバッグオプションで末尾呼び出し最適化をしない（`call_only`命令を使わない）みたいなのがあるか調べたのですが、特に無さそうです。
なのでスタックトレースに情報を残したいなら、末尾呼び出し最適化を何とかして妨害する必要があります。

```elixir
defmodule X do
  def f() do
    throw 42
  end
  def g() do
    # + 1 することで末尾呼び出し最適化を妨害する
    f() + 1
  end
  def h() do
    # + 1 することで末尾呼び出し最適化を妨害する
    g() + 1
  end
end

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

これで無事 `X.g/0` や `X.h/0` の情報がスタックトレースに表示されるようになりました。

ただ、末尾呼び出し最適化を妨害するためだけに謎のコードを埋め込むのは良くないので、「スタックトレースの情報を見た時に、本来呼ばれてるべき関数が無かったら末尾呼び出し最適化が起きたと考えて、焦らずにどの関数から呼ばれたかを予測する」ぐらいでいいのかもしれません。
