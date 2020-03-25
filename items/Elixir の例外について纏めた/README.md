意外と複雑だったので、可能な限り情報を纏めてみました。

## 例外の種類

Elixir の例外には throw と error と exit という３種類があります。
どの例外も、投げた直後に処理を抜けて `catch` や `rescue` に飛ぶ（あるいはそれが無ければプロセスが終了する）という点では同じです。

### throw

throw はフロー制御のための例外です。
Elixir は、関数の途中で return したり、ループで条件を満たしたら break するというのが出来ません。
そういう時、処理の流れ（フロー）を変えるために throw を利用します。

throw を使うことで、例えば [`Enum.find/2`](https://hexdocs.pm/elixir/Enum.html#find/3) のような関数は以下のように書けます。

```elixir
def find(enumerable, default \\ nil, fun) do
  try do
    for v <- enumerable do
      # 値が見つかったら即終了
      if fun.(v) do
        throw v
      end
    end
    default
  catch
    found -> found
  end
end
```

例外を投げるには [`throw/1`](https://hexdocs.pm/elixir/Kernel.html#throw/1) を使います。
`throw/1` で投げた例外は `catch` 節で上記のように書けば取得できます。

また、throw は **例外発生時のスタックトレースを取得しません**。
そのため `catch` 節で [`System.stacktrace/0`](https://hexdocs.pm/elixir/System.html#stacktrace/0) を実行しても情報は取れません。

このように、throw はフロー制御のために投げるもので、確実に誰かが catch すると分かっている前提の機能です。
throw で投げた例外がモジュールやアプリケーションのレイヤーを越えたり、あるいはプロセスが落ちる可能性があるなら、それはほぼバグでしょう。

### error

error は、他の言語でよくある普通の例外と同じ用途で利用します。
つまり、ファイルが存在しなかったり、パターンマッチに失敗したり、関数に不正な引数を渡したりした場合に error を利用します。

```elixir
try do
  raise RuntimeError, message: "error"
rescue
  e -> IO.puts("Error: #{inspect e}")
end
```

出力:

```
Error: %RuntimeError{message: "error"}
```

error の例外を投げるには [`raise/{1,2}`](https://hexdocs.pm/elixir/Kernel.html#raise/1) を使います。
`raise/{1,2}` で投げた例外は `rescue` 節で上記のように書けば取得できます。
他の方法でも取得できますが、詳細に関しては後で説明します。

### exit

exit は、プロセスを終了させるための例外です。
通常、この例外を捕まえて処理を継続させてはいけません。

```elixir
try do
  exit :anyway_i_want_to_exit
catch
  :exit, reason -> IO.puts("You should not exit yet: #{reason}")
end
```

出力:

```
You should not exit yet: anyway_i_want_to_exit
```

exit の例外を投げるには [`exit/1`](https://hexdocs.pm/elixir/Kernel.html#exit/1) を使います。
`exit/1` で投げた例外は `catch` 節で上記のように書けば取得できます。

## 例外の捕捉について

[`try/1`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#try/1) ブロック内で投げた例外は、`rescue` や `catch` で捕まえられます。
ただし `rescue` や `catch` では仕様が全然違うので、仕様を理解して適切に使い分けましょう。

### `rescue` 節

`rescue` 節は、通常のエラー、つまり error の例外だけを捕捉する目的で利用します。

`rescue` で受け取る際、特定の例外だけを捕まえることもできます。

```elixir
try do
  ...
rescue
  error in RuntimeError -> ... # RuntimeError だけ捕まえる
  error in [RuntimeError] -> ... # 上に同じ
  error in [RuntimeError, ArgumentError] -> ... # RuntimeError と ArgumentError だけ捕まえる
  error -> ... # 全ての error 例外を捕まえる
end
```

コメントにあるように `<変数名> in <例外のモジュール名 or そのリスト>` で特定の例外だけを捕まえます。
判定は上から順番に行われ、最初に一致した部分を実行します。

逆に、`rescue` で受け取る際にパターンマッチを使うことはできません。

```elixir
try do
  ...
rescue
  %RuntimeError{message: message} -> ... # コンパイルエラー！こういう書き方はできない
end
```

### Erlang のエラーを `rescue` 節で受け取る

`rescue` 節では、Erlang のエラーも適切に Elixir のエラーに変換されます。

Erlang で error の例外を投げるには [`:erlang.error/1`](http://erlang.org/doc/man/erlang.html#error-1) を使っていました。
ただ、この例外は Elixir の例外と違い、`:erlang.error/1` には任意の term を渡すことができます。

```elixir
:erlang.error(:badarg)
:erlang.error({:badkey, map, key})
```

`rescue` 節では、`:erlang.error/1` によって投げられた Erlang のエラーも捕捉できます。

```elixir
try do
  UndefinedModule.undefined_function()
rescue
  error in UndefinedFunctionError -> IO.puts(Exception.message(error))
end
```

出力:

```
function UndefinedModule.undefined_function/0 is undefined (module UndefinedModule is not available)
```

通常、未定義の関数を呼び出すと、`:erlang.error(:undef)` という Erlang の例外が投げられます。
`rescue` 節の内部では、この Erlang の例外を捕捉して、既知の名前なら、対応する例外に置き換えています。
`:undef` は既知の名前として登録されているので、内部で `:undef` に対応する [`UndefinedFunctionError`](https://hexdocs.pm/elixir/UndefinedFunctionError.html) に置き換えます。

もし既知の名前でなかった場合は、[`ErlangError`](https://hexdocs.pm/elixir/ErlangError.html) に置き換えます。

```elixir
try do
  :erlang.error(:unknown_exception)
rescue
  error in ErlangError -> IO.puts(Exception.message(error))
end
```

出力:

```
Erlang error: :unknown_exception
```

つまり、`:erlang.error/1` で投げた Erlang の例外は全て Elixir の例外に置き換えられるため、`rescue` 節では Elixir の例外だけ気にすればいいということになります。

また、`ErlangError` で例外を捕まえると、**全ての Erlang の例外を捕捉する** という機能もあります。
つまり先程の `:undef` も `ErlangError` で取得できます。

```elixir
try do
  UndefinedModule.undefined_function()
rescue
  error in ErlangError -> IO.puts(Exception.message(error))
end
```

出力:

```
function UndefinedModule.undefined_function/0 is undefined (module UndefinedModule is not available)
```

`error in ErlangError` で `ErlangError` だけ捕捉しているように見えますが、出力を見れば分かるように、変数 `error` は `UndefinedFunctionError` になっています。
これは `ErlangError` と書いた場合だけの特殊なケースで、普通はこんなことは出来ません。
どういう用途で使うのかは分かりませんが、`ErlangError` で捕捉する際には気を付けておきましょう。

### `catch` 節

`catch` 節は、`rescue` 節とは違い、throw/error/exit の全ての種類の例外を捕捉できます。
error 以外の例外も取得したい場合に利用します。

以下のように書きます。

```elixir
try do
  ...
catch
  value -> ... # :throw, value と同じ
  :throw, value -> ... # throw の例外だけ捕まえる
  :error, value -> ... # error の例外だけ捕まえる
  :exit, value -> ... # exit の例外だけ捕まえる
  _, value -> ... # あらゆる例外を捕まえる
end
```

コメントに書いているように、`:throw`, `:error`, `:exit` でパターンマッチすることで、それぞれ throw/error/exit の例外を捕捉できます。
省略した場合には `:throw` を書いたのと同じ意味になります。

また、`rescue` 節と違ってパターンマッチが使えるので、`_, value -> ...` と書くことで全ての例外を捕捉できるし、`value` 側でもパターンマッチが可能です。

```elixir
try do
  raise RuntimeError, message: "foo"
catch
  :error, %RuntimeError{message: "foo"} -> IO.puts("caught a RuntimeError and the message is \"foo\"")
end
```

出力:

```
caught a RuntimeError and the message is "foo"
```

ただし、`rescue` 節と違って Erlang のエラーを Elixir のエラーに変換する機能はありません。

```elixir
try do
  :erlang.error(:badarg)
catch
  :error, %ArgumentError{} -> IO.puts("ArgumentError")
  :error, :badarg -> IO.puts(":badarg") # こっちが呼ばれる
end
```

出力:

```
:badarg
```

Elixir のエラーに変換する場合、[`Exception.normalize/3`](https://hexdocs.pm/elixir/Exception.html#normalize/3) が使えます。
実際、`rescue` 節も内部的には `Exception.normalize/3` を呼んでいるだけです。

```elixir
try do
  :erlang.error(:badarg)
catch
  # この時点では error は :badarg
  :error, error ->
    # Elixir の例外に変換
    error = Exception.normalize(:error, error)
    # error が ArgumentError になっている
    IO.inspect(error)
end
```

出力:

```
%ArgumentError{message: "argument error"}
```

### `catch` と `rescue` の優先順位

`catch` 節と `rescue` 節が両方とも記述されている場合、記述した順序に関係なく、常に `rescue` 節が優先されます。

```elixir
try do
  :erlang.error("foo")
catch
  _, error -> IO.puts("catch: #{inspect error}")
rescue
  error -> IO.puts("rescue: #{inspect error}")
end
```

出力:

```
rescue: %ErlangError{original: "foo"}
```

このコードでは、`catch` 節の後に `rescue` 節を書いていますが、`rescue` 節で捕捉できるエラーだったため、そちらが優先されています。

なお、Elixir 1.6 以降は、このような書き方をしたら警告が出るようになりました。
なので Elixir 1.6 以降は警告さえちゃんと守ってれば気にしなくて良さそうです。

### `else` 節と `after` 節について

例外の捕捉とは関係ないですが、`try/1` では `else` 節と `after` 節も書けるので、軽く説明しておきます。

`else` 節は、`try` ブロックの中で例外が起きなかった場合に実行される節です。
`try` の最後の式の結果を引数として受け取り（パターンマッチ可能）、処理を続行します。

この `else` 節の例外は、ここの `try/1` の `catch` や `rescue` では **捕捉されません** 。
捕捉したいなら直前の `try/1` の方に書くか、`else` 節の中で更に `try/1` をネストさせて書きましょう。

```elixir
try do
  ...
else
  value when value < 100 ->
    # ここで発生した例外は捕捉されない
    do_something1()
  value ->
    # 更に try/1 で囲めばそこで捕捉可能
    try do
      do_something2()
    rescue
      error -> ...
    end
rescue
  error -> ...
end
```

`after` 節は、`try` の処理が成功しても失敗しても実行される節です。
あまり説明することは無いですが、注意としては、`after` 節は **必ず実行されるとは限らない** ことです。

他のプロセスから突然 exit メッセージがやってきた場合、`after` 節を実行する間も無くプロセスが終了します。
幸いなことに、メモリやファイルディスクリプタといったリソースは、プロセスが終了すると自動的に解放されるので、そこはあまり気にする必要はありません。
ただ、`after` 節の実行がログやコンソールに残らない可能性があるので気を付けましょう。

## `raise/{1,2}` について

`raise/{1,2}` は error の例外を投げる関数です。
基本的には、以下のように利用します。

```elixir
raise <エラー型>, <エラー型固有の引数>
```

例えば [`KeyError`](https://hexdocs.pm/elixir/KeyError.html) の場合、以下のように書きます。

```elixir
key = ...
term = ...
raise KeyError, key: key, term: term
```

`KeyError` がエラーの型で、`key: key, term: term` が `KeyError` 型固有の引数となります。
これによって `raise/2` は処理の中で `KeyError.exception(key: key, term: term)` という処理を呼ぶので、`KeyError.exception/1` の中で例外用の構造体を生成して返すようになっています。
詳細は `Exception` ビヘイビアの説明でやります。

### 詳細な使い方

`raise/{1,2}` では他にもいくつかの方法で error の例外を投げることができます。

```elixir
raise "aaa"
# → raise RuntimeError, message: "aaa" と同じ意味

raise FooError
# → raise FooError, [] と同じ意味

raise %RuntimeError{message: "aaa"}
# → RuntimeError.exception/1 を呼ばず、そのまま例外を投げる
```

コメントに書いている通りです。

特に３番目の方法を利用することで、`exception/1` を実装せずに例外を投げられます。
これも詳細は `Exception` ビヘイビアの説明でやります。

## `reraise/{2,3}` について

`raise/{1,2}` を使うと、スタックトレースがそれを呼び出した場所からになります。
しかし、[例外中立](https://qiita.com/Kokudori/items/987073d59529b6c9a37c#%E4%BE%8B%E5%A4%96%E4%B8%AD%E7%AB%8B) の観点から、一度受け取った例外を、まるで元の場所で例外が投げられたかのように振る舞いたいことが（割と頻繁に）あります。

そういう場合には [`reraise/{2,3}`](https://hexdocs.pm/elixir/Kernel.html#reraise/2) を利用します。

```elixir
try do
  do_something()
rescue
  error ->
    Logger.error(fn -> "do_something/0 were occure an error: #{inspect error}" end)
    # 元のスタックトレースで再度例外を投げる
    reraise error, System.stacktrace()
end
```

こうすることで、スタックトレースを見た時に例外の発生元が `do_something/0` になり、例外の中立性が保たれることになります。

なお Elixir 1.7 以降では `System.stacktrace()` の代わりに `__STACKTRACE__` を使う必要があります。

## `defexception/1` について

[`defexception/1`](https://hexdocs.pm/elixir/Kernel.html#defexception/1) は、新しい例外を定義するための機能です。
`defexception/1` で例外を定義することで、その構造体を使って例外を投げれるようになります。

```elixir
defmodule FooError do
  defexception [:message]
end

try do
  raise FooError, message: "foo error"
rescue
  error -> IO.puts "Exception occured: #{Exception.message(error)}"
end
```

出力:

```elixir
Exception occured: foo error
```

### `defexception/1` の実装

もう少し具体的に言うと、`defexception/1` は、例外フラグの付いた構造体を定義し、[Exception ビヘイビア](https://hexdocs.pm/elixir/Exception.html#callbacks) をいい感じに実装します。

```elixir
defmodule FooError do
  defexception [:message]
end
```

このコードは、実際には以下のコードに展開されます。

```elixir
defmodule FooError do
  @behaviour Exception
  defstruct [{:__exception__, true}, :message]

  # ------ :message がある場合のみ定義 begin ------
  @spec message(Exception.t) :: String.t
  def message(exception) do
    exception.message
  end

  defoverridable message: 1

  @spec exception(String.t) :: Exception.t
  def exception(msg) when is_binary(msg) do
    exception(message: msg)
  end
  # ------ :message がある場合のみ定義 end ------

  @spec exception(keyword) :: Exception.t
  def exception(args) when is_list(args) do
    Kernel.struct!(__struct__(), args)
  end

  defoverridable exception: 1
end
```

`defstruct` の部分で `{:__exception__, true}` というフィールドを追加しています。
この `:__exception__` が `true` かどうかが、例外用の構造体かどうかを分けるためのフラグになっています。
実際、例外用の構造体かどうかを判別する [`Exception.exception?/1`](https://hexdocs.pm/elixir/Exception.html#exception?/1) 関数は以下のような実装になっています。

```elixir
def exception?(%{__struct__: struct, __exception__: true}) when is_atom(struct), do: true
def exception?(_), do: false
```

`__exception__: true` のパターンマッチを使って例外を認識していることが分かります。

また、`raise` は [`Exception` ビヘイビア](https://hexdocs.pm/elixir/Exception.html#callbacks) の関数を要求します。
そのため `defexception/1` では `Exception` ビヘイビアが要求する `message/1` と `exception/1` を実装しています。

「:message がある場合のみ定義」というコメントで挟まれた部分は、`:message` フィールドが存在している場合だけ実装します。
見ての通り、`:message` フィールドを指定せずに例外を定義した場合には `message/1` が実装されないので、その場合は自分で `message/1` を実装する必要があります。

## `Exception` ビヘイビアについて

`Exception` ビヘイビアは、`raise` で例外を投げたり、エラーメッセージを取得する際に利用するコールバック関数です。
`Exception` ビヘイビアは以下の関数を要求しています。

```elixir
@callback exception(term) :: Exception.t
@callback message(Exception.t) :: String.t
```

具体的にこれらの関数がどのように使われているかというと、

- `raise mod, args` した時に `mod.exception(args)` を呼んで例外の構造体を作る
- `Exception.message(exception)` した時に `mod.message(exception)` を呼んでメッセージを取得する

となります。
例えば `raise FooError, message: "foo error"` と書いた場合には `FooError.exception(message: "foo error")` が呼ばれ、その例外の構造体を受け取って `Exception.message(error)` と書いた場合には `FooError.message(error)` が呼ばれます。

逆に言えば、それをしなければ `Exception` ビヘイビアを実装してなくても例外を扱えます。

```elixir
defmodule FooError do
  defstruct [__exception__: true]
end
```

```elixir
try do
  raise %FooError{}
rescue
  error -> IO.puts "Exception occured: #{inspect error}"
end
```

出力:

```elixir
Exception occured: %FooError{}
```

`defstruct/1` の際に `__exception__: true` を入れておく必要はありますが、`Exception` ビヘイビアが要求する関数は一切実装していません。
`raise/1` の仕様として、引数に例外の構造体を渡した場合、`mod.exception(args)` を呼ばず、引数に渡した例外を直接利用します。
今回は `raise %FooError{}` と例外の構造体を直接渡しているので、`FooError.exception([])` は呼ばれません。

また、`rescue` の中でも `Exception.message(error)` を利用していないので、ここでも `FooError.message(error)` が呼ばれず、結局 `Exception` ビヘイビアが要求する関数を実装していなくても動作します。

### `Exception.message/1` を利用する

例外からメッセージを取り出すなら、ほとんどの場合は何も考えず [`Exception.message/1`](https://hexdocs.pm/elixir/Exception.html#message/1) を呼び出した方がいいでしょう。
`error.message` で直接 `:message` フィールドを取り出したり、`FooError.message(error)` のように直接 `Exception` ビヘイビアの関数を読んだりしてはいけません。

なぜなら、`error` がどのような例外か分からない場合、確実に `error.message` が存在するとは限らないし、その例外が確実に `message/1` を実装しているとは限らないからです。
`Exception.message/1` なら、そのような場合でもちゃんとメッセージを返してくれます。

ただし、`Exception.message/1` に渡す引数は例外用の構造体である必要があります。
つまり `error.__exception__ == true` なフィールドが含まれている必要があります。

これは `catch` で Erlang の `:badarg` や `:undef` といった、atom の例外を受け取った場合に問題になります。
そのような Erlang の例外を受け取る可能性がある場合には [`Exception.normalize/3`](https://hexdocs.pm/elixir/Exception.html#normalize/3) を呼び出して、Erlang の例外を Elixir の例外に変換しましょう。

つまり、`catch` 節であらゆるエラーからメッセージを取得するなら以下のようになります。

```elixir
try do
  ...
catch
  :error, error ->
    # normalize してから
    exc = Exception.normalize(:error, error)
    # メッセージを取得
    message = Exception.message(exc)
end
```

## まとめ

Elixir の例外は、最初に書いたように、意外と複雑です。

ただ、実際に利用するのはこの中の一部分だけでしょう。
それでもこれだけ細かく説明したのは、これを知っておくことで、落とし穴を回避できる可能性が高いからです。

例えば `raise/{1,2}` の仕様と、`defexception/1` で `:message` の有無によってどういうコードが生成されるかを知っていれば、`raise MyError, "message"` と書いた時のコンパイルエラーを早めに修正できるでしょう。[^1]

[^1]: `raise MyError, "message"` は `MyError.exception("message")` を呼び出すが、`defexception/1` で `:message` を定義しなかった場合は文字列を引数に取る `exception/1` が定義されないので、コンパイルエラーになる

例外の仕様を把握して、Elixir の例外とうまく付き合っていきましょう。

## 参考

- [try, catch, and rescue - Elixir](https://elixir-lang.org/getting-started/try-catch-and-rescue.html)
- [Exception](https://hexdocs.pm/elixir/Exception.html)
- [try/1 - Kernel.SpecialForms](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#try/1)
- [raise/1 - Kernel](https://hexdocs.pm/elixir/Kernel.html#raise/1)
- [reraise/1 - Kernel](https://hexdocs.pm/elixir/Kernel.html#reraise/1)
