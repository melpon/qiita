Elixirでは、小文字で始まる、記号を含まない関数しか定義できません。

```elixir
# これはOK
def fA0(a, b), do: a + b

# 大文字で始まるのはNG
def Hoge(a, b), do: a + b

# 数字で始まるのはNG
def 0Fuga(a, b), do: a + b

# 言語道断
def !"#$%&'()=~|(a, b), do: a + b
```

一方、Erlangでは、関数名はただのアトムであるため、どのような文字列であっても定義可能です。

```erlang
%% OK
fA0(A, B) -> A + B.

%% OK
'Hoge'(A, B) -> A + B.

%% OK
'0Fuga'(A, B) -> A + B.

%% OK
'!"#$%&\'()=~|'(A, B) -> A + B.
```

Elixirは最終的にErlangと同じ構造になるんだから、Erlangで出来るものがElixirで出来ない訳がないと、調べてみました。

## アドホックな解決策

[Elixir で普通では使えない記号が入った名前の関数を定義する](http://qiita.com/uasi/items/2d5dc9a06bc55b96ed6f) で書かれている方法で、任意のアトムを関数名にできます。

```elixir
defmodule MyApp.Impl do
  defmacro __using__(_) do
    # 記号入り関数を定義
    name = :"!\"#$%&'()=~|"
    quote do
      def unquote(name)(a, b) do
        a + b
      end
    end
  end
end

defmodule MyApp do
  use MyApp.Impl
end

# このように呼び出す
MyApp."!\"#$%&'()=~|"(10, 20)
```

ただし、この方法だと、記号入りの関数を定義しようと思う度に、上記のようなモジュールを作って `use` を呼び出す必要があります。
もう少し汎用的に出来ないかと、いろいろ試してみました。

## 汎用的な解決策

[追記][コメント欄](http://qiita.com/melpon/items/451fdcd5be67f269a9af#comment-a2f4f7037beaf3e45872)にすごく簡単に書ける方法が書いてあるので、そちらを参照して下さい。このセクションはほぼ無意味になりますが、黒歴史的な意味で残しておきます。[/追記]

以下のようなマクロを書くことで、汎用的に記号入り関数を書けるようにしました。

```elixir
defmodule Symbol do
  defmacro defsymbol(name, args, expr) do
    define(:def, name, args, [], expr, __CALLER__)
  end

  defmacro defsymbol(name, args, opts, expr) do
    define(:def, name, args, opts, expr, __CALLER__)
  end

  defmacro defsymbolp(name, args, expr) do
    define(:defp, name, args, [], expr, __CALLER__)
  end

  defmacro defsymbolp(name, args, opts, expr) do
    define(:defp, name, args, opts, expr, __CALLER__)
  end

  defp assert_module_scope(env, fun, arity) do
    case env.module do
      nil -> raise ArgumentError, "cannot invoke #{fun}/#{arity} outside module"
      _   -> :ok
    end
  end

  defp assert_no_function_scope(env, fun, arity) do
    case env.function do
      nil -> :ok
      _   -> raise ArgumentError, "cannot invoke #{fun}/#{arity} inside function/macro"
    end
  end

  defp define(kind, name, args, opts, expr, env) do
    assert_module_scope(env, kind, 2)
    assert_no_function_scope(env, kind, 2)
    line = env.line

    # >>>>>>>
    call = {name, [line: line], args}
    call = case Keyword.fetch(opts, :when) do
             {:ok, when_expr} -> {:when, [line: line], [call, when_expr]}
             :error -> call
           end
    # <<<<<<<

    {call, unquoted_call} = :elixir_quote.escape(call, true)
    {expr, unquoted_expr} = :elixir_quote.escape(expr, true)

    check_clauses = not(unquoted_expr or unquoted_call)
    pos = :elixir_locals.cache_env(env)

    quote do
      :elixir_def.store_definition(unquote(line), unquote(kind), unquote(check_clauses),
                                   unquote(call), unquote(expr), unquote(pos))
    end
  end
end
```

これは [`Kernel.def`マクロの実装](https://github.com/elixir-lang/elixir/blob/07e05c5fd48495c860b840364e8f4f6e63cecb5b/lib/elixir/lib/kernel.ex#L3457) をほぼそのまま持ってきた形になります。
違いは `name` と `args` から `call` を作っているところ（`# >>>>>>>` と `# <<<<<<<` の間）だけです。

`defsymbol`を使って定義するには、`def f(a, b)` と書いていた部分を、`defsymbol :f, [a, b]` にするだけです。
`:f` の部分には任意のアトムを書けるため、どのような文字列でも記述できるようになります。

具体的には、以下のように使います。

```elixir
defmodule MyApp do
  import Symbol, only: [defsymbol: 3, defsymbol: 4]
  # アトムで関数名を書ける
  defsymbol :fA0, [a, b], do: a + b
  defsymbol :"Hoge", [a, b], do: a + b
  defsymbol :"0Fuga", [a, b], do: a + b
  defsymbol :"!\"#$%&'()=~|", [a, b], do: a + b

  # when も書ける
  defsymbol :f, [a, b], when: a < b do
    a + b
  end
end

MyApp.fA0(10, 20)
MyApp."Hoge"(10, 20)
MyApp."0Fuga"(10, 20)
MyApp."!\"#$%&'()=~|"(10, 20)
```

このようなマクロを定義することで、無事、記号を含む関数を定義できるようになりました。

## なぜ記号入り関数を定義したいのか

こんなもの何に使うんだ、と思うかもしれませんが、実際に、プロダクションのコードで記号入りの関数を定義する必要がありました。

というのも、ErlangにはXMLをパースする [`xmerl`](http://erlang.org/doc/man/xmerl.html) というライブラリがあり、`xmerl:export_element/2` などを実行した際にコールバックされる関数の名前が、`#text#` や `#element#` といった、記号入りの名前なのです。

この詳細は省きますが、例えばXMLの中に入っている無駄な空白を除去するなら、以下のように書きます。

```elixir
# XMLの空白を除去するコールバックモジュール
defmodule RemoveSpace do
  import Symbol, only: [defsymbol: 3]

  defsymbol :"#xml-inheritance#", [] do
    :xmerl_xml."#xml-inheritance#"()
  end

  defsymbol :"#text#", [text] do
    text = IO.iodata_to_binary(text)
    text = Regex.replace(~r/\n\s*/, text, "")
    text = Regex.replace(~r/>\s+</, text, "><")
    :xmerl_xml."#text#"(text)
  end

  defsymbol :"#element#", [tag, data, attrs, _parents, _element] do
    [:xmerl_lib.start_tag(tag, attrs), data, :xmerl_lib.end_tag(tag)]
  end
end

# 適当な空白等が入ったXML
xml = '''
<?xml version="1.0"?>   <root>
    <foo>
        </foo>
</root>
'''

{xml_element, _} = :xmerl_scan.string(xml)
rs_xml = :xmerl.export_element(xml_element, RemoveSpace) |> IO.chardata_to_string()

IO.puts rs_xml
# <root><foo></foo></root> が出力される
```

このように、`xmerl`を使っていると、記号入りの関数を定義する必要が出てくるのです。

ということで、必要になった人は上記のコードを使うといいでしょう。
