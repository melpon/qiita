Erlangではwhen区の中で使える関数が限定されています。

```erlang
is_hoge() ->
  io:format("~p~n", [hoge]),
  true.

func() when is_hoge() ->
  do_something.
```

これで`func()`を呼び出そうとすると、以下のようなエラーが出ます。

```
prog.erl:5: call to local/imported function is_hoge/0 is illegal in guard
```

`when`区で使える関数は、ホワイトリスト方式で決められています。
つまり`when`区では、特定の関数以外は使えません。
そのためこのようなエラーが出るのです。

これは`when`の中で関数呼び出しをした際に、何らかの副作用が出てしまうのを防ぐためです。

ではどの関数が使えるのか、というのが気になるところです。
[ドキュメント](http://erlang.org/doc/reference_manual/expressions.html#id84484)には "Arithmetic expressions" や "Boolean expressions" などが書かれていますが、それが具体的にどの関数なのかを調べてみました。

以下の通りです。

## BIF

| BIF |
|:-:|
| abs/1 |
| float/1 |
| trunc/1 |
| round/1 |
| length/1 |
| hd/1 |
| tl/1 |
| size/1 |
| bit_size/1 |
| byte_size/1 |
| element/2 |
| self/0 |
| map_size/1 |
| node/{0,1} |
| tuple_size/1 |
| is_atom/1 |
| is_binary/1 |
| is_bitstring/1 |
| is_boolean/1 |
| is_float/1 |
| is_function/{1,2} |
| is_integer/1 |
| is_list/1 |
| is_map/1 |
| is_number/1 |
| is_pid/1 |
| is_port/1 |
| is_reference/1 |
| is_tuple/1 |
| is_record/{2,3} |
| binary_part/{2,3} |

`binary_part/{2,3}`はドキュメントには載っていません。
どこかの新しいバージョンで追加されたんでしょうか。

## 算術演算子

| 算術演算子 |
|:-:|
| '+'/{1,2} |
| '-'/{1,2} |
| '*'/2 |
| '/'/2 |
| 'bnot'/1 |
| 'div'/2 |
| 'rem'/2 |
| 'band'/2 |
| 'bor'/2 |
| 'bxor'/2 |
| 'bsl'/2 |
| 'bsr'/2 |

## bool演算子

|bool演算子|
|:-:|
| 'not'/1 |
| 'and'/2 |
| 'or'/2 |
| 'xor'/2 |

## 比較演算子

|比較演算子|
|:-:|
| '=='/2 |
| '/='/2 |
| '=<'/2 |
| '<'/2 |
| '>='/2 |
| '>'/2 |
| '=:='/2 |
| '=/='/2 |

## 参考

- [8.25  Guard Sequences](http://erlang.org/doc/reference_manual/expressions.html#id84484)
- [otp/core_lint.erl at cc25f4cd184ca41ba935e1d2d14eb250e68f11d2 · erlang/otp](https://github.com/erlang/otp/blob/cc25f4cd184ca41ba935e1d2d14eb250e68f11d2/lib/compiler/src/core_lint.erl#L314)
- [otp/erl_internal.erl at cc25f4cd184ca41ba935e1d2d14eb250e68f11d2 · erlang/otp](https://github.com/erlang/otp/blob/cc25f4cd184ca41ba935e1d2d14eb250e68f11d2/lib/stdlib/src/erl_internal.erl)

