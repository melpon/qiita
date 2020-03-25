Erlangの実行時に使える例外機構には`exit/1`、`error/1`、`throw/1`があります。
この中で`error/1`と`throw/1`をどう使い分ければいいかを調べました。

## 結論

- `error/1`はエラーを伝えるために使う
- `throw/1`はフロー制御のために使う

## もう少し詳細に

`error/1`と`throw/1`の大きな違いは、スタックトレースを持っているかどうかです。
つまり`throw/1`は **スタックトレースを必要としない処理にだけ使え** というのが言語側の意図なのでしょう。

いくつかのサイトやプロジェクトを調べた限り、以下のような方針で使うのが良さそうです。

`error/1`は、

- 他の言語で言う「例外を投げる」ということをする場合に使う
- エラーが起きて処理が続行できなくなったことを上位レイヤに知らせるために使う
- そのエラーでプロセスがクラッシュしても構わない場合に使う

要するに、普通に例外を投げる気分で投げればいいだけなので、そんなに難しく考えなくても大丈夫そうです。

`throw/1`は、

- 他の言語で言う「gotoで飛ばして共通処理を行う」ということをする場合に使う
- ネストした関数から `{ok, Value}` と `{error, Reason}` で抜けていくのは、`case ... of` だけでやってるとかなり辛いので、それらをうまいこと扱うために使う
- アプリケーションを超えてその関数を呼び出す側は、間違いなく`throw/1`が呼ばれることを想定していないことを意識して使う
  - `throw/1`を投げることでプロセスが落ちるようなら、間違いなくバグ

要するに「フロー制御のために使う」ということになるのですが、これは例を見たほうが早いでしょう。

`throw/1`を使った例として、各要素に `{ok, Value} | {error, Reason}` を返す関数を適用しつつ、エラーが起きたら即座に中断する関数を定義するなら、以下の様に書きます。

```erlang
apply_f(F, Xs) ->
  try
    lists:map(fun (X) ->
                case F(X) of
                  {ok, Value} -> Value;
                  {error, Reason} -> throw(Reason)
                end
              end, Xs)
  of
    Rs -> {ok, Rs}
  catch
    Reason -> {error, Reason}
  end.
```

もし `throw` を使わない場合、以下の様になります。

```erlang
apply_f(F, Xs) ->
  case
    lists:foldl(fun (_X, {error, Reason}) -> {error, Reason};
                    (X, {ok, Rs}) ->
                      case F(X) of
                        {ok, Value} -> {ok, [Value | Rs]};
                        {error, Reason} -> {error, Reason}
                      end
                end, {ok, []}, Xs)
  of
    {ok, Rs} -> {ok, lists:reverse(Rs)};
    {error, Reason} -> {error, Reason}
  end.
```

読むのが大分つらくなっている感じがします。
また、`throw/1`を使った場合、その瞬間ループが止まるのに対して、こちらは全ての要素に対してループするため、おそらく処理速度的にも`throw/1`を使う方が速い可能性が高いでしょう。[^1]

[^1]: `throw/1`を投げるコスト、リストの長さ、渡された関数が `{error, Reason}` を返す確率によって変わるので、一概にどちらが速いと言うことはできません。

## `throw/1`の使い所

`throw/1`をどのように使うかに関して、いくつか書籍やライブラリを調べてみました。

### すごいE本

以下の様に書いています。

> スロー（throw）は例外のクラスで、プログラマが予想した事態に対処するために使われます。その意図は、終了やエラーのように「プロセスをクラッシュしろ！」と伝えることではなく、フローの制御です。

### Stack Overflow

[when to use throw/1 vs. exit/1 vs. error/1 in Erlang? - Stack Overflow](http://stackoverflow.com/questions/13618261/when-to-use-throw-1-vs-exit-1-vs-error-1-in-erlang]) では、以下の様な例が挙げられています。

```erlang
exists(P, List) ->
  F = fun(X) -> 
    case P(X) of 
      true -> throw(true); 
      Whatever -> Whatever 
    end
  end,
  try lists:foreach(F, List) of
    ok -> false
  catch
   true -> true
  end.
```

ローカルでのループからの離脱に `throw/1` を使っています。

### array

標準ライブラリのarrayでは以下の様に使っています。

```erlang
reset(I, #array{size = N, max = M, default = D, elements = E}=A)
    when is_integer(I), I >= 0 ->
    if I < N ->
            try A#array{elements = reset_1(I, E, D)} 
            catch throw:default -> A
            end;
       M > 0 ->
            A;
       true ->
            erlang:error(badarg)
    end;
reset(_I, _A) ->
    erlang:error(badarg).

reset_1(I, E=?NODEPATTERN(S), D) -> ...
reset_1(_I, E, _D) when is_integer(E) ->
    throw(default);
reset_1(I, E, D) -> ...
```

`reset_1/3` で `throw(default)` を投げ、それを `reset/2` で受けて、デフォルト値 `A` を返すようにしています。
こうすることで、`reset_1/3` でデフォルト値を持つ必要が無くなるし、デフォルト値のためだけに `{ok, Value} | default` みたいな戻り値を返すように全ての関数を修正する必要も無くなります。

### gproc

gprocでも例外が使われています。

```erlang
-define(CATCH_GPROC_ERROR(Expr, Args),
        try Expr
        catch
            throw:{gproc_error, GprocError} ->
                erlang:error(GprocError, Args)
        end).

-define(THROW_GPROC_ERROR(E), throw({gproc_error, E})).
```

```erlang
reg_or_locate(Key) ->
    ?CATCH_GPROC_ERROR(reg_or_locate1(Key), [Key]).

...
reg_or_locate1(_, _, _) ->
    ?THROW_GPROC_ERROR(badarg).
```

エラーが起きた時に `throw/1` で例外を投げて、一番外側の関数でそれを受け、`error/2` を投げ直しています。

エラーが起きた時に `error/1` ではなく `throw/1` を使っているのは、引数 `Args` の情報を `error/2` に入れるためでしょう。
失敗時のフロー制御として `throw/1` を使い、アプリケーションの最上位の関数で受けとり、受け取った側でgprocを使うユーザのためのエラーハンドリングをしているようです。

### lager

```erlang
validate_logfile_proplist(List) ->
    try validate_logfile_proplist(List, []) of
        Res -> ...
    catch
        {bad_config, Msg, Value} ->
            ?INT_LOG(error, "~s ~p for file ~p",
                [Msg, Value, proplists:get_value(file, List)]),
            false
    end.

validate_logfile_proplist([{level, Level}|Tail], Acc) ->
    case validate_loglevel(Level) of
        false ->
            throw({bad_config, "Invalid loglevel", Level});
        Res ->
            validate_logfile_proplist(Tail, [{level, Res}|Acc])
    end;
validate_logfile_proplist([{size, Size}|Tail], Acc) ->
    case Size of
        S when is_integer(S), S >= 0 ->
            validate_logfile_proplist(Tail, [{size, Size}|Acc]);
        _ ->
            throw({bad_config, "Invalid rotation size", Size})
    end;
...
```

lagerでは設定ファイルのバリデーションに`throw/1`が使われていました。
これを`throw/1`を使わないように書き換えるのは結構面倒だし、かなり分かりにくくなると思うので、バリデーションの処理に`throw/1`を使うのはとても良さそうです。

## 参考

- [uwiger/gproc: Extended process registry for Erlang](https://github.com/uwiger/gproc)
- [erlang-lager/lager: A logging framework for Erlang/OTP](https://github.com/erlang-lager/lager)
- [Amazon.co.jp: すごいErlangゆかいに学ぼう！](https://www.amazon.co.jp/dp/B00MLUGZIS/)
- [when to use throw/1 vs. exit/1 vs. error/1 in Erlang? - Stack Overflow](http://stackoverflow.com/questions/13618261/when-to-use-throw-1-vs-exit-1-vs-error-1-in-erlang])
