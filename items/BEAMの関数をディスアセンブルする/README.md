最適化をしたい時、ErlangのプログラムがどのようなBEAMの命令にコンパイルされているのかを知りたいことがあります。

そこで使えるのが`erts_debug:disassemble/1`です。

引数には`{M, F, A}`か、ディスアセンブルしたい命令の位置を直接指定する整数のアドレスを渡します。

## ディスアセンブルしてみる

とりあえず`lists:seq/2`をディスアセンブルしてみましょう。

```erlang
1> erts_debug:disassemble({lists, seq, 2}).
{373440976,
 <<"00000000164241A8: i_func_info_IaaI 0 lists seq 2 \n">>,
 {lists,seq,2}}
```

戻り値として、`{Addr, Inst, MFA}`を返します。

`Inst`の部分、つまり`<<"00000000164241A8: i_func_info_IaaI 0 lists seq 2 \n">>`がディスアセンブルした結果です。
`i_func_info_IaaI`が命令で、`0 lists seq 2`が引数です。
見ての通り、１命令分しかディスアセンブルしてくれません。

`Addr`には次の命令のアドレスが入っているので、これを再度渡すことで次の命令をディスアセンブルしてくれます。

```erlang
2> erts_debug:disassemble(373440976).      
{373441024,
 <<"00000000164241D0: i_minus_jIssd j(00000000164241A8) 2 x(0) 1 x(2) \n">>,
 {lists,seq,2}}
```

ただし、これは本当に次の命令のアドレスを返しているだけなので、次々と実行していると以下のようになります。

```erlang
9> erts_debug:disassemble(373441200).
{373441216,
 <<"00000000164242B0: i_call_only_f lists:seq_loop/3 \n">>,
 {lists,seq,2}}
10> erts_debug:disassemble(373441216).
{373441256,
 <<"00000000164242C0: i_func_info_IaaI 0 lists seq_loop 3 \n">>,
 {lists,seq_loop,3}}
```

戻り値の３番目のタプルが、`{lists,seq,2}`から`{lists,seq_loop,3}`に変わりました。
これを実行し続けると、このままモジュールの終わりまでずっと命令が続きます。[^1]

[^1]: モジュールの終わりまで来ると `ets_debug:disassemble/1` は `false` を返します。

今回は関数をディスアセンブルしたいだけなので、終わりかどうかの判断としてこの３番目のタプルを利用すると良さそうです。
以下のように関数全体をディスアセンブルする関数を書きます。

```erlang
dis(M, F, A) ->
    Fun = fun Fun(V, Xs) ->
                  case erts_debug:disassemble(V) of
                      {Addr, Inst, {M, F, A}} -> Fun(Addr, [Inst | Xs]);
                      {_Addr, _Inst, {_M, _F, _A}} -> lists:reverse(Xs)
                  end
          end,
    Fun({M, F, A}, []).

disp(M, F, A) ->
    Xs = dis(M, F, A),
    lists:foreach(fun (X) -> io:format("~ts", [X]) end, Xs).
```

`dis/3`は`Inst`の一覧をリストで返し、`disp/3`は更にそれを出力します。

```erlang
1> x:disp(lists, seq, 2).
00000000164241A8: i_func_info_IaaI 0 lists seq 2 
00000000164241D0: i_minus_jIssd j(00000000164241A8) 2 x(0) 1 x(2) 
0000000016424200: is_integer_fx f(00000000164241A8) x(0) 
0000000016424218: is_integer_fx f(00000000164241A8) x(1) 
0000000016424230: is_ge_fxx f(00000000164241A8) x(1) x(2) 
0000000016424250: i_minus_jIxxd j(0000000000000000) 2 x(1) x(0) x(0) 
0000000016424280: i_increment_rIId r(0) 1 2 x(0) 
00000000164242A0: move_nx [] x(2) 
00000000164242B0: i_call_only_f lists:seq_loop/3 
```

`disp/3`のような関数をどこかに用意しておけば、便利に使えると思います。

## 診断してみる

Erlangで定数畳み込みが起きているかどうかを確認してみましょう。

```erlang
f() ->
    A = <<1, 2, 3>>,
    B = <<A/binary, 4, 5, 6>>,
    io:format("~p~n", [B]),
    ok.
```

```erlang
6> x:disp(vv, f, 0). 
0000000016E1ED78: i_func_info_IaaI 0 x f 0 
0000000016E1EDA0: allocate_tt 0 0 
0000000016E1EDB0: move_x1_c [<<1,2,3,4,5,6>>] 
0000000016E1EDC0: i_move_call_ext_ce "~p~n" io:format/2 
0000000016E1EDD8: move_deallocate_return_cQ ok 0 
ok
```

各命令の詳細は分かりませんが、`move_x1_c [<<1,2,3,4,5,6>>]` は `io:format/2` に渡す引数に見えるので、`B`が`<<1,2,3,4,5,6>>`に展開されていることが分かります。
つまりErlangコンパイラは定数畳み込みをする、ということが分かります。

このようにディスアセンブルするといろいろ分かって面白いので、気になる場所があったらやってみるといいでしょう。

## 別の方法でディスアセンブルしてみる

`beam_disasm:file/1`を使うという方法もあります。

```erlang
1> c(x).
{ok,x}
2> beam_disasm:file("x.beam").
{beam_file,x,
           [{f,0,2},{module_info,0,4},{module_info,1,6}],
           [{vsn,[322251350146261013614375517310191863950]}],
           [{options,[]},
            {version,"7.0.1"},
            {source,"/Users/melpon/qiita/test/x.erl"}],
           [{function,f,0,2,
                      [{label,1},
                       {line,1},
                       {func_info,{atom,x},{atom,f},0},
                       {label,2},
                       {allocate,0,0},
                       {move,{literal,[<<1,2,3,4,5,6>>]},{x,1}},
                       {move,{literal,"~p~n"},{x,0}},
                       {line,2},
                       {call_ext,2,{extfunc,io,format,2}},
                       {move,{atom,ok},{x,0}},
                       {deallocate,0},
                       return]},
            {function,module_info,0,4,
                      [{line,0},
                       {label,3},
                       {func_info,{atom,x},{atom,module_info},0},
                       {label,4},
                       {move,{atom,x},{x,0}},
                       {line,0},
                       {call_ext_only,1,{extfunc,erlang,get_module_info,1}}]},
            {function,module_info,1,6,
                      [{line,0},
                       {label,5},
                       {func_info,{atom,x},{atom,module_info},1},
                       {label,6},
                       {move,{x,0},{x,1}},
                       {move,{atom,x},{x,0}},
                       {line,0},
                       {call_ext_only,2,{extfunc,erlang,get_module_info,2}}]}]}
```

`[{function,f,0,2,` と書いている行からの要素が `x:f/0` をディスアセンブルした結果です。
先程の例と同様に `{move,{literal,[<<1,2,3,4,5,6>>]},{x,1}}` で定数がそのまま設定されていることが分かります。

なおこの戻り値は `#beam_file{}` 型のレコードになっていて、以下のように定義されています。

```erlang:beam_disasm.hrl
-record(function, {name      :: atom(),
                   arity     :: byte(),
                   entry     :: beam_lib:label(),    %% unnecessary ?
                   code = [] :: [beam_instr()]}).

-record(beam_file, {module               :: module(),
                    labeled_exports = [] :: [beam_lib:labeled_entry()],
                    attributes      = [] :: [beam_lib:attrib_entry()],
                    compile_info    = [] :: [beam_lib:compinfo_entry()],
                    code            = [] :: [#function{}]}).
```

ただし、このレコードは src/ の下にあるので include はできません。
どういう風に使うのが正解なのかはいまいち分からないですね。

`beam_disasm:file/1` の方が `erts_debug:disassemble/1` より読みやすいですが、各命令の名前が [`beam_emu.c`](https://github.com/erlang/otp/blob/maint/erts/emulator/beam/beam_emu.c) と綺麗に対応していないので、ここから先を追う場合は面倒かもしれません。

用途に応じて使い分けましょう。
