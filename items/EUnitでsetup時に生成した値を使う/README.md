EUnitでは、`_test_` で終わる関数を定義すると、テストジェネレータとして動作します。

```erlang
foo_test_() = {setup, fun setup/0, fun cleanup/0, fun foo/0}.
```

この様に、開始時に呼ぶ関数、終了時に呼ぶ関数、実際のテストを記述することで、テスト用の関数を自動生成してくれます。

引数の無い単純なテストならこれで問題ないですが、時々、`setup`関数の戻り値を使ってテストを書きたいことがあります。

例えば、テストのために特定のプロセスを事前に起動しておく必要があるけれども、テストごとに立てておくプロセス数が異なる場合です。
この場合、setup時にプロセスを起動しておき、それらのPidをテストで利用したくなるのです。

その場合、以下のように書くことでsetup時に生成した値を使えます。

```erlang
%% setup時にN個のプロセスを起動しておく
setup(N) ->
  lists:map(fun (_) -> hoge:spawn_link() end, lists:seq(1, N)).

%% 開始したプロセスを全て終了させておく
cleanup(Pids) ->
  lists:foreach(fun (Pid) -> gen_server:stop(Pid)).

%% Pidsを使っていろいろテストする
bar(Pids) ->
  ...

bar_test_() = {setup,
               %% 10プロセス起動
               fun () -> setup(10) end,
               fun (Pids) -> cleanup(Pids) end,
               %% Instantiatorを使ってPidsをテストに渡す
               fun (Pids) ->
                       fun () ->
                               bar(Pids)
                       end
               end}.
```

`bar_test_/0`のタプルの最後で渡している関数は Instantiator と呼ばれるもので、setup時の関数の戻り値を引数として受け取り、テスト関数を返す関数として定義します[^1]。

[^1]: 他にも返せる値はありますが、詳細は[ドキュメント](http://erlang.org/doc/apps/eunit/chapter.html#Fixtures)を参照

こうすることで、無事setup時に生成した値をテストで利用できます。
EUnitはドキュメントをよく読むと結構いろいろな機能があるので、一通り見ておくことをお勧めします。

## 参考

- [Erlang -- EUnit - a Lightweight Unit Testing Framework for Erlang](http://erlang.org/doc/apps/eunit/chapter.html)
