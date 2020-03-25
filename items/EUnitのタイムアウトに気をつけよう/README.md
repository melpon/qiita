EUnitでテストコードを増やしていくと、突如としてテストが通らなくなることがあります。

## 導入

例えば元のコードが以下のようになっていた時、

```erlang
foo_test() ->
    do_something(),
    timer:sleep(1000), % 処理を待つ
    ?assertEqual(read_something(), 10), % 処理結果を取得

    do_something2(),
    timer:sleep(2000),
    ?assertEqual(read_something(), 20),

    ok.
```

このコードに対して、以下のように同じようなコードを追加すると、なんとテストが通らなくなります。

```erlang
foo_test() ->
    do_something(),
    timer:sleep(1000), % 処理を待つ
    ?assertEqual(read_something(), 10), % 処理結果を取得

    do_something(),
    timer:sleep(2000),
    ?assertEqual(read_something(), 20),

    %% ここが追加した部分
    do_something(),
    timer:sleep(3000),
    ?assertEqual(read_something(), 30),

    ok.
```

しかもこの時のエラーメッセージはこのようになっています。

```
===> Performing EUnit tests...

Pending:
  myapp_foo_tests:foo_test/0: module 'myapp_foo_tests'
    %% Unknown error: timeout
  undefined
    %% Unknown error: {blame,[1]}


Finished in ? seconds
2 tests, 0 failures, 2 cancelled
===> Error running tests
```

通常、テストに失敗したら、失敗した場所からのスタックトレースが表示されるはずなのに、なぜかそのような情報がありません。
役に立ちそうな情報が無いため、エラーメッセージから原因を調べるのは難しそうです。

そのため、多くの場合は、「あの追加したコードでエラーが発生するということは、`do_something()`を３回呼ぶと失敗するバグがあるのか？」と`do_something/0`を調べに行くでしょう。
そこに問題は無さそうということになれば、スリープ時間を弄ったり、1回目や2回目の`do_something()`呼び出しを消してみたり、増やしてみたり等をやって、テストが通ったり通らなかったりするのを見て、どのコードに問題があるのかと時間を費やすわけです。

## 原因

これは **EUnitがテストを5秒で打ち切る** ために起きています。
そしてEUnitがテストを打ち切ると、上記のような **訳の分からないエラーメッセージ** を表示します。

これはほんとクソだと思うのですが、原因さえ分かってしまえばあとは簡単です。

## 修正

テストコードを以下のように修正します。

```erlang
foo() ->
    do_something(),
    timer:sleep(1000), % 処理を待つ
    ?assertEqual(read_something(), 10), % 処理結果を取得

    do_something2(),
    timer:sleep(2000),
    ?assertEqual(read_something2(), 20),

    do_something3(),
    timer:sleep(3000),
    ?assertEqual(read_something3(), 30),

    ok.

foo_test_() ->
    %% タイムアウトを7秒に設定する
    {timeout, 7, fun foo/0}.
```

まず、`foo_test()`を`foo()`という名前に変えています。
これは、テスト処理をテスト対象から外すためです。実際のテスト処理はテストジェネレータから呼ぶようにします。

そして `foo_test_()` というテストジェネレータを用意し、タイムアウトのパラメータを設定した上で `foo/0` を呼ぶように設定します。

`{timeout, 7, fun foo/0}` というのがテストのタイムアウト時間を設定する方法で、これでテスト時間を7秒まで延ばすことができます。

これでテストを実行すると、無事テストが通るはずです。
同じエラーが出るなら、更に時間を延ばしましょう。

## 応用

上記のテストで、タイムアウトを指定しない場合、単に `foo_test_() -> fun foo/0.` と書くだけで構いません。
つまり `Tests` を `{timeout, N, Tests}` に置き換えることで、タイムアウトの設定ができます。

タイムアウトの他にもいくつかオプションがありますが、このような置き換えをするだけで対応できます。
例えば子プロセスを起動してからテストを実行する `spawn` とタイムアウトと混ぜて使うなら、以下のようになります。

```erlang
foo_test_() ->
    {timeout, 7,
     {spawn, fun foo/0}}.
```

さらにテストジェネレータは複数のテストを書けるので、以下のように書けます。

```erlang
foo_test_() ->
    [{timeout, 7,
      {spawn, fun foo/0}},
     {timeout, 7,
      {spawn, fun foo/0}}].
```

さらにこれらを並列に実行するようなオプション `inparallel` を指定すると、以下のようになります。

```erlang
foo_test_() ->
    {inparallel, [{timeout, 7,
                   {spawn, fun foo/0}},
                  {timeout, 7,
                   {spawn, fun foo/0}}]}.
```

そして更に `setup` で開始時/終了時に呼ぶ関数を指定したりなんかもすると、以下のようになります。

```erlang
foo_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {inparallel, [{timeout, 7,
                    {spawn, fun foo/0}},
                   {timeout, 7,
                    {spawn, fun foo/0}}]}}.
```

このように様々なオプションを組み合わせられるので、EUnitのドキュメントを読み、いろいろ組み合わせて使うといいでしょう[^1]。

[^1]: 正直ドキュメントの説明は分かりにくい（特に英語読むの辛い人には）

## 参考

- [Erlang -- EUnit - a Lightweight Unit Testing Framework for Erlang](http://erlang.org/doc/apps/eunit/chapter.html)
