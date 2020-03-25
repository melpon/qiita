Mnesiaを使う際には、基本的にレコードを使って格納したり検索したりします。

例えば以下のようなレコードを定義して、

```erlang
-record(myapp_record, {
          node :: node(),
          key :: string(),
          value :: any()
         }).
```

以下のように検索を行います。

```erlang
F = fun() ->
            %% 全件検索
            MatchHead = #myapp_record{_='_'},
            Result = '$_',
            mnesia:select(myapp_record, [{MatchHead, [], [Result]}])
    end,
Results = mnesia:activity(transaction, F),
```

これは実行する時には全く問題のないコードです。

しかし、この状態で dialyzer に掛けると、

- 「`node()`型であるはずの`#myapp_record.node`に`'_'`アトムを入れてるよ！」
- 「`string()`型であるはずの`#myapp_record.key`に`'_'`アトムを入れてるよ！」

という警告が出ます。[^1]

[^1]: `value`は`any()`型でありアトムを含むので警告は出ない。

なぜなら `mnesia:select/2` で検索する時に `#myapp_record{_='_'}` という検索用のレコードを作っていて、これによってレコードの各フィールドに `'_'` アトムが設定されるからです。[^2]

[^2]: `'_'` を書きすぎて `'_'` が顔にしか見えなくなってきた

レコードは単なるタプルなので、以下のように書くことで警告を回避できます。

```erlang
MatchHead = {myapp_record, '_', '_', '_'}
```

しかし、これはフィールドの数が増えたり順序が変わった時にこの行も変更する必要があるので、やめておいた方がいいでしょう。

[Record typing for dialyzer](http://erlang.org/pipermail/erlang-questions/2009-November/047780.html) にあるように、以下のように書くのが良さそうです。

```erlang
-type search_spec() :: '_' | '$1' | '$2' | '$3' | '$4' | '$5' | '$6' | '$7' | '$8' | '$9'.

-record(myapp_record, {
          node :: node() | search_spec(),
          key :: string() | search_spec(),
          value :: any() | search_spec()
         }).
```

このように、各フィールドに検索用のアトムが入ることを許可します。

なんか汚い解決方法だとは思いますが、`search_spec()` という名前があるおかげでまだマシかなと思います。

`ets`でレコードを格納、検索する際にも同様の問題が起きたりしますが、同様の方法で解決できます。
dialyzer は便利なので、うまく共存していきましょう。
