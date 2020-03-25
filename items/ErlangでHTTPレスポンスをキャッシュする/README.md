HTTP経由で何らかのデータを読み込み、一定時間経ったらキャッシュアウトして再度取得する、という（多分）よくある実装について考えます。

## ETSを使う

一番簡単なのは、ETSに入れることです。

```erlang:cache_ets.erl
-module(cache_ets).

-export([init/0, get/0]).

-define(TAB, cache_tab).
-define(KEY, cache_key).
-define(CACHE_TIMEOUT, 60 * 1000).

init() ->
  ets:new(?TAB, [public, named_table]),
  update().

get() ->
  case ets:lookup(?TAB, ?KEY) of
    [{?KEY, ExpiredAt, Value}] ->
      %% 期限切れかどうかの確認
      Now = erlang:monotonic_time(millisecond),
      case ExpiredAt < Now of
        true -> update();
        false -> Value
      end
  end.

update() ->
  %% HTTP経由で値を取得する
  Value = do_process(),
  %% キャッシュの期限切れの時間を書いて格納する
  ExpiredAt = erlang:monotonic_time(millisecond) + ?CACHE_TIMEOUT,
  ets:insert(?TAB, {?KEY, ExpiredAt, Value}),
  ok.
```

しかしこれには問題があります。
HTTPでの問い合わせで大量アクセスが発生したり、あるいは古い値で上書きする可能性があります。

例えばプロセスが10,000個ぐらいあり、それらがそこそこ高い頻度で`get/0`を呼んでいるとします。
この時、以下のようになる場合があります。

1. 期限切れになった後にあるプロセスが`get/0`を呼び出すと、期限切れと判断されて`update/0`の中に入り、このプロセスがHTTP通信を開始する
2. HTTP通信は時間が掛かるので、その間に他のプロセスは`get/0`を呼び出すが、これも更新切れと判断され、このプロセスもHTTP通信を開始する
4. 同様に残り9,998個のプロセスがHTTP通信を開始する

つまり最大で10,000リクエストが同時に発生する可能性があります。
HTTPサーバに負荷を掛けたくないがためのキャッシュなのに、これだと意味がありません。

更に、以下のようなケースも考えられます。

1. 期限切れになった後にプロセスAが`get/0`を呼び出すと、期限切れと判断されて`update/0`の中に入り、プロセスAがHTTP通信を開始する
2. プロセスAが値*foobar*を受け取る
3. 管理者がHTTPサーバの設定を変えて、値*hogefuga*になるように変更する
4. プロセスBが`get/0`を呼び出し、期限切れと判断されて`update/0`の中に入り、プロセスBがHTTP通信を開始する
5. プロセスBが値*hogefuga*を受け取る
6. プロセスBが`ets:insert/2`を呼び出して値*hogefuga*を書き込む
7. プロセスAが`ets:insert/2`を呼び出して値*foobar*を書き込む

こうなると、プロセスAの持っていた古い値が書き込まれます。
これは次のキャッシュタイムアウトの更新で直る可能性もあるし、直らない可能性もあります。
いつ反映されるか分からないとなると、少し困るかもしれません。

これはどちらも、`ets:lookup/2`から`ets:insert/2`を呼び出すまでの間に別の処理が入っていることに起因します。
つまり、`ets:lookup/2`から`ets:insert/2`を呼び出すまでの間は、ロックされていて欲しいのです。

## プロセスを使う

Erlangでロックと言えばプロセスです。
以下のような実装にしてみましょう。

```erlang:cache_proc.erl
-module(cache_proc).

-export([get/0]).
-export([init/1, handle_call/3, ...]).

-define(CACHE_PROC, __MODULE__).
-behaviour(gen_server).

init([]) ->
  cache_ets:init(),
  {ok, {}}.

handle_call(get, _From, State) ->
  {reply, cache_ets:get(), State}.

...

get() ->
  gen_server:call(?CACHE_PROC, get).
```

プロセスを使って、`get/0`の処理全体をロックします。
これによって、`cache_ets`モジュールの問題を解決します。

しかし1プロセスで処理するため、今度はスケールしなくなります。
少なくともETSから取得するよりは遅くなるでしょう。

期限切れが発生した時には仕方ないにしても、少なくともキャッシュから取り出す時ぐらいはETSにアクセスするのと同程度の速度を出したい、と思ったので、もう少し頑張ってみました。

## ETSとプロセスを使う

```erlang:cache_dcl.erl
-module(cache_dcl).

-export([get/0]).

get() ->
  case ets:lookup(?TAB, ?KEY) of
    [{?KEY, ExpiredAt, Value}] ->
      %% 期限切れかどうかの確認
      Now = erlang:monotonic_time(millisecond),
      case ExpiredAt < Now of
        true -> gen_server:call(?CACHE_PROC, get);
        false -> Value
      end
  end.
```

まず`get/0`で`ets:lookup/2`を実行して、期限切れかどうかを調べます。
もし期限切れじゃなかった場合はそのまま値が返します。
こうすることで、`cache_ets:get/0`の実装とほぼ同等の速度が出るはずです。

もし期限切れだった場合は、単に`gen_server:call(?CACHE_PROC, get)`を実行するだけです。

`cache_proc.erl`のバージョンと違うのは、事前に`ets:lookup/2`して期限切れの確認を行うかどうかだけです。

こうすることで、以下のようになります。

1. プロセスAが`get/0`を実行して、期限切れで更新依頼を投げる
2. プロセスBが`get/0`を実行して、期限切れで更新依頼を投げる
3. 同様に9,998プロセスが更新依頼を投げる
4. `?CACHE_PROC`で更新依頼を受けて、`cache_ets:get/0`でもう一度期限切れの確認を行う
5. 期限切れになっているのでHTTPリクエストを投げて期限と値を更新して呼び出し元に値を返す
6. `?CACHE_PROC`で更新依頼を受けて、`cache_ets:get/0`でもう一度期限切れの確認を行う
7. 期限切れではないので、そのまま呼び出し元に値を返す
8. 同様に9,998プロセスに値を返す

このように複数のプロセスで期限切れがほぼ同時に発生しても、HTTPリクエストを投げるのは１回だけです。

ただしこの実装は、HTTPリクエストを投げて結果を取得している間は、その間に`get/0`を呼び出した全てのプロセスが`gen_server:call/2`で待つことになります。
もしこの待ち時間が許容できない場合は、もっと別の方法を考える必要があります。

## 考察

なぜこれでうまく動くのかというと、

- 最終的には、`?CACHE_PROC`プロセスが期限切れの確認とETSの更新を行っている
- `get/0`で実行する期限切れの確認は、**実際は更新が不要でも更新依頼を投げて構わない** [^1]

[^1]: 偽陽性(false positive)と言いますが、正直、偽陰性(false negative)とどっちがどっちか分からなくなるので誰か間違えない覚え方教えてください

からです。

「実際は更新が不要」というのは「既に別プロセスからの依頼で`?CACHE_PROC`が更新処理を実行しているから、他のプロセスから依頼を投げなくても、待ってればそのうち値が更新される」ということを意味します。

今回のコードだと、過剰に更新依頼を投げる可能性はありますが、最終的な期限切れの確認は全て`?CACHE_PROC`プロセスが行なっているため、間違った更新依頼がいくら来ても、`cache_proc.erl`と同程度には安全に動きます。

最初のチェックはロック（プロセス）の外で行って、そこを抜けたらロックを取り、再度チェックをして更新を行う、という手法は [Double-checked locking](https://en.wikipedia.org/wiki/Double-checked_locking) に相当します。
なぜかシングルトンパターンでしか見かけないし、[書くのが非常に難解だし大体言語側が何とかしてくれる](http://yamasa.hatenablog.jp/entry/20100128/1264693781) という残念な手法ですが、Erlangではリオーダリングの問題が発生しない（はず）なので、大丈夫でしょう。
