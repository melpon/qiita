[cowboy](https://ninenines.eu/) は、HTTP2 や WebSocket に対応してる HTTP サーバです。
cowboy に触る機会があったので、その時に得た知見を纏めてみました。

cowboy のバージョンには 1.0 系と 2.0 系がありますが、（この記事公開時にはまだ公式にはリリースされてない方の）2.0 系を使いました。

## 1モジュールに複数のハンドラを書く

cowboy の簡単なサンプルを見ると、以下のように、1つのURLに対して1モジュールを書いているのが多くあります。

```erlang
Dispatch = cowboy_router:compile([
        {'_', [
            {"/", myapp_handler, []}
        ]}
    ]),
```
```erlang
-module(myapp_handler).

...

init(Req0, Opts) ->
    ...
    {ok, Req, Opts}.
```

そのため URL 1個に対して1ファイルずつ分ける必要があるのかと思っていました。
が、全くそんなことはなく、複数のURLを1ファイルで受けることができました。

URL指定の3番目のタプルは、`init/2`の第二引数に渡される値になります。
なのでこれでパターンマッチしてやれば、1ファイル内に複数のハンドラを書くことができます。

```erlang
Dispatch = cowboy_router:compile([
        {'_', [
            {"/", myapp_handler, root},
            {"/foo", myapp_handler, foo},
            {"/bar/piyo", myapp_handler, {bar, piyo}},
            {"/bar/hoge", myapp_handler, {bar, hoge}}
        ]}
    ]),
```
```erlang
-module(myapp_handler).

...

init(Req0, root) ->
    ...
init(Req0, foo) ->
    ...
init(Req0, {bar, Name}) ->
    ...
    bar(Name),
    ...

bar(piyo) ->
    ...
bar(hoge) ->
    ...
```

こうすれば無事1ファイルで複数のURLを処理できるようになります。

更に`init/2`の3番目のパターンマッチで `{bar, Name}` のようにすることで、複数のURLを1つのハンドラで受けることもできます。
こうすることで、共通の処理をした後、分岐して更に処理を続けるといったことも簡単に書けます。

## Constraintsを使う

ディスパッチャのURL指定の際にはパラメータを書けます。

```erlang
Dispatch = cowboy_router:compile([
        {'_', [
            {"/:name/[:id]", myapp_handler, root}
        ]}
    ]),
```
```erlang
-module(myapp_handler).

...

init(Req0, root) ->
    Name = cowboy_req:binding(name, Req0),
    case cowboy_req:binding(id, Req0) of
        undefined -> ...
        Id -> ...
    end,
    {ok, Req, []}.
```

`:name` や `:id` で名前付きのパラメータになり、`[]` で囲むことでオプショナルな指定になります。
この指定したパラメータは `cowboy_req:binding/{2,3}` を使って取り出します。
3引数版は値が存在しなかった場合のデフォルト値を指定できます。

ただし、このままだと上記コードの `Name` や `Id` は全てバイナリ文字列になります。
もし `Id` に数字のみしか認めないのであれば Constraints を使うのがいいでしょう。
以下のように指定できます。

```erlang
Dispatch = cowboy_router:compile([
        {'_', [
            {"/:name/[:id]", {id, int}, myapp_handler, root}
        ]}
    ]),
```

URL指定の1番目と2番目の間に `{id, int}` を追加しました。
これで `id` というパラメータを整数にしてくれます。

このようにパラメータを変換してくれるのが Constraints です。
組み込みでは、整数に変換する `int` と空文字列でないことを保証する `nonempty` の2つの Constraints があります。
また、引数の数が1つの関数を渡すと、その関数を使って変換してくれます。

```erlang
Dispatch = cowboy_router:compile([
        {'_', [
            %% id が "foo" だったら、cowboy_req:binding(id, Req) で取得できる値は 1000 になる。
            %% id が "foo" 以外だったら、この URL にはマッチしない
            {"/:name/[:id]",
             [{id, fun (V) -> case V == "foo" of
                                  true -> {true, 1000};
                                  false -> false
                              end
                   end}],  % ここのリストは1要素しか無いなら省略可能
             myapp_handler,
             root}
        ]}
    ]),

```

また、1つのパラメータに複数の Constraints を指定することもできます。

```erlang
{id, [nonempty, int]}
```

こんな風にいろいろ書けるので、一度[ドキュメント](https://ninenines.eu/docs/en/cowboy/2.0/guide/constraints/)を見ておくといいでしょう。

## middlewareを使ってBasic認証を行う

全てのアクセスに対して何らかの変換を掛けたり制限を付けたい場合、通常のハンドラで書くのはかなり面倒なことになります。
middleware機能を使えば、これを簡単に行うことができます。

- [Nine Nines: Middlewares](https://ninenines.eu/docs/en/cowboy/2.0/guide/middlewares/)

middlewareは、 `cowboy_middleware` ビヘイビアである `execute/2` を実装しているモジュールのことを指します。
ここで、アクセスを許可したり拒否したり、あるいはヘッダを弄ったりといったこともできます。

```erlang
-module(myapp_middleware).

-behaviour(cowboy_middleware).

%% cowboy_middleware
-export([execute/2]).

execute(Req0, _Env) ->
    Req = cowboy_req:reply(451, [], <<>>, Req0),
    {stop, Req}.
```

これは、あらゆるアクセスに対して問答無用で 451 エラーを返すmiddlewareです。
`execute/2` の有効な戻り値は以下の３つがあります。

- `{ok, Req, Env}` は、リクエストの処理を続行します。`Req`と`Env`が次のmiddlewareに渡されます。
- `{suspend, Module, Function, Args}` は、`erlang:hibernate/3`を呼び出してハイバネート状態に移行します。復帰時に指定したMFA（この関数の戻り値は `execute/2` と同じ仕様にする必要がある）が呼ばれます
- `{stop, Req}` は、リクエストの処理を中断します。次のmiddlewareは呼び出されません。

このmiddlewareを以下のようにして組み込むことで、ハンドラが呼び出される前に `myapp_middleware:execute/2` が呼ばれます。

```erlang
Port = 8000,
AcceptorNum = 100,
Dispatch = ...,
{ok, _Pid} = cowboy:start_http(myapp_listener, AcceptorNum,
                               [{port, Port}],
                               [{env, [{dispatch, Dispatch}]},
                                {middlewares, [cowboy_router, myapp_middleware, cowboy_handler]}]).
```

`{middlewares, [cowboy_router, myapp_middleware, cowboy_handler]}` という指定で `myapp_middleware` を組み込んでいます。
`cowboy_router` は URL を見てディスパッチするmiddlewareで、`cowboy_handler` は対応するハンドラを呼び出すmiddlewareです。
これらは、`middlewares`オプションを指定しなければデフォルトで入っているmiddlewareになります。
つまりURLを見てディスパッチする部分も、ハンドラを呼び出す部分もカスタマイズ可能です。

まあそれをカスタマイズすることはあまり無いだろうから置いといて、ひとまずこのmiddlewareを使ってBASIC認証を組み込んでみましょう。
とても簡単です。

```erlang
-module(myapp_middleware).

-behaviour(cowboy_middleware).

%% cowboy_middleware
-export([execute/2]).

-spec is_authorized(Req) -> {true, Req} | {false, Req} when Req::cowboy_req:req().
is_authorized(Req0) ->
    % これはsys.configあたりで指定できるようにしておくと良い
    Authinfo = [{<<"melpon">>, <<"seirenkeppaku">>},
                {<<"ushi">>, <<"usshisshi-">>}]
    Realm = <<"need authentication">>

    Authenticated = case cowboy_req:parse_header(<<"authorization">>, Req0) of
                        {basic, User, Password} -> lists:member({User, Password}, Authinfo);
                        _ -> false
                    end,
    case Authenticated of
        true -> {true, Req0};
        false ->
            Req = cowboy_req:reply(401, [{<<"www-authenticate">>, <<"Basic realm=\"", Realm/binary, "\"">>}], <<>>, Req0),
            {false, Req}
    end.

execute(Req0, Env) ->
    case is_authorized(Req0) of
        {true, Req} -> {ok, Req, Env};
        {false, Req} -> {stop, Req}
    end.
```

`cowboy_req:parse_header/2` を使うことで、入力されたユーザIDとパスワードが分かるので、あとはそれと比較しているだけです。
失敗した場合は 401 を返し、成功したら次のmiddlewareに処理を渡しています。

なお `cowboy_req:parse_header/2` でパース可能な Authorization ヘッダは Basic, Digest, Bearer だけです。
その他の認証の場合は自前でパースする必要があります。

このようにmiddlewareを利用することで簡単にURLを横断した処理が書けるので、有効に活用していきましょう。
