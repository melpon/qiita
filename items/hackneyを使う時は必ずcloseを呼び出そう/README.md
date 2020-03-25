[hackney](https://github.com/benoitc/hackney) は簡単に扱える HTTP クライアントですが、ちょっとした注意点があります。

## 実験

```erlang
1> hackney:request(get, "http://google.co.jp/").
{ok,301,
    [{<<"Location">>,<<"http://www.google.co.jp/">>},
     {<<"Content-Type">>,<<"text/html; charset=UTF-8">>},
     {<<"Date">>,<<"Tue, 13 Dec 2016 10:07:57 GMT">>},
     {<<"Expires">>,<<"Thu, 12 Jan 2017 10:07:57 GMT">>},
     {<<"Cache-Control">>,<<"public, max-age=2592000">>},
     {<<"Server">>,<<"gws">>},
     {<<"Content-Length">>,<<"221">>},
     {<<"X-XSS-Protection">>,<<"1; mode=block">>},
     {<<"X-Frame-Options">>,<<"SAMEORIGIN">>}],
    #Ref<0.0.4.825>}
2> hackney:request(get, "http://google.co.jp/").
...
3> hackney:request(get, "http://google.co.jp/").
...
4> hackney:request(get, "http://google.co.jp/").
...
%% 以下大量に投げる（やり過ぎ注意）
```

この状態で別のコンソールを開き、以下のコマンドを入れます。

```bash
$ netstat -anv | grep ESTABLISHED | grep '\.80'
tcp4       0      0  192.168.14.244.52548   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52547   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52546   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52545   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52544   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52543   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52542   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52541   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52540   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52539   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52538   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52537   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52536   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52535   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52534   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52533   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52532   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
tcp4       0      0  192.168.14.244.52521   216.58.220.227.80      ESTABLISHED 131330 131874   5759      0
```

大量にソケットが開きっぱなしになっています。
これを放置しておくとリソースリークになるし、サーバの負担にもなるので何とかする必要があります。

## 原因

`hackney:request/{1-5}`は、レスポンスボディをまだ読んでいない状態で返されます。
そのため、hackneyのソケットは開きっぱなしの状態になります。

`hackney:request/{1-5}`は、`{ok, StatusCode, RespHeader, ClientRef}` という４タプルを返します。
この `ClientRef` を使って `hackney:body(ClientRef)` することで初めてレスポンスボディが読まれます。

もし`hackney:body(ClientRef)`で全てのデータが読み込めたら、ソケットを閉じます。[^1]

[^1]: Keep-Aliveが設定されてる場合はcloseされないっぽいけど、これがどこで切られてるのかよく分からなかったので割愛

また、hackneyでは、`hackney:request/{1-5}`を呼び出したプロセス上でソケットを開きます。
そのため、呼び出したプロセスが落ちない限りは、ソケットが勝手に閉じたりはしません。

## 対策

つまり、以下のどれかを行う必要があります。

- `hackney:request/{1-5}`の後、全てのデータを`hackney:body/1`で読み込む
- `hackney:request/{1-5}`の後、必ず`hackney:close/1`を呼び出す
- `hackney:request/{1-5}`の後、プロセスを落とす

`hackney:close/1`を呼ぶのが普通の対応でしょう。
以下のようになります。

```erlang
case hackney:request(get, Url) of
    {ok, StatusCode, Headers, ClientRef} ->
        try
            case StatusCode div 100 of
                %% 200系の処理
                2 -> {ok, Body} = hackney:body(ClientRef),
                     {ok, Body};
                %% それ以外の処理
                _ -> {error, {bad_status_code, StatusCode}}
            end
        after
            hackney:close(ClientRef)
        end;
    {error, Reason} -> {error, Reason}
end.
```

よくあるコードでは、以下のように書いていたりしますが、これは間違いです。

```erlang
%% リークする可能性のあるコード
case hackney:request(get, "...") of
    %% 200 OK以外は興味が無いのでパターンマッチでクラッシュさせる
    {ok, 200, Headers, ClientRef} ->
        {ok, Body} = hackney:body(ClientRef),
        hackney:close(ClientRef),
        {ok, Body};
end.
```

もしこのパターンマッチのエラーを誰かが拾っていた場合、200以外のレスポンスが返ってきた際にソケットが開きっぱなしになります。

hackneyを使う際には`hackney:close/1`を呼ぶコードが入っているか確認するようにしましょう。
