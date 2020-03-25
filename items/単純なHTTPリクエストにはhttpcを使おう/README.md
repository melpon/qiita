Erlangには標準のHTTPクライアントとして [httpc](http://erlang.org/doc/man/httpc.html) があります。

これを使えば簡単にHTTPでデータを受信できます。

## 簡単な使い方

`applicaion:start(inets)`でアプリケーションを起動しておいて、`httpc:request/1`でリクエストを投げれば、GETリクエストを適当に送信します。

```erlang
1> application:start(inets).
ok
2> httpc:request("http://example.com/").
{ok,{{"HTTP/1.1",200,"OK"},
     [{"cache-control","max-age=604800"},
      {"date","Sun, 18 Dec 2016 10:55:37 GMT"},
      {"accept-ranges","bytes"},
      {"etag","\"359670651\""},
      {"server","ECS (rhv/818F)"},
      {"vary","Accept-Encoding"},
      {"content-length","1270"},
      {"content-type","text/html"},
      {"expires","Sun, 25 Dec 2016 10:55:37 GMT"},
      {"last-modified","Fri, 09 Aug 2013 23:54:35 GMT"},
      {"x-cache","HIT"},
      {"x-ec-custom-error","1"}],
     "<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n..."}}
```

`httpc:request(Url)`は単純に`httpc:request(get, {Url, []}, [], [])`を呼び出しているだけなので、メソッドやオプションをカスタマイズしたい場合は `http:request/4` あるいは `http:request/5` を呼び出す必要があります。

個人的によく使うオプションは`body_format`と`full_result`です。

## body_format

`httpc:request/1`はレスポンスボディをリストで返すので、ちょっと微妙な気分になります。[^1]

[^1]: しかも実装を見ると、レスポンスボディを一度バイナリで受け取った後に `binary_to_list/1` で変換して返しているので、効率的にはとても良くない

そういうときは `httpc:request/4` を使います。
`{body_format, binary}` というオプションを指定することで、レスポンスボディをバイナリで受け取ることができます。

```erlang
3> httpc:request(get, {"http://example.com/", []}, [], [{body_format, binary}]). 
{ok,{{"HTTP/1.1",200,"OK"},
     [{"cache-control","max-age=604800"},
      {"date","Sun, 18 Dec 2016 11:49:36 GMT"},
      {"accept-ranges","bytes"},
      {"etag","\"359670651\""},
      {"server","ECS (rhv/818F)"},
      {"vary","Accept-Encoding"},
      {"content-length","1270"},
      {"content-type","text/html"},
      {"expires","Sun, 25 Dec 2016 11:49:36 GMT"},
      {"last-modified","Fri, 09 Aug 2013 23:54:35 GMT"},
      {"x-cache","HIT"},
      {"x-ec-custom-error","1"}],
     <<"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-8\" />\n  "...>>}}
```

## full_result

`http:request/4`の結果は`{ok, {status_line(), headers(), Body}}`という形式になっていますが、レスポンスヘッダや`"HTTP/1.1"`のプロトコルバージョンが不要な場合には`{full_result, false}`が使えます。

```erlang
4> httpc:request(get, {"http://example.com/", []}, [], [{body_format, binary}, {full_result, false}]).
{ok,{200,
     <<"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-8\" />\n    <m"...>>}}
```

とてもシンプルな戻り値になります。
テスト用途で使うなら、この形式で十分でしょう。

## POST

POSTで送信する場合は以下のようにします。

```erlang
5> httpc:request(post,
5>               {"http://example.com/",
5>                [],
5>                "application/json",
5>                <<"{\"foo\": 100}">>},
5>               [],
5>               [{body_format, binary},
5>                {full_result, false}]).
{ok,{200,
     <<"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-8\" />\n    <m"...>>}}
```

POST の場合は `httpc:request/4` の２番目の引数に `{url(), headers(), content_type(), body()}` という形式で渡します。
`body()`には文字列だったりバイナリだったり、あとは関数を渡してチャンクデータを送信することもできるようです。

## まとめ

httpcは、ドキュメントを読むと意外（？）と多機能で、例えばクッキーやセッションの情報を保存してくれていたり、その保存先としてデフォルトではデフォルトプロファイルが使われますが、プロファイルを変えればそちらに保存したりもできます。

ただ、httpcにはコネクションプールが無いし、HTTP1.0や1.1にしか対応していないため、高頻度でリクエストを投げるような処理には向いていないと思います。

しかし標準で既に入っているという点はとても強いので、何らかのHTTPサーバを実装した際にテスト用に使うとか、数十秒に1回程度の頻度でネットワーク上にある設定を取得するために使うとか、そういうのに使うといいと思います。
