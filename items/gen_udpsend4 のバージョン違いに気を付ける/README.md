## `gen_udp`は遅い

`gen_udp`は、仕組み上とても遅くなりがちです。
というか普通に書いていると、間違いなく`gen_tcp`より遅くなります。

というのも、`gen_udp`は仕組み上、ソケットを１つしか持てないからです。
`gen_tcp`は`gen_tcp:accept/{1,2}`でコネクション毎にソケットを持てますが、`gen_udp`のソケットは`gen_udp:open/{1,2}`で開いた1ソケットだけです。

このソケットは`erlang:open_port/2`で開いていて、これはErlangプロセスを経由してPortドライバとやり取りするため、1ソケットにつき1プロセスの処理能力までしか処理できません。

更に `gen_udp:send/4` は、`erlang:port_command/3` で送信した後、そのソケットが送信完了するのを `receive` で待っています。
そのためソケットのプロセスで時間が掛かると、`gen_udp:send/4` を呼び出すプロセスが次々とブロックしていく可能性が高くなっていきます。

で、その辺は [gen_udp/tcp:send がボトルネックなときにやること](http://qiita.com/mururu/items/9b77e49b5b8a2815ceb6) に書いているように、`gen_udp:send/4` を非同期にすると、呼び出し元がブロックするという問題が解決します。

`prim_inet.erl` の実装を読みながら、以下のように書くだけです。

```erlang
-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

encode_ip_and_port({A,B,C,D}, Port) ->
    [?int16(Port),
     A band 16#ff, B band 16#ff,
     C band 16#ff, D band 16#ff].

encode_ip_and_port({A,B,C,D,E,F,G,H}, Port) ->
    [?int16(Port),
     ?int16(A), ?int16(B), ?int16(C), ?int16(D),
     ?int16(E), ?int16(F), ?int16(G), ?int16(H)].

-spec send(gen_udp:socket(), iodata(), inet:ip_address() | inet:hostname(), inet:port_number()) -> ok.
send(Socket, Binary, Host, Port) ->
    Data = [encode_ip_and_port(Host, Port), Binary],
    port_command(Socket, Data, []),
    ok.
```

## Erlang/OTP 19で動かない問題

上記のコードは、Erlang/OTP 19で動きません。
これは微妙に`erlang:port_command/2`で送るフォーマットが変わったためです。
Erlang/OTP 19で動かすには以下のようなコードにする必要があります。

```erlang
-define(INET_AF_INET,         1).
-define(INET_AF_INET6,        2).
-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

encode_ip_and_port({A,B,C,D}, Port) ->
    [?INET_AF_INET, ?int16(Port),
     A band 16#ff, B band 16#ff,
     C band 16#ff, D band 16#ff].

encode_ip_and_port({A,B,C,D,E,F,G,H}, Port) ->
    [?INET_AF_INET6, ?int16(Port),
     ?int16(A), ?int16(B), ?int16(C), ?int16(D),
     ?int16(E), ?int16(F), ?int16(G), ?int16(H)].

-spec send(gen_udp:socket(), iodata(), inet:ip_address() | inet:hostname(), inet:port_number()) -> ok.
send(Socket, Binary, Host, Port) ->
    Data = [encode_ip_and_port(Host, Port), Binary],
    port_command(Socket, Data, []),
    ok.
```

先頭に `?INET_AF_INET` か `?INET_AF_INET6` が書き込まれるようになりました。
これは多分、Erlang/OTP 19でUNIXドメインソケットに対応したためだと思います。

`gen_udp:send/4` を非同期化する際にはバージョンの違いに注意しましょう。
