[Meck](https://github.com/eproxus/meck) はErlangのmockライブラリです。

この使い方は [meck コトハジメ](https://gist.github.com/voluntas/4243779) を見てもらうとして、ここではMeckを使っている状態でオリジナルの関数を呼ぶ方法について書きます。

## オリジナルの関数を呼び出す

例えば `hackney:request/1` 呼び出しで、特定のURLが渡された場合だけmockの値を返し、それ以外の場合にオリジナルの関数を呼び出したいことがあります。

`meck:init/2` 呼び出し時に `passthrough` オプションを指定することで、mock化していない関数を呼び出した場合にオリジナルの関数を呼び出してくれますが、これは引数の値によって分岐してくれないので意味がありません。

そこで `meck:passthrough/1` です。
`meck:passthrough/1` を使えば、これを実現できます。

```erlang
Url = <<"http://example.com">>,
meck:new(hackney),

% "http://example.com" にアクセスした場合だけ特定の戻り値を返す。
% それ以外の URL は元の関数を呼び出す。
meck:expect(hackney, request, fun (Url) -> {ok, 200, [], client_ref};
                                  (OtherUrl) -> meck:passthrough([OtherUrl]) end),
```

なお `meck:passthrough/1` は `meck:expect/3` の関数内でしか使えません。
`meck:expect/3` の外でオリジナルの関数を呼ぶ方法は多分無いと思います。
