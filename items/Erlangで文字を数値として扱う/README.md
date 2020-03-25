Erlangでは時々、記号やアルファベット1文字を数字として書きたいことがあります。

例えば以下のようなコード。

```erlang
is_fullpath(Path) ->
  case binary:first(Path) of
    47 -> true;  % '/' は ASCII コードで 47
    _ -> false
  end.
```

`47`とかいうマジックナンバーが出てて酷い。
マクロを使うという手もありますが、何とか別の方法で書けないかと調べたら、ありました。

```erlang
is_fullpath(Path) ->
  case binary:first(Path) of
    $/ -> true;
    _ -> false
  end.
```

`$/`とか`$a`とか書くことで１文字を表せるようです。知らなかった…。

なおこの機能、日本語を使うこともできます。

```erlang
1> $あ.
12354
```

Unicodeのコードポイント値になります。
