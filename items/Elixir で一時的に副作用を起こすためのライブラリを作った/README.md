Elixir で複雑なロジックを書いてると、一時的に代入したり書き換えたりしたくなることがあります。

例えばこんなコードがあったとします。

```elixir
def f(xs, ys) do
  for x <- xs do
    for y <- ys do
      if writable?(x, y) do
        write_to_db(x, y)
      end
    end
  end
  :ok
end
```

これは、「`xs` と `ys` の組み合わせを、条件を満たした場合だけ `write_to_db/2` で書き込む」という処理であるのがすぐに分かります。

この時、仕様が変わり、「この関数の戻り値として、最後に `write_to_db/2` した戻り値が欲しい」と言われた場合、かなりコードを書き換えることになります。
以下のようになるでしょう。

```elixir
def f(xs, ys) do
  Enum.reduce(xs, nil, fn {x, last_written_data} ->
    Enum.reduce(ys, last_written_data, fn {y, last_written_data} ->
      if writable?(x, y) do
        write_to_db(x, y)
      else
        last_written_data
      end
    end
  end
end
```

`Enum.reduce/3` が出てきて、一気に複雑になってきた気がします。
そもそも `Enum.reduce/3` は **大体なんでも出来る** 関数なので、ほぼ再帰するのと変わらないぐらいの力を持ちます。このような汎用的な関数はあまり使いたくないところです。[^1]

[^1]: [foldr のすごさを体験してみた](http://d.hatena.ne.jp/melpon/20111005/1317799929) で、Haskell の `foldr` という `Enum.reduce/3` 相当の関数を使って他の様々な関数を実装したことがあります。

副作用が使えるなら、`write_to_db/2` した戻り値を代入しておいて、最後にそれを返すだけで済むのに、ここまでコードが変わるのはなかなか辛いものがあります。

そのため、その関数内だけでいいから、一時的に副作用を許可して欲しい、と思うことがあるのです。
なので、そういった場合に使えるライブラリを作りました。

## mutable ライブラリ

- [Hex](https://hex.pm/packages/mutable)
- [GitHub](https://github.com/melpon/mutable)

使い方は簡単で、`Mutable.put/2` で値を設定して、`Mutable.get/1` で値を取得するだけ。取得と更新を同時に行う `Mutable.update/2` もあります。
ただしこれらは `Mutable.run/2` の中だけでしか使えないようになっています。

```elixir
Mutable.run([x: 10], fn ->
  assert 10 == Mutable.get(:x)
  Mutable.put(:x, 20)
  assert 20 == Mutable.get(:x)
  Mutable.update(:x, &(&1 + 1))
  assert 21 == Mutable.get(:x)
end)
# エラー: ここでは :x にアクセスできない
Mutable.get(:x)
```

これを使うと、先程のコードは以下のようになります。

```elixir
def f(xs, ys) do
  Mutable.run([last_written_data: nil], fn ->
    for x <- xs do
      for y <- ys do
        if writable?(x, y) do
          data = write_to_db(x, y)
          Mutable.put(:last_written_data, data)
        end
      end
    end
    Mutable.get(:last_written_data)
  end)
end
```

最初のコードにちょっと付け足しただけです。`Enum.reduce/3` で頑張って値を上に渡すより、こっちの方が分かりやすいし、お手軽です。

## プロセス辞書とmutable

mutable の実装としては、単純に [`Process.get/2`](https://hexdocs.pm/elixir/Process.html#get/2) や [`Process.put/2`](https://hexdocs.pm/elixir/Process.html#put/2) といった関数を呼んでプロセス辞書を読み書きしているだけです。
そのため、これらの関数を直接使えば、上記のようなコードは簡単に書けます。

それなのに、なぜ mutable ライブラリを作ったかというと、

- プロセス辞書を使っている場合、一時的な副作用が欲しいだけなのかどうかがすぐに分からない。関数の外に情報を渡そうとしている可能性もある。
- プロセス辞書の場合、そのデータがスコープに閉じない。データを消すことを忘れていた場合、意図しない場所で問題になる可能性もあるし、メモリも無駄である。
- プロセス辞書の場合、キーの名前を間違えてた場合にデフォルト値が返されてしまう。これはバグを発見するのが遅れるので良くない。

mutable なら、使っていればすぐに一時的な副作用が欲しいということが分かるし、`Mutable.run/2` の外に抜ければ副作用のために使ったデータは消えるし、初期化時に渡した値以外のキーを使おうとしたらエラーになります。

複雑なロジックを前に、再帰や `Enum.reduce/3` を駆使して作るのもありですが、こういったやり方で解決するのも手です。
