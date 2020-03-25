`mix phx.new` で Phoenix プロジェクトを生成すると、`config/` ディレクトリの中に `prod.secret.exs` というファイルが生成されます。

この `prod.secret.exs` は、不要なので **サクッと消しましょう**。これを有効に活用しようなんて考える必要はありません。

## なぜ `prod.secret.exs` は不要か

端的に言えば、パッケージ化した際に、このファイルに書いたシークレットな情報（以降シークレット情報と呼ぶ）が埋め込まれてしまうからです。[^1]

[^1]: 正確に言えば `sys.config` ファイルになるだけなので、これを弄れば実行時に書き換えることはできますが、それは Erlang term で書く必要があるのでこれも結構厳しい

[Elixir アプリケーションをパッケージ化しよう](https://qiita.com/melpon/items/d61907ae51307a55c39a) で書いた通り、通常、Phoenix を本番サーバにデプロイする際にはパッケージ化をします。
`prod.secret.exs` は、このパッケージ化の際に読み込まれ、パッケージ化したファイルにシークレット情報が埋め込まれます。

これにはいくつか問題があります。

### 設定を変えて起動するのが面倒

シークレット情報は本番で扱う情報なので、本番にデプロイしてから問題が発覚することがよくあります。
それを直すためには、`prod.secret.exs` を書き換えて再度パッケージ化する必要があるため面倒です。

### サーバ毎に異なる設定が書きにくい

サーバごとに異なるシークレット情報がある場合、サーバごとにシークレット情報を変えてパッケージ化する必要があります。
パッケージ１個を複数のサーバにデプロイするといったことが出来ません。

### 権限を分けるのが難しい

開発やパッケージ化やデプロイする人と、シークレット情報を知っていて構わない人が常に一緒であるとは限りません。

特定の人だけがシークレット情報を知っているようにするには、その人がパッケージ化をするか、`prod.secret.exs` に触れないようにしながらパッケージ化するための方法を提供する必要があります。
また、パッケージ化したデータにはシークレット情報が含まれているため、その人がデプロイするか、ファイルの中身を見られないようにしながらデプロイする方法を提供必要があります。

## 代替手段

いくつかありますが、一番良いのは環境変数を使うことでしょう。

環境変数以外の方法としては、`sys.config` を書き換えるか読み込むパスを変える、JSON や `.exs` 等の設定ファイルを作って実行時に読み込む等が考えられますが、いろいろ考えたり試行錯誤した結果、自分は環境変数が一番良いという結論に至りました。

環境変数を利用してシークレットな情報等の設定を読むライブラリとして [`env`](https://hex.pm/packages/env) があります。

以下のように設定を書くだけです。

```elixir
case Mix.env() do
  :test ->
    config :my_app,
      db_password: "testdb"
  :prod ->
    config :my_app,
      db_password: {:system, "MY_APP_DB_PASSWORD"}
end
```

```elixir
Env.fetch!(:my_app, :db_password)
# MIX_ENV=test なら "testdb"
# MIX_ENV=prod なら MY_APP_DB_PASSWORD 環境変数の値
```

`Application.fetch_env!/2` などを使って設定を読んでいた部分は、全部 `Env.fetch!/2` などに置き換えましょう。

シークレットな情報だけでなく、気軽に外から変えたい値や、サーバごとに異なる設定も全部環境変数にします。
全部環境変数にするのはメリットが少ないので、必要そうな部分だけやっていくのがいいでしょう。

デメリットとしては、環境変数は文字列しか扱えないことです。
`prod.secret.exs` は Elixir のプリミティブな型を使えますが、環境変数には文字列しかありません。
なので状況に応じて様々な文字列フォーマットを [`:transformer`](https://github.com/michalmuskala/env#transformer) などの機能を使って頑張ってパースして扱うことになるでしょう。
これは微妙ですが、`prod.secret.exs` のデメリットと比べるとマシだと思うので受け入れましょう。

## 2018/10/07 追記

Distillery 2.0 になって [Config Provider](https://hexdocs.pm/distillery/config/runtime.html#config-providers) が使えるようになり、いろんな方法で実行時の設定を入れられるようになりました。
なので今なら env を使わず、実行時に環境変数を読んで `Application.put_env/3` する設定プロバイダを書くのが良いでしょう。

## 参考

- [Prod.secret.exs or env. variables? - General Questions - Elixir Forum](https://elixirforum.com/t/prod-secret-exs-or-env-variables/10000)

他の手段に関しても結構書いてるので、気になる方は見ておいても良いと思います。

- [Runtime Configuration – distillery v1.5.2](https://hexdocs.pm/distillery/runtime-configuration.html#configuration-conventions)

Distillery の推奨する書き方と真っ向から反対しているので、その点も注意する必要があります。[^2]

[^2]: `start/2` で `Application.put_env/3` しろって書いてるように読めるけど、自身の `start/2` アプリケーションが起動した時点で依存しているアプリケーションは既に起動しているので、依存しているアプリケーションに起動時に設定を読む奴が居た場合は自身の `start/2` の段階で設定しても手遅れなので、何でこの方法を推奨してるのか分からない。
