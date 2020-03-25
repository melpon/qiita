Elixir から AWS を叩く人は多分大体の人が使ってる [ex_aws](https://hex.pm/packages/ex_aws) ですが、バージョン 2.0 からサービスごとにパッケージが分割されることになりました。

そのため、例えば S3 と Dynamo が使いたいなら、`mix.exs` に以下のように書くことになります。（バージョンは適当です）

```elixir
defp deps do
  [
    {:ex_aws, "~> 2.0"},
    # 追加が必要な部分
    {:ex_aws_s3, "~> 2.0"},
    {:ex_aws_dynamo, "~> 2.0"},
    # この辺は以前と同じもので大丈夫
    {:poison, "~> 3.0"},
    {:hackney, "~> 1.9"},
  ]
end
```

というのも、AWS のサービスが多すぎて CargoSense（多分ExAws作者の所属する組織）で ExAws を管理するのが難しくなってきたからだそうです。
なのでパッケージをコア部分と各サービスに分けて、CargoSense で ExAws のコア部分と S3 だけメンテナンスして、既存のサービスはやりたい人にメンテナを設定して、新しいサービスについては欲しい人が作ればいい、という方針にするようです。

## 参考

- [Proposal: ExAws 2.0 - Libraries - Elixir Forum](https://elixirforum.com/t/proposal-exaws-2-0/9269)
