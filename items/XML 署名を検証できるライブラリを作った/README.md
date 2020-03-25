Elixir で [SAML](https://ja.wikipedia.org/wiki/Security_Assertion_Markup_Language) を使ったシングルサインオンが必要そうだったので、XML 署名を検証できるライブラリを作りました。

## SAML とは

乱暴に言えば、XML 版の OAuth みたいな奴です。

クライアントとサーバ間でユーザ認証するのではなく、クライアントと第三者の認証機関（Identity Provider(IdP) と呼ぶ）の間でユーザ認証を行います。

IdPとクライアント間で認証が終わったら、IdP は SAML レスポンスと呼ばれる XML のデータを返すので、クライアントはこれをそのままサーバに投げると、サーバはこのデータを見てユーザを識別したり各操作を許可したりします。

ただ、サーバが無条件にこの SAML レスポンスを信用してしまうと、ユーザはいくらでも偽装したり操作を許可したり出来てしまいます。
そのため、この SAML レスポンスが、確実に指定した IdP が返した SAML レスポンスであり、クライアントによって一切手を加えられていない状態であることを確認する必要があります。[^replay]

[^replay]: 単に手を加えられてないことを確認するだけだとリプレイ攻撃が出来てしまうので、SAML レスポンスに生存期間を付けて、サーバで検証後にその期間内に入っているかもチェックする必要があります（生存期間の情報は、署名の検証によって手が加えられてないことが保証されている）。あと SAML レスポンスが他人に傍受されるのもまずそうなので、通信は全て HTTPS で行った方がいいでしょう。

SAML レスポンスは [XML 署名](https://ja.wikipedia.org/wiki/XML%E7%BD%B2%E5%90%8D) という仕様によって署名されているので、この XML 署名によって署名された SAML レスポンスの検証を行うためのライブラリを作りました。

## なぜ既存のライブラリを使わなかったのか

SAML 認証を行う既存のライブラリとしては [samly](https://hex.pm/packages/samly) があります。
このライブラリは、XML 署名を検証するために [esaml](https://hex.pm/packages/esaml) というライブラリを使っているのですが、コードを見たりいろいろ弄った限りだと、

- ルート証明書を確認しないので自己署名でも通る[^self]
- 検証が rsa + sha1 or sha256 にしか対応してない
- Erlang の XML ライブラリ xmerl にバグがある[^1]のでちゃんと XML 正規化ができない
- XML 正規化の xml-exc-c14n のコメント無しバージョンにしか対応していない
- 正直 XML 正規化は複雑過ぎるので、多分 esaml はまだ問題があると思われる

[^1]: いろんな XML を食わせて libxml と比較してたら見つけました。[Wrong attribute-value normalization](https://bugs.erlang.org/browse/ERL-475) に報告しています。
[^self]: SAML で自己署名するのは別に問題ない気もするけど、XML 署名単体で考えると良くない。

というのがあり、ちゃんとライブラリを作った方が良さそうだったので、SAML のために、まずは XML 署名のライブラリを作ってみることにしました。

## XML 署名と XML 正規化の辛さ

ところで、これが XML 署名の目次の一部です。

[XML Signature Syntax and Processing Version 1.1](https://www.w3.org/TR/xmldsig-core/)

<img width="581" alt="スクリーンショット 2017-12-20 23.21.43.png" src="https://qiita-image-store.s3.amazonaws.com/0/64060/a384ce27-ef82-a692-2009-35f584a7c966.png">

自分の知ってる署名は、例えば POST で渡された `x=10&y=20` みたいなデータを昇順にソートし直してちょっとシグネチャ計算するだけというイメージだったのですが、どうやら全く違うようです。
ヤバそうな気配を感じます。

ところで、XML 署名を検証するためには、必要な部分を取り出した後、[XML 正規化](https://ja.wikipedia.org/wiki/Canonical_XML) (Canonicalization, C14N) と呼ばれる XML の変換作業を行う必要があります。

例えば [Canonical XML Version 1.0](https://www.w3.org/TR/2001/REC-xml-c14n-20010315) の 3.3 のサンプルを見ると、

```xml
<!DOCTYPE doc [<!ATTLIST e9 attr CDATA "default">]>
<doc>
   <e1   />
   <e2   ></e2>
   <e3   name = "elem3"   id="elem3"   />
   <e4   name="elem4"   id="elem4"   ></e4>
   <e5 a:attr="out" b:attr="sorted" attr2="all" attr="I'm"
      xmlns:b="http://www.ietf.org"
      xmlns:a="http://www.w3.org"
      xmlns="http://example.org"/>
   <e6 xmlns="" xmlns:a="http://www.w3.org">
      <e7 xmlns="http://www.ietf.org">
         <e8 xmlns="" xmlns:a="http://www.w3.org">
            <e9 xmlns="" xmlns:a="http://www.ietf.org"/>
         </e8>
      </e7>
   </e6>
</doc>
```

この XML を XML 正規化した時に、

```xml
<doc>
   <e1></e1>
   <e2></e2>
   <e3 id="elem3" name="elem3"></e3>
   <e4 id="elem4" name="elem4"></e4>
   <e5 xmlns="http://example.org" xmlns:a="http://www.w3.org" xmlns:b="http://www.ietf.org" attr="I'm" attr2="all" b:attr="sorted" a:attr="out"></e5>
   <e6 xmlns:a="http://www.w3.org">
      <e7 xmlns="http://www.ietf.org">
         <e8 xmlns="">
            <e9 xmlns:a="http://www.ietf.org" attr="default"></e9>
         </e8>
      </e7>
   </e6>
</doc>
```

このようになる必要があると定義しています。

タグの空白が消えてたり、でもタグとタグの間の空白や改行は消えてなかったり、属性の順番が変わっていたり、名前空間が統合されていたり、`DOCTYPE` に書かれている属性が `<e9>` タグに適用されてたりしています。

また、XML 正規化の仕様は、Canonical XML Version 1.0 の他にも [Canonical XML Version 1.1](https://www.w3.org/TR/xml-c14n/)
 と [Exclusive XML Canonicalization Version 1.0](https://www.w3.org/TR/xml-exc-c14n/) があり、それぞれ微妙に仕様が違います。
かなりヤバそうな気配を感じます。

## libxml

ということで、最初は xmerl を使って XML 正規化を頑張って手で書いていたのですが、複雑過ぎて死にそうだったのと、前述した通り xmerl にバグがあったこともあり、心が折れたので既存のライブラリを使うことにしました。
<del>XML 署名や XML 正規化は人間が書くものじゃない。</del>

信頼のおける XML ライブラリといえば C で書かれた [Libxml2](http://xmlsoft.org/) です。
このライブラリは [C14N も実装している](http://xmlsoft.org/html/libxml-c14n.html) ので、これを使えばかなり楽ができそうです。

Libxml2 の Elixir ラッパーは存在しなかったので、まずこれを作りました。[^2]

[^2]: SAML 認証するための XML 署名するための XML 正規化するための XML ライブラリのラッパーを実装する。完全に [ヤクの毛刈り](https://ja.wiktionary.org/wiki/yak_shaving) です。

- [hex.pm](https://hex.pm/packages/libxml)
- [GitHub](https://github.com/melpon/libxml)

Libxml2 の薄いラッパーなので、当然ながら NIF を使っていて、ちょっとでも使い方を間違えると落ちるので注意して下さい。

## sign_xml

これで XML の読み書きと XML 正規化が出来るようになったので、あとは XML 署名の検証を行うだけです。
これは Python の [signxml](https://pypi.python.org/pypi/signxml) の実装を見ながら作りました。

- [hex.pm](https://hex.pm/packages/sign_xml)
- [GitHub](https://github.com/melpon/sign_xml)

大体 signxml と同程度のテストが通るようになったので、これで問題ないかなと思います。

## まとめ

SAML のために XML 署名のライブラリを書きました。
仕様は複雑ですが、利用するのは簡単だと思うので、Elixir のサーバで SAML のシングルサインオンがしたくなった場合は、利用を検討してみて下さい。
