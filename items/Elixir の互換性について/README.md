Elixir は割と互換性を大事にしている言語で、古い機能が割とずっと残っていたりします。

例えば [HashDict](https://hexdocs.pm/elixir/HashDict.html) は Elixir 1.4 で deprecated（非推奨）の警告が出るようになりましたが、まだ削除されていません。
例えば Elixir 1.3 まで [`Enum.partition/2`](https://hexdocs.pm/elixir/1.3.0/Enum.html#partition/2) という関数があったのですが、Elixir 1.4 ではひっそりとドキュメントから消されています。しかし今でも呼び出すことが出来るし、deprecated の警告も出ません。

これらの機能が、いつから deprecated の警告が出て、いつまで残っているのか気になるところです。

## 互換性のポリシー

答えは、公式のドキュメント [Deprecations](https://hexdocs.pm/elixir/deprecations.html) に書いています。

それによると、Elixir の deprecated や削除の方針は以下のようになっています。

- 消したい機能があった時、まず soft deprecated 状態にする。CHANGELOG やドキュメントに deprecated の記述をしたり、ドキュメントから削除したりするが、これらの関数やモジュールを利用する時には **警告を出さない** 。
- 代替機能を用意してから **最低でも２マイナーバージョン** 経過した後に deprecated にする。これらの関数やモジュールを利用する時に **警告を出す** 。
- **メジャーバージョンを上げる時** に、deprecated になっている機能を消す。つまり今 deprecated になっている機能は Elixir 2.0 で消える。

つまり Elixir 1.5 の `HashDict` で deprecated の警告が出るのは、Elixir 1.2 で `Map` という代替機能が追加されて soft deprecated になり、２マイナーバージョンが経過した Elixir 1.4 以降になっているからです。
また `Enum.partition/2` で警告が出ないのは、Elixir 1.4 で `Enum.split_with/2` という代替機能が追加されて soft deprecated になり、Elixir 1.5 の時点ではまだ２マイナーバージョンが経過していなくて deprecated になっていないからです。つまり `Enum.partition/2` は Elixir 1.6 で deprecated になり、警告が出るようになります。[^2]

[^2]: しかし [`Supervisor.Spec`](https://hexdocs.pm/elixir/Supervisor.Spec.html) が deprecated （あるいは soft deprecated）になってるのに、そのことが CHANGELOG にも書かれていないし警告も出ないことだけがよく分からない。

また、これらの機能は Elixir 2.0 で削除されます。
逆に言えば、ドキュメントから消えたり deprecated 警告が出るものの、Elixir 2.0 までは残っていることが保証されています。

## soft deprecated が存在する理由

ユーザは、わざわざ CHANGELOG を見たりしないし、書いているモジュールのドキュメントを見に行ったりしまません。
ユーザが気がつくのは deprecated 状態になって警告が出てきてからでしょう。
それなのにわざわざ soft deprecated 状態を用意している理由はなぜでしょうか。

これはただの予想ですが、多分警告を除けた時に古いバージョンが動かなくなるのを防ぐためだと思います。

例えば Elixir 1.5 から `Atom.to_char_list/1` や `Integer.to_char_list/1` といった `.*char_list.*` 系の関数や atom が全て `.*charlist.*` になりました。
これによって多くのライブラリが警告を出すようになったのですが、だからといってすぐに直すと、Elixir 1.4 で動かなくなりそうだから直さない、といったことがあるかもしれません。

しかし、`.*charlist.*` 周りの実装は、実は Elixir 1.3 から入っています。つまり警告が出た時点ですぐ直しても、**過去２マイナーバージョンで動作する** ことが保証されているのです。
こういう理由で入れてるとしたら、soft deprecated はよく出来た仕組みだなぁと思います。

## まとめ

このように、Elixir は互換性をかなり大事にしています。
Elixir 2.0 までは、古いコードでも割と動くと思うので、安心して利用しましょう。
