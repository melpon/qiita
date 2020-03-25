`make_ref/0` は呼び出す度にユニークな値を返す関数ですが、これがどの程度ユニークなのかを調べてみました。

## 結論

- 全く別の独立した Erlang VM 上ではユニークにならない
- 同じノード名で再起動した際に、再起動前に生成した値と同じになることがある

## Referenceの仕組み

`make_ref/0`が返すReference型は、送信する際のバイナリに以下の値を含めています[^1]。

[^1]: Reference型の値そのものにはノード名は入っていない。ノード名は `term_to_binary/1` 等でバイナリにエンコードされる際に追加される。

- 自身のノード名の`atom`
- スレッドID
- １つのスレッドでユニークなカウンタ

これらの値によってユニーク性を担保しています。

スレッドIDは、Erlang VM起動時、必要な数だけスレッドが起動した時に決定します。
ソースを見た限り、それ以降は特にスレッド起こしたりしていないようです。
そのため、この値は Erlang VM が生きている間は変わらないでしょう。

また、Erlang VM 間でクラスタリングを組む場合、それぞれのノード名は異なっている必要があります。
そのため、クラスタリングしている Erlang VM 間で一致する可能性は無さそうです。

しかし、名前を付けていない Erlang VM のノード名は `noname@nohost` という名前になるし、たまたまノード名が一致する可能性もあるため、独立した Erlang VM 同士は Reference の値が一致する可能性を考える必要があります。

また、同じノード名で再起動した場合も同様に Reference の値が一致する可能性があります。

## 気を付けること

以下のことに注意して Reference を使いましょう。

- クラスタリングを組まず、例えばネットワーク通信で Reference のバイナリを送っても、それがお互いの Erlang VM 間でユニークになる保証は無い
- Reference のバイナリをファイルなどに保存し、再起動した後に読み込んだ際に、再起動する前の Erlang VM と自身の Erlang VM 間でユニークになる保証は無い

つまり **生きててクラスタリングを組んでいる Erlang VM 同士でしかユニークにならない** 、ということに注意して利用しましょう。

## 参考

- [[erlang-questions] make_ref bug or feature?](http://erlang.org/pipermail/erlang-questions/2009-March/042719.html)
- [make_ref/0](http://erlang.org/doc/man/erlang.html#make_ref-0)
- [11.20  NEW_REFERENCE_EXT](http://erlang.org/doc/apps/erts/erl_ext_dist.html#id101374)
