[Erlangバイナリの挙動を理解する - 値の追加編](http://qiita.com/melpon/items/17c4eaa3fb15fba0e8b5) では、値を追加した時のバイナリの挙動を書きました。

今回はメッセージパッシングを行った時にどのような挙動になるかを説明します。[^0]

[^0]: 今回も変わらず[OTP-19.2](https://github.com/erlang/otp/tree/OTP-19.2)の情報を元に書きます。

今回の話も [Erlang -- Constructing and Matching Binaries](http://erlang.org/doc/efficiency_guide/binaryhandling.html) の 4.2  Constructing Binaries を深く掘り下げた内容になっています。

また、 [Sharing Preserving](http://qiita.com/mururu/items/55324a0aea0ee5cf7f61) に関するコードはちゃんと追っていないため、Sharing Preservingを有効にして送信した場合に違いが現れるのかは分かりません。

# プロセスヒープと共有ヒープ

Erlang のランタイムにはプロセスヒープと共有ヒープと呼ばれる領域があります。

プロセスヒープは、プロセス毎に完全に分かれている領域です。
他のプロセスが参照することは決してありません。
HeapBinや、ProcBin、SubBinはプロセスヒープに確保されます。

それに対して共有ヒープは、全てのプロセスが共有可能な領域です。
Binaryの領域は、共有ヒープ上に確保されます。

[前回](http://qiita.com/17c4eaa3fb15fba0e8b5)と同じような図をプロセスヒープと共有ヒープに分けると、以下のようになります。

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/6e35aae0-4798-02ba-10d5-2723042737c2.png)

# メッセージパッシングを行う

バイナリのメッセージパッシングでは、コピーするバイト数と、使用するメモリサイズが可能な限り少なくなるように最適化されています。
この動作はとても面白くて、**メッセージパッシングをした方が使用メモリが少なくなる**ということが起きます。

それぞれのケースでどのような挙動になるかを見ていきましょう。

## HeapBinの場合

プロセスヒープは他のプロセスから参照できません。
そのため、メッセージパッシングを行う場合、プロセスヒープにある領域はどうやってもコピーが発生します。

つまり、全ての領域がプロセスヒープに確保されるHeapBinは、全てコピーされることになります。

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/3a879b02-d8e3-a137-9a6e-742b8d267f1e.png)

バイナリの場合、プロセスヒープには最大で64バイト以下の領域しか確保しないため、そこまで遅くはならないでしょう。

## ProcBinの場合

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/2f0c1f60-446a-99fa-9bfb-95f9b6c31f75.png)

先程説明したように、ProcBinはプロセスヒープに、Binaryは共有ヒープに置かれます。

この図では、Binaryは`orig_bytes`の領域として256バイト使っていますが、実際に使っているのは128バイトだけです。
前回説明したように、値の追加をする最適化のために、Binaryは余分な領域を確保することがあります。

この時、`A`をメッセージパッシングで別プロセスに送ると、以下のようになります。

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/12faffbf-749a-ed58-685d-a17236fe0bbe.png)

ProcBinは、HeapBinと同じように、単純にコピーされるだけです。
この時、両方のプロセスで同じBinaryを指すようにします。
こうすることで、最小限のコピーで他のプロセスにバイナリを渡すことができます。

面白いのはBinaryで、メッセージパッシングを送った時に余分な領域があるなら、**Binaryの領域をreallocで縮めます**。

reallocは、特にサイズを縮める方向なら単にメモリの使用サイズを減らすだけで対応できるため、メモリを確保せず使用サイズを書き換えて同じポインタを返すことが多くなっています。
そのため、reallocをすることで一切領域をコピーせずメモリの使用量を減らすことができます。[^1][^2]

[^1]: glibc の場合、多分 [この辺](https://sourceware.org/git/?p=glibc.git;a=blob;f=malloc/malloc.c;h=584edbf059e5b5adcc21946406b8199ab3f7280d;hb=d08ab9ced75e0d88827e0bb58183612afb7fe1fd#l4372) がその実装
[^2]: ただし、ソースを追いかけたけど結局glibcリのreallocを使っているかどうかは分からなかった。[この辺](https://github.com/erlang/otp/blob/OTP-19.2/erts/emulator/beam/erl_alloc_util.c#L2624)の処理で領域を縮めてるようにも見える。

ここで領域を縮める理由は、他のプロセスにバイナリを投げたのに、バイナリを投げた先で更に領域を追加することは考えにくいからでしょう。
今後追加される可能性が低く、もう使われることのなさそうな領域であるため、メモリを縮めた上でBinaryを他のプロセスと共有しています。

## SubBinの場合

SubBinは更に、`orig`の指すデータがHeapBinかProcBinであるかによって処理が異なります。

### `orig`がHeapBinを指す場合

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/6e2d5586-2864-442a-6765-5245cae27d04.png)

HeapBinが何バイトあっても、`SubBin:off`と`SubBin:size`を考慮し、必要な部分だけをコピーします。
この例では、HeapBinに10バイトあり、その中の3バイトだけをSubBinが参照しています。
この時、その3バイトだけを新しくHeapBinとして作り、必要な領域だけをコピーします。

面白いのは、メッセージを送った先では**SubBinを作らない**ことです。
必要な領域しかコピーしないため、SubBinが不要になるのです。

### `orig`がProcBinを指す場合

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/43ff7e68-6c55-7106-5176-171ead4163ed.png)

Binaryは、ProcBinの場合と同じように、realloc()でヒープを縮めます。
あとはProcBinをコピーするだけです。
この時、`ProcBin:size`を128から10に書き換え、更に`bytes`の指す領域を、SubBinの`off`のバイト数分だけ加算します。

これはつまり、SubBinが`off`と`size`によって領域の一部だけを指す仕組みを、**SubBinを使わずProcBinだけで**実現しています。
バイナリに値を追加する処理では、ProcBinそのものを共有するためにSubBinを生成していましたが、ProcBinそのものをコピーするメッセージパッシングの場合、このようにSubBinを使わずに実現できます。

このように、どちらも、メッセージパッシングした先のプロセスではSubBinを**生成しません**。[^3]
これによって、SubBinを生成するコストや、コピーするサイズを可能な限り減らしています。
かなり最適化されていることが分かります。

[^3]: ただし、SubBinじゃないとビット情報を保持できないので、ビットを扱っている場合はSubBinを生成します

# 便利関数shrink

バイナリに値を追加した後、他のプロセスにメッセージを送信することで、メモリの使用量が減らせます。
１プロセスで、バイナリに値を追加した後に「もう使わないけどこの余分に確保しているだろうメモリが気になる」という場合に、以下のような関数を使うことでメモリ使用量を減らせる可能性があります。

```erlang
shrink(Bin) ->
    Pid = spawn_link(fun () ->
                             receive
                                 {shrink, Parent, _Bin} -> Parent ! {received, self()}
                             end
                     end),
    Pid ! {shrink, self(), Bin},
    receive
        {received, Pid} -> ok
    end.
```

単純に、新しいプロセスで１回だけメッセージを受け、返信してから終了しているだけです。

これは以下のように使います。

```erlang
%% 3バイトの領域を使用
A = binary:copy(<<1, 2, 3>>),
io:format("~p~n", [binary:referenced_byte_size(A)]),

%% 値の追加によって256バイトの領域を使用
B = <<A/binary, 4, 5, 6>>,
io:format("~p~n", [binary:referenced_byte_size(B)]),

%% メッセージを投げることで6バイトになる
shrink(B),
io:format("~p~n", [binary:referenced_byte_size(B)]),

ok.
```

出力:

```
3
256
6
```

`binary:referenced_byte_size/1`は、ProcBinの場合にはBinaryの`orig_size`を返します。
最初の`B`のサイズは256であり、余分な領域が250バイトもあります。
しかし`shrink/1`を呼び出すと、次の呼び出しでは6になり、ちゃんと領域が縮められていることが分かります。

`binary:copy/1`でも同じようにサイズを減らすことができますが、`binary:copy/1`はBinaryの領域に対しても**必ずメモリコピーが発生します**。
それに比べてこちらはreallocで縮めるので、Binaryのメモリコピーが発生しない可能性があります。
メッセージパッシングの挙動を知っていれば、このようなコードを書くこともできます。

ただし、普通はこのコードを見ても意味が分からないので、よっぽど最適化したいのでなければやらない方がいいでしょう。

# まとめ

- 共有ヒープにあるBinaryはreallocで領域を縮めた上で共有される
- SubBinを可能な限り生成しないようにしている

前回の分と合わせれば、値を効率よく追加した上で、追加する必要が無くなったら余分に確保していたメモリをメッセージパッシングで減らすという、かなり効率の良い最適化ができるようになります。

また、メッセージパッシングでは、HeapBinは64バイト以下だけでしか利用しないし、Binaryに領域があるならメモリが共有され、SubBinも生成されません。
そのため、メッセージパッシングのコストはかなり少ないと言えます。[^5]
安心してバイナリを送信しましょう。

[^5]: もちろん別ノードのプロセスにメッセージパッシングする場合は別

# 参考

- [Erlang -- Constructing and Matching Binaries](http://erlang.org/doc/efficiency_guide/binaryhandling.html)
- [otp/erts/emulator/beam at OTP-19.2 · erlang/otp](https://github.com/erlang/otp/tree/OTP-19.2/erts/emulator/beam)
