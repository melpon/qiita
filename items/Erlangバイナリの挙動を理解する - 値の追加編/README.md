Erlangの値は不変（immutable）です。

この不変というのは、Erlangを利用する側からは不変に見えるというだけの話です。
実際のところErlangは、一度生成したオブジェクトの値をガンガン書き換えます。[^1]

[^1]: 例えば`setelement/3`は、[特定の条件](http://erlang.org/doc/efficiency_guide/commoncaveats.html#id63352)を満たすと in-place で書き換えてくれます。

Erlangバイナリもその対象であり、外から見てる限りでは操作を加える度にコピーしているように見えますが、内部では全く異なる動作になっています。
特に、バイナリの後方に値を追加する際にはとても良いパフォーマンスを発揮するように最適化されています。

今回は、Erlangのバイナリが内部的にどのような構造になっているかと、バイナリを後方に追加する時にそれらがどのような挙動になるかを説明します。

なお、今回の話は [Erlang -- Constructing and Matching Binaries](http://erlang.org/doc/efficiency_guide/binaryhandling.html) の 4.2  Constructing Binaries を深く掘り下げた内容になっています。

# Erlangバイナリの種類

Erlangのバイナリは、内部的には４種類あります。
HeapBin, ProcBin, SubBin, BinMatchState です。
BinMatchState は、今回の話では使わないので省いています。

## HeapBin

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/730c1b5c-aff5-20cd-deac-b17dfc3dbcc2.png)

HeapBinは、小さいバイナリ（64バイト以下）を格納する際に使います。
2ワード+`data`バイト分しか容量を取らないので省メモリです。
また、HeapBinの中に実データが格納されていてデリファレンス不要であるため、アクセスは高速です。

`thing_word`というのは、バイナリを種別するためのタグ[^1]です。
`size`は実データのバイト数です。

[^1]: 正確にはタグ情報+実データのワード数なのですが、HeapBin以外のサイズは固定なのでほぼ固定値と変わりません。


## ProcBin

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/89fcbd9d-ece1-6261-35d1-9d6f13819cc6.png)

ProcBinは、実データを格納するためのBinaryと、それを参照するProcBinの２つに分かれています。
HeapBinに収まらない、大きいバイナリを扱う際に使います。

ProcBinが6ワード、Binaryが3ワード+`orig_bytes`バイト分取るので、そこそこメモリを使います。
また、２つの要素に分かれているため、アクセスはそこまで早くありません。

ただし、Binaryは全プロセスが共有している共有ヒープ上に格納されます。
そのため、メッセージパッシングの際にこのバイナリをコピーする必要が無くなるのがメリットです。

`orig_bytes`が実データを格納するための領域で、その領域のバイト数が`orig_size`に格納されています。

`size`には、`orig_bytes`の領域のうち、現在何バイトを使っているかという値を格納しています。

なお、今回の説明で使わない部分は灰色の文字にしています。

## SubBin

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/efa0d7fb-c729-ca7f-c1fc-3da6e9d6d1d1.png)

SubBinは、HeapBinやProcBinの一部を参照する際に使います。

`orig`は、HeapBinかProcBinのどちらかを指していて、`orig`の指す先にある実データの、`off`バイトから`off + size`バイトまでを実データとして扱います。
`orig`がProcBinを指している場合、実データにアクセスするために間接参照が２回必要になるため、遅くなります。
ただし、実データの全部または一部を他のバイナリと共有できるため、省メモリになるし、構築もコピーが不要な分、高速になります。

`is_writable`フラグは、このバイナリに値を追加する際に、既存の実データに追加していいかどうかを判断するためのフラグです。

なお、`bitoff`や`bitsize`というフィールドが`is_writable`と同じワードの場所にあるのですが、図に入れるのが面倒だったので省いています。
これらのフィールドはビットを扱う時に必要なフィールドで、これを持っているのはSubBinだけであるため、ビットレベルでバイナリを扱う際には必ずSubBinになります。

# バイナリに値を追加する

バイナリに値を追加するという処理は、頻繁に出現します。
例えば [Erlang -- Constructing and Matching Binaries](http://erlang.org/doc/efficiency_guide/binaryhandling.html) では、以下の例が載っています。

```erlang
my_list_to_binary(List) ->
    my_list_to_binary(List, <<>>).

my_list_to_binary([H|T], Acc) ->
    my_list_to_binary(T, <<Acc/binary,H>>);
my_list_to_binary([], Acc) ->
    Acc.
```

このようなコードはよく書くことになるため、Erlangはその処理のために最適化しています。
良いパフォーマンスを任意に引き出せるようにするために、この最適化がどのように動いているのかを知っておくのは重要でしょう。

## 戦略

基本的には、バイナリに値を追加する際に確保する際に、**メモリは余分に確保しておく** という戦略になります。
そうすれば、次の値の追加では、余分に確保しておいた領域に値を追加すればいいだけなので、追加する操作をする度に新しくメモリを確保する、ということが不要になります。

ただし、**外から見た際に不変に見えなければならない** という前提があります。
そのため、いつでも空き領域に値を追加して良いという訳ではありません。
その辺をうまくやろうとすると、以下のようになります。

## 具体的な手順

具体的な手順を文字で書くと、以下のようになります。

AというバイナリにBという値を追加する時、

- Aに追記不可の場合:
    - （つまりAがHeapBin または、AがProcBin または、AがSubBinかつ`is_writable==0`の場合）
    - NバイトのBinaryを参照するProcBinを作る
        - N: AとBのサイズの２倍。ただしそれが256より小さければ256
    - 新しいSubBinを生成し、`is_writable`を1にして、そのProcBinを参照する

- Aに追記可能な場合:
    - （つまりAがSubBinかつ`is_writable==1`の場合）
    - Aの`is_writable`を0にする
    - Bのサイズを足すと、Binaryの`orig_size`のサイズを超えてしまう場合:
        - Nバイトの領域を持つようにBinaryをreallocし、ProcBinはそのBinaryを指すようにする
            - N: AとBのサイズの２倍。ただしそれが256より小さければ256
    - 新しいSubBinを生成し、`is_writable`を1にして、ProcBinを参照する

- 得られたSubBinの指すバイナリにBの内容を書き込む

という動作になります。[^2]

[^2]: [OTP-19.2](https://github.com/erlang/otp/tree/OTP-19.2) 時点の動作です。

文字だと分かりにくいので、具体的な処理から図を書いていきます。

```erlang
%% A. 初期化
A = binary:copy(<<1, 2, 3>>),

%% B. Aに3バイト追加
B = <<A/binary, 4, 5, 6>>,

%% C. Bに3バイト追加
C = <<B/binary, 7, 8, 9>>,

%% D. Cに3バイト追加
D = <<C/binary, 10, 11, 12>>,

%% E. Dに1000バイト追加
E = <<D/binary, (binary:copy(<<100>>, 1000))/binary>>,

%% F. Dに3バイト追加
F = <<D/binary, 13, 14, 15>>,
```

このようなコードを書いた場合、以下のような動作になります。

## A. 初期化

```erlang
A = binary:copy(<<1, 2, 3>>),
```

結果:

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/338bbbec-bd5a-8e34-9bc4-7b2f44a93a24.png)

これは要素の追加ではなく、単に新しくバイナリを生成する処理になります。
64バイト以下のバイナリであるため、HeapBinを生成します。

また、新しく生成されたり、書き換わった要素は色を変えるようにしています。（ただし`thing_word`を除く）

なお、値を追加する時に`binary:copy/1`で`<<1, 2, 3>>`をコピーしているのは、定数畳み込みの最適化を抑制するためです。
[BEAMの関数をディスアセンブルする](http://qiita.com/melpon/items/2e96caf2bfaebbd46bb3)で書いたように、単に`<<1, 2, 3>>`と書くだけだと全て畳み込まれて意図した動作をしません。

つまり、少なくとも OTP 19.2 において、 [Erlang -- Constructing and Matching Binaries](http://erlang.org/doc/efficiency_guide/binaryhandling.html) の 4.2  Constructing Binaries にある例は説明の通りの動作をしません。

## B. Aに3バイト追加

```erlang
B = <<A/binary, 4, 5, 6>>,
```

結果:

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/0ba3c52a-1097-232a-d16d-c10f2e826e6a.png)

AはHeapBinであるため、追記不可です。
そのため新しくバイナリを生成します。

この時、Binaryの`orig_bytes`の領域として **256バイト確保します**。
実際利用する領域は6バイトであるため、`ProcBin:size`には6を入れます。
つまりBinaryの`orig_bytes`は、250バイトが未使用領域になります。

また、`SubBin:size`や`SubBin:off`にも値を設定し、**`is_writable`を1にします**。
これによって、次の書き込みで追記が可能になります。

## C. Bに3バイト追加

```erlang
C = <<B/binary, 7, 8, 9>>,
```

結果:

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/3dce5acf-4691-773c-d049-4d928980da9f.png)

BはSubBinかつ`is_writable`が1であるため、**追記可能**です。
そのため、新しいバイナリを確保することなく、Cの内容が`B->orig->bytes->orig_bytes`に追加されます。
使用サイズが増えたので、`B->orig->size`を6から9にします。
また、**`B->is_writable`は0に書き換えます**。

Cのために新しくSubBinを生成し、`orig_bytes`の[1..9]バイトを参照するようにします。
`is_writable`は1にしておきます。

この時、ProcBinは9バイト使用している、とマークしていますが、BはSubBinであり、[1..6]バイトを利用することになっているため、Bの指している内容は変わっていません。
そのため**不変性は守られています**。

## D. Cに3バイト追加

```erlang
D = <<C/binary, 10, 11, 12>>,
```

結果:

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/66002e8f-7ec6-ddc9-158d-cc73922a0aa6.png)

先ほどと同じです。Aはもう説明する上で使わないので非表示にしました。

CはSubBinかつ`is_writable`が1であるため、追記可能です。
そのため、新しいバイナリを確保することなく、Dの内容が`C->orig->bytes->orig_bytes`に追加されます。
使用サイズが増えたので、`C->orig->size`を9から12にします。
また、`C->is_writable`は0に書き換えます。

Dのために新しくSubBinを生成し、`orig_bytes`の[1..12]バイトを参照するようにします。
`is_writable`は1にしておきます。

この時、BinaryやProcBinは12バイト使用している、とマークしていますが、CはSubBinであり、[1..9]バイトを利用することになっているため、Cの内容は変わっていません。
そのため不変性は守られています。

## E. Dに1000バイト追加

```erlang
E = <<D/binary, (binary:copy(<<100>>, 1000))/binary>>,
```

結果:

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/572101e9-7bf7-5048-3b8d-831af645b1c7.png)

Binaryの余分な領域を超えるサイズを追加するとどうなるか、という実験です。

DはSubBinかつ`is_writable`が1であるため、追記可能です。
しかし `D->orig->bytes->orig_bytes` の領域には1000バイトの余分な領域はありません。
そのため **`D->orig->bytes`の指すBinaryをreallocして、新しくBinaryを生成します**。
この時`orig_bytes`の領域は、新しく必要な領域の２倍、つまり1012*2の2024バイトになります。
`D->orig->bytes`は新しく生成した領域を指すように切り替え、使用サイズを1024バイトにします。
また、`D->is_writable`は0にしておきます。

Eのために新しくSubBinを生成し、`orig_bytes`の[1..1012]バイトを参照するようにします。
`is_writable`は1にしておきます。

この時、BinaryやProcBinは1024バイト使用している、とマークしていますが、DはSubBinであり、[1..12]バイトを利用することになっているため、Dの内容は変わっていません。
そのため不変性は守られています。

## F. Dに3バイト追加

```erlang
F = <<D/binary, 13, 14, 15>>,
```

![image](https://qiita-image-store.s3.amazonaws.com/0/64060/28d2dafc-a4d2-cc67-ff5b-b870b58a65cc.png)

今度は、Eではなく、Dに3バイト追加しています。B, Cはもう説明する上では使わないので非表示にしました。

DはSubBinですが、**既にis_writableが0なので、追記不可です**。
そのため、Fのために新しくBinary, ProcBin, SubBinを生成します。

この時、Binaryの`orig_bytes`の領域として256バイト確保します。
Binaryには、D->orig->bytesの指す領域から[1..12]バイトをコピーし、新しい要素の3バイトを追加で書き込み、合計の使用領域は15バイトとなります。

もしここでDの`is_writable`を考慮しなかった場合、`D->orig->bytes`の指す領域が書き換えられることになり、Eの不変性が崩れることに注意して下さい。
`is_writable`を使って新しい領域を確保させることで、そのようなるのを防いでいます。

# まとめ

重要なのは、

- SubBinをうまく使ってバイナリを共有することで、省メモリ・構築の高速化をしている。
- `is_writable`が1の時はバイナリの後方に追記可能という意味であり、一度でもバイナリに値を追加すると`is_writable`が0になる。これによって外から見たときの不変性を保っている。

という点です。

つまりバイナリは **後方に、かつ１度だけ追加する場合** とても良い効率になります。
このルールを理解して利用することで、バイナリを使ってある程度高速に動作するコードが書けるようになるでしょう。

# 参考

- [Erlang -- Constructing and Matching Binaries](http://erlang.org/doc/efficiency_guide/binaryhandling.html)
- [otp/erts/emulator/beam at OTP-19.2 · erlang/otp](https://github.com/erlang/otp/tree/OTP-19.2/erts/emulator/beam)
