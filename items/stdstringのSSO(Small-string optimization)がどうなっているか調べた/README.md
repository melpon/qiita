GCC と Clang の SSO が気になったので調べました。

## SSO(Small-string optimization)とは

通常、`std::string` は文字列を確保する際、動的にメモリを確保します。
しかし `"aaa"` とか `"hogehoge"` とかの小さい文字列でメモリをアロケートするのは勿体無い。
何とかメモリを確保せずに済ませようと頑張って最適化された実装が SSO です。

具体的には、`std::string` オブジェクトの中に文字列を格納します。

```cpp
class string {
  char sso[16];
  ...
};
```

こうすることで、メモリをアロケートせずに文字列を格納できます。
ただし、これは全ての `std::string` オブジェクトのサイズが増えることになります。

通常、`std::basic_string` には以下のデータが必要になります。

- 文字列の実体へのポインタ
- 利用している文字列のサイズ（size）
- 確保している領域のキャパシティ（capacity）
- アロケータ

それぞれが8バイトとして32バイト、これに加えて `char sso[16]` で 16 バイト確保するとすれば、48バイトです。
1文字をコピーするために48バイトもコピーするのはコストが高すぎるでしょう。

そのため GCC の libstdc++ や Clang の libcxx は、ものすごく頑張って最適化をしています。

実際、以下のコードを書いて調べてみると、GCC 6.3.0 なら **32**、Clang 4.0.0 なら **24** と表示されました。[^1]

[^1]: 64bitマシン上の話です

```cpp
#include <string>
#include <iostream>

int main() {
    std::cout << sizeof(std::string) << std::endl;
}
```

GCC も Clang も SSO を含んでいます。
それなのにこのサイズで済んでいるのはどんな実装になっているのか。
気になったのでソースを見て調べてみました。

## GCCの場合

```cpp
      // Use empty-base optimization: http://www.cantrip.org/emptyopt.html
      struct _Alloc_hider : allocator_type // TODO check __is_final
      {    
        _Alloc_hider(pointer __dat, const _Alloc& __a = _Alloc())
        : allocator_type(__a), _M_p(__dat) { }

        pointer _M_p; // The actual data.
      };   

      _Alloc_hider      _M_dataplus;
      size_type         _M_string_length;

      enum { _S_local_capacity = 15 / sizeof(_CharT) };

      union
      {    
        _CharT           _M_local_buf[_S_local_capacity + 1];
        size_type        _M_allocated_capacity;
      };
```

ものすごく分かりやすい実装です。

`_Alloc_hide` 型が `_M_p` を持っていて、これが文字列の実体を指すポインタになります。
アロケータを継承しているのは、コメントに書いている通り [EBO](http://en.cppreference.com/w/cpp/language/ebo) (Empty base optimization) を有効にするためです。
アロケータが状態を持たない場合、EBO によって `_Alloc_hide` のサイズはポインタの8バイトだけになります。

`_M_string_length` は文字列のサイズで、8バイト取っています。

`_S_local_capacity` は、`15 / sizeof(_CharT)` です。
`_CharT == char` であり、`sizeof(char) == 1` なので、`_S_local_capacity == 15` になります。
そしてこの `_S_local_capacity` を使って `_M_local_buf[_S_local_capacity + 1]` と `_M_allocated_capacity` で union していますが、`_M_local_buf` は16バイト、`_M_allocated_capacity` は8バイトであるため、このunionは16バイト取ります。

これで合計32バイトになります。

SSO 用のバッファは、名前から何となく想像がつくと思いますが、`_M_local_buf` です。
15文字を超えないサイズの文字列であれば、この `_M_local_buf` に格納されることになります。
つまり **GCC の SSO は 15 バイトまで有効** ということです。

GCC 版 `std::string` の良いところは、**分岐が少ない** ところです。
それぞれの関数の実装を調べると、以下のようになっていました。

- `c_str()`: `_M_dataplus._M_p` を返すだけ（_M_p にはデフォルトで `_M_local_buf` が設定されているので）。
- `size()`: `_M_string_length` を返すだけ。
- `capacity()`: SSOが効いてるなら `_S_local_capacity` を返す、効いてないなら `_M_allocated_capacity` を返す。

なお、SSOが効いているかの判断は `_M_dataplus._M_p == _M_local_buf` で行っています。

このように `c_str()` と `size()` は分岐が必要ありません。
実体へのポインタやサイズは内部でもよく参照するため、ここで分岐が必要ないというのは、そこそこ高速化に寄与するでしょう（たぶん）。

ただし、Clang の実装よりオブジェクトのサイズが大きくなっているため、そこはちょっと残念なところです。

多分、古い `std::string` との ABI の互換性を崩したくなかったんじゃないかなと思いますが、ちゃんと調べていないので分かりません。

## Clangの場合

Clang は、`_LIBCPP_ABI_ALTERNATE_STRING_LAYOUT` が定義されているかどうかでレイアウトが変わります。
ここでは、`_LIBCPP_ABI_ALTERNATE_STRING_LAYOUT` が定義されていないバージョンを書きます。デフォルトでは定義されていないバージョンが使われるはずです。
また、定数を計算している箇所を定数にしたり、今回の説明で関係無さそうな部分は削除しています。

```cpp
    struct __long
    {
        size_type __cap_;
        size_type __size_;
        pointer   __data_;
    };   

    enum {__short_mask = 0x01};
    enum {__long_mask  = 0x1ul};

    enum {__min_cap = 23};

    struct __short
    {    
        unsigned char __size_;
        value_type __data_[__min_cap];
    };

    struct __rep
    {
        union
        {
            __long  __l;
            __short __s;
        };
    };

    __compressed_pair<__rep, allocator_type> __r_;
```

まず、`__compressed_pair<__rep, allocator_type>` によって EBO を効かせて `allocator_type` のサイズを消しています。

次に、SSO が効いてる場合と効いてない場合で、利用する構造体を `__long` と `__short` に分けています。

SSO が効いていない場合、単純に `__long` 型の `__cap_` でキャパシティを、`__size_` でサイズを、`__data_` でポインタを格納しているだけです。

SSO が効いている場合、サイズを格納するため `__size_` に1バイト、実体を格納するため `__data_` に23バイトを使っています。
最後は NULL である必要があるので、22文字まで格納できます。
つまり **Clang の SSO は 22 バイトまで有効** ということです。

GCC のオブジェクトサイズが 32 バイトで SSO が 15 バイトなのに対し、Clang のオブジェクトサイズが 24 バイトで SSO が 22 バイト。
完全に Clang が勝ってる感じです。

本当にこれでうまく動作するのか見ていきましょう。

### SSOが効いているかを見る

まず、SSO が効いているか効いていないかは、`__r_.first().__s.__size_` の下位1bitを見て判断しています。

いきなりサイズのデータを壊しているように見えますが、実は `__r_.first().__s.__size_` には、文字列サイズの**2倍の値**を入れるようにしています。
そのため、下位1bitは空いているのです。
実際、SSO が効いている場合の文字列サイズの設定・取得関数は以下のようになっています。

```cpp
    void __set_short_size(size_type __s)
        {__r_.first().__s.__size_ = (unsigned char)(__s << 1);}
    size_type __get_short_size() const
        {return __r_.first().__s.__size_ >> 1;}
```

SSO が効いている場合はたかだか 22 までしか値が増えないため、2倍しても1バイトの最大値（255）を超えることはありません。
そのためこれで SSO が効いているかどうかを判断できます。

ただし、`__short::__size_` の下位1bitは、同時に `__r_.first().__l.__cap_` の下位1bitでもあります。
SSO が効いていない場合、こちらの値として使われることになります。
`__cap_` は領域のキャパシティとして使われていますが、この変数のキャパシティとして利用する値を **常に2の倍数にする** ことにより、下位1bitを SSO が効いているかどうかのフラグとして使えるようにしています。

### 文字列へのポインタ

SSO が効いている場合は `__r_.first().__s.__data_` を、効いていない場合は `__r_.first().__l.__data_` を使います。

### 文字列のサイズ

先程も説明しましたが、SSO が効いている場合は2倍した値が `__r_.first().__s.__size_` に格納されているため、2で割って値を返します。

```cpp
    size_type __get_short_size() const
        {return __r_.first().__s.__size_ >> 1;}
```

SSO が効いていない場合は、単純に `__long::__size_` を返すだけです。

```cpp
    size_type __get_long_size() const
        {return __r_.first().__l.__size_;}
```

### キャパシティ

SSO が効いている場合は、単純に `__min_cap - 1` を返すだけです。

```cpp
    size_type capacity() const
        {return (__is_long() ? __get_long_cap()
                             : static_cast<size_type>(__min_cap)) - 1;}
```

SSO が効いていない場合は、先程も説明した通り、下位1bitを0にしてから返します。
これによって `__get_long_cap()` が必ず2の倍数になります。

```cpp
    void __set_long_cap(size_type __s)
        {__r_.first().__l.__cap_  = __long_mask | __s;}
    size_type __get_long_cap() const
        {return __r_.first().__l.__cap_ & size_type(~__long_mask);}
```

`capacity()` 関数は `__get_long_cap() - 1` を返すので、SSO が効いていない場合は必ず奇数になります。
-1 している理由は、文字列は必ず終端に '\0' が必要であるため、最低でも `capacity() + 1` バイトの領域を確保しておく必要があるからです。

このように、どの関数も分岐を必要としていて、GCC より少しだけ遅くなる可能性があります。
しかしオブジェクトサイズが GCC より小さいこと、SSO のサイズが GCC より大きいことを考えると、総合的には Clang の方が良さそうに見えます。

なお、`_LIBCPP_ABI_ALTERNATE_STRING_LAYOUT` が定義されていると、`__long::__data_` や `__short::__data_` が構造体の先頭に来るようになります。
こうすることで、`std::string` オブジェクトの先頭アドレスと文字列の実体の先頭アドレスが同じ位置になるため、文字列へアクセスする速度を上げることができるようです。
ただ、ABIが変わってしまうため、ベンダー以外の人がフラグを変えるのは推奨していないとのことです。

## まとめ

GCC, Clang の SSO がどのように実現されているかを見てきました。

GCC は大分率直な実装になっていて、オブジェクトサイズが 32 バイトで、SSO が効く最大サイズが 15 バイトでした。
Clang は少し複雑な実装になっていて、オブジェクトサイズが 24 バイトで、SSO が効く最大サイズが 22 バイトでした。

これを知っておくことで、もっと効率の良い C++ のコードを書けるかもしれません。

## 参考

- [What are the mechanics of short string optimization in libc++?](http://stackoverflow.com/questions/21694302/what-are-the-mechanics-of-short-string-optimization-in-libc)
