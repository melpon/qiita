ほんとだ、完全に読み間違えてました。

```cpp
    size_type capacity() const
        {return (__is_long() ? __get_long_cap()
                             : static_cast<size_type>(__min_cap)) - 1;}
```

を、

```cpp
    size_type capacity() const
        {return __is_long() ? __get_long_cap()
                            : static_cast<size_type>(__min_cap) - 1;}
```

と見ていて `__min_cap` に対してだけ -1 してるものだと思ってました。
ありがとうございます。修正しておきました！

> 個人的には、__set_long_cap() と __get_long_cap() が再開 bit をオンオフした上で関数呼び出し側で +1 したり -1 したりするのが今一つ意味が分からないですが。+1 や -1 しないでそのまま取得設定しろよ、と…

`capacity()` で -1 しているのは、内部で `__get_long_cap()` （NULL 含むキャパシティ取得）単体で使うケースと `capacity()` （NULL 含まないキャパシティ取得）単体で使うケースを考えると、これでいいんじゃないかなと思います。
