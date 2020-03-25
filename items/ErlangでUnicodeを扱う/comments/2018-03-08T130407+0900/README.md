> %% BinStrはUnicode文字列をEncodingでエンコードしたバイナリ文字列とする
> -spec f(binary(), Encoding) :: ok.
> f(BinStr) ->
>   UniStr = unicode:characters_to_list(BinStr, Encoding),
>   ..

`f(BinStr, Encoding)` ?
