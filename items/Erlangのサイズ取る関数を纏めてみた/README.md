Erlang、毎回リストのサイズやバイナリのサイズを調べる時に、何の関数を使えばいいんだっけってググることになるので、ちょっと纏めてみました。


|データ構造|関数|
|:-:|:-:|
|リスト `[]`|`erlang:length/1`
|タプル `{}`|`erlang:tuple_size/1`<br>または<br>`erlang:size/1`|
|バイナリ `<<>>`|`erlang:byte_size/1`<br>または<br>`erlang:size/1`|
|マップ `%{}`|`erlang:map_size/1`<br>または`maps:size/1`|

この一貫性の無さすごくない？[^1]

[^1]: 具体的にどの辺が一貫性が無いかというと、<br>- リストだけsizeという単語を含まずlengthになっている<br>- タプルとバイナリは`erlang:size`でサイズが取れるのに、リストとマップは`erlang:size`では取れない<br>- マップは`maps`モジュールにサイズ取得関数があるのに、`lists`や`binary`モジュールには無い
