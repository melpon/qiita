Mapと構造体は非常に似ています。

構造体は、単に `:__struct__` というキーを持っているだけのMapです。
詳細は[構造体のドキュメント](http://elixir-lang.org/getting-started/structs.html)を読んでもらうとして、Mapと構造体を相互変換する方法についていくつか書きます。

## 正攻法

Map→構造体には、`Kernel.struct/2`を使います。

```elixir
defmodule User do
  defstruct [:name]
end

map = %{name: "melpon"}

user = struct(User, map)
```

構造体→Mapには、`Map.from_struct/1` を使います。

```elixir
^map = Map.from_struct(user)
```

なぜ片方が `Map` モジュールなのに、もう片方が `Kernel` モジュールにあるのか、という疑問はあるものの、簡単ですね。

これだけだと面白くないので、もう少しいろんな方法を考えてみましょう。

## `:__struct__` を直接弄る

冒頭に書いたように、構造体は単に `:__struct__` があるかどうかだけの違いしか無いので、直接それを入れるだけで構造体として扱われるようになります。

Map→構造体

```elixir
^user = Map.put(map, :__struct__, User)
```

構造体→Map

```
^map = Map.delete(user, :__struct__)
```

## マージする

`%User{}`と書くと、デフォルト値で初期化された構造体になるので、これにマップの内容をマージすることで構造体を作れます。

Map→構造体

```elixir
^user = Map.merge(%User{}, map)
```

## おまけ: JSONのMapから構造体を作る

JSONの文字列を、例えば[`Poison`モジュール](https://github.com/devinus/poison)を使い、`as:`パラメータを指定せずデコードした場合、Map型になります。
その際、キーはアトムではなく、文字列になってしまいます。

```elixir
map = Poison.decode!("{\"name\": \"melpon\"}")
# map == %{"name" => "melpon"}
```

このデータから構造体を作りたい場合は、上記に書いている方法だと作れません。
また、キーの `"name"` を `:name` アトムに変換するのは、[可能な限り避けるべき](http://qiita.com/KOU_CHANG/items/c5eef2c5e6dc35b3fd65)です。
特にこの JSON が HTTP の POST パラメータに入っているデータである場合は、絶対にやめましょう。

ではどうやって変換するかというと、以下のようになります。

```elixir
map = %{"name" => "melpon"}

empty = %User{}
elements = Enum.map(Map.from_struct(empty),
                    fn {k, _} ->
                      {k, Map.fetch!(map, Atom.to_string(k))}
                    end)
user = struct(User, elements)
```

戦略的には、

1. `User`構造体のキーの一覧を取得して、
2. それらを文字列に変換し、Mapから値を取得して、
3. 構造体を構築する。

となります。

構造体のキーの一覧を取得するには、まずデフォルト値で初期化された構造体 `empty` を作り、それを `Map.from_struct(empty)` で Map に変換すれば取得できます。

このキー `k` はアトム型であるため、それを `Atom.to_string(k)` で文字列に変換しています。
その変換した文字列をキーに、 `Map.fetch!/2` で `map` を検索します。
今回のケースでは、構造体の値が無ければエラーにするという方針でやっていますが、これはアプリケーションのポリシーによって変わる部分でしょう。

`map`から値が取れれば、あとは `struct(User, elements)` で構造体を作るだけです。

もう少し汎用的にすれば、以下のようになるでしょう。

```elixir
defmodule Util do
  def jsonmap_to_struct(jsonmap, struct_type) do
    empty = struct(struct_type)
    elements = Enum.map(Map.from_struct(empty),
                        fn {k, _} ->
                          {k, Map.fetch!(jsonmap, Atom.to_string(k))}
                        end)
    struct(struct_type, elements)
  end
end
```

```elixir
Util.jsonmap_to_struct(%{"name" => "melpon"}, User)
# → %User{name: "melpon"}
```

このように、JSON の Map から構造体にするのは、少し手間が掛かります。
しかし `user.name` のようにアクセスできるのは非常に便利であるし、理解しやすくなるため、JSON の Map は、可能なら構造体に入れておいた方がいいでしょう。
