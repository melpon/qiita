## 構造体とマップの違い

Elixir で「構造体とマップの違いは？」と聞かれたら、まず最初に出てくるのが `__struct__` の有無でしょう。

```elixir
defmodule Person do
  defstruct [:name, :age]
end

iex> person = %Person{name: "John", age: 42}
iex> Map.keys(person)
[:__struct__, :age, :name]
iex> person.__struct__
Person
```

ただ、構造体とマップの違いは他にもあります。
それは、**マップは [`Enumerable`](https://hexdocs.pm/elixir/Enumerable.html) プロトコルと [`Access`](https://hexdocs.pm/elixir/Access.html) ビヘイビアを実装しているが、構造体は実装していない**という点です。[^1]

[^1]: 他には `Collectable` プロトコルも実装していないのですが、それは省略

そのため、これらの振る舞いに依存した関数は、構造体では動作しません。
例を上げていきます。

### `for ... do`

構造体は `for` で列挙できません。これは `Enumerable` プロトコルを実装していないからです。

```elixir
iex> person = %Person{name: "John", age: 42}
iex> for _ <- person, do: nil
** (Protocol.UndefinedError) protocol Enumerable not implemented for %Person{age: 42, name: "John"}
    (elixir) lib/enum.ex:1: Enumerable.impl_for!/1
    (elixir) lib/enum.ex:116: Enumerable.reduce/3
    (elixir) lib/enum.ex:1823: Enum.reduce/3
```

構造体のキーや値を全部列挙したい場合なんかにやってしまうことがあります。

### `person[:name]`

`person[:name]` でアクセスすることは出来ません。これは `Access` ビヘイビアを実装していないからです。

```elixir
iex> person = %Person{name: "John", age: 42}
iex> person[:name]
** (UndefinedFunctionError) function Person.fetch/2 is undefined (Person does not implement the Access behaviour)
    Person.fetch(%Person{age: 42, name: "John"}, :name)
    (elixir) lib/access.ex:304: Access.get/3
```

見ての通り、`person[:name]` という操作は `Access` ビヘイビアの `fetch/2` を要求するので、構造体では動作しません。
指定した atom の値を取得する処理を書いた場合なんかにやってしまうことがあります。

```elixir
def get_value(data, key) do
  data[key]
end
```

### `put_in/3`, `update_in/3`

[`put_in/3`](https://hexdocs.pm/elixir/Kernel.html#put_in/3) や [`update_in/3`](https://hexdocs.pm/elixir/Kernel.html#update_in/3) も `Access` ビヘイビアを利用しているので、動作しません。

```elixir
iex> person = %Person{name: "John", age: 42}
iex> put_in(person, [:name], "Smith")
** (UndefinedFunctionError) function Person.get_and_update/3 is undefined (Person does not implement the Access behaviour)
    Person.get_and_update(%Person{age: 42, name: "John"}, :name, #Function<12.67985749/1 in Kernel.put_in/3>)
    (elixir) lib/access.ex:356: Access.get_and_update/3
    (elixir) lib/kernel.ex:1880: Kernel.put_in/3
```

ただし [`put_in/2`](https://hexdocs.pm/elixir/Kernel.html#put_in/2) （マクロ実装の方）を使って構造体を `.` でアクセスすれば動作します。

```elixir
iex> put_in(person.name, "Smith")
%Person{age: 42, name: "Smith"}
```

`Access` ビヘイビアを実装せずに構造体に対して put_in したいなら `put_in/2` を使うしかありません。[^2]

[^2]: なおこのケースなら `%{person | name: "Smith"}` とした方が素直で良いです。`put_in/2` が生きるのは `person.birthday.year` みたいなネストしたデータを扱う場合です。

マクロの利用を嫌って `put_in/3` を使おうとした時にやらかしました。以降は素直に `put_in/2`, `update_in/2` を使っています。

## 構造体をマップに近づける

どれも `Map.from_struct/1` を使って変換すれば動作するのですが、どうせなので構造体をマップに近づけることでどれも動作するようにしてみましょう。

まず `Enumerable` プロトコルに対応します。

```elixir
defimpl Enumerable, for: Person do
  defp to_list(enumerable) do
    list =
      enumerable
      |> Map.from_struct()
      |> Map.to_list()
    [{:__struct__, Person} | list]
  end

  def count(enumerable) do
    Enumerable.List.count(to_list(enumerable))
  end

  def member?(enumerable, element) do
    Enumerable.List.member?(to_list(enumerable), element)
  end
  
  def reduce(enumerable, acc, fun) do
    Enumerable.List.reduce(to_list(enumerable), acc, fun)
  end
end
```

構造体をリストにした後、リスト型に対する `Enumerable` の実装に渡しているだけです。
これで `Enumerable` プロトコルを実装したので、以下のように `for` でキーと値を列挙できるようになりました。

```elixir
iex> for {k, v} <- person do
...>   {k, v}
...> end
[__struct__: Person, age: 42, name: "John"]
```

続いて `Access` ビヘイビアを実装します。

```elixir
defmodule Person do
  defstruct [:name, :age]

  # Access ビヘイビアを実装する
  @behaviour Access

  @impl Access
  def fetch(term, key) do
    Access.fetch(Map.from_struct(term), key)
  end

  @impl Access
  def get(term, key, default) do
    Access.get(Map.from_struct(term), key, default)
  end

  @impl Access
  def get_and_update(container, key, fun) do
    {value, container} = Access.get_and_update(Map.from_struct(container), key, fun)
    {value, struct(__MODULE__, container)}
  end

  @impl Access
  def pop(data, key) do
    {value, data} = Access.pop(Map.from_struct(data), key)
    {value, struct(__MODULE__, data)}
  end
end
```

これで `person[:name]` と `put_in/3` でもちゃんと動作するようになりました。

```elixir
iex> person = %Person{name: "John", age: 42}
iex> person[:name]
"John"
iex> put_in(person, [:name], "Smith")
%Person{age: 42, name: "Smith"}
```

ここまですると、割と「構造体は `__struct__` が追加されただけのマップ」と言っても良さそうです。
ただ、構造体をマップみたいにフレキシブルに扱いたいなら、都度マップに変換すればいいだけです。ここまでする必要は多分ほぼ無いでしょう。

## まとめ

構造体とマップは `Enumerable` プロトコルや `Access` ビヘイビアを実装していないということを、割と汎用的な処理を書いている時には忘れがちです。
「構造体なんてマップみたいなものでしょ」と思ってコードを書く時にはこの辺に気を付けましょう。
