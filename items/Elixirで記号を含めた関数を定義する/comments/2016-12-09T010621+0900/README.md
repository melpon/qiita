```
defmodule A do
  def unquote(:"!\"#$%&'()=~|")(a, b), do: a + b
end
```

とかでよさそうな気がしますがどうでしょう
