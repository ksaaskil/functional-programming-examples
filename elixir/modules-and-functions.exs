
# Capture function from Math.module
# Question mark denotes a function returning boolean
fun = &Math.zero?/1

IO.puts is_function(fun)

# Anonymous function `fun` must be invoked with a dot before parentheses
IO.puts fun.(0)
# IO.puts Math.zero?(0)

# Shorthand for creating functions:
fun2 = &(&1 + 1)
# same as
fun3 = fn x -> x + 1 end

# Default arguments 

defmodule Concat do
    def join(a, b \\ nil, sep \\ " ")

    def join(a, b, _sep) when is_nil(b) do
        a
    end

    def join(a, b, sep) do
        a <> sep <> b
    end


end

IO.puts Concat.join("Hello", "World")
IO.puts Concat.join("Hello", "World", "--")
