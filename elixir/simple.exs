defmodule Math do
  def sum(a, b) do
    do_sum(a, b)
  end

  def zero?(0), do: true
  def zero?(x) when is_integer(x) do
    false
  end

  defp do_sum(a, b) do
    a + b
  end
end

defmodule Recursion do
  def print_multiple_times(msg, n) when n <= 1 do
    IO.puts msg
  end

  def print_multiple_times(msg, n) do
    IO.puts msg
    print_multiple_times(msg, n - 1)
  end
end

IO.puts Math.sum(1, 2)    #=> 3
# IO.puts Math.do_sum(1, 2) #=> ** (UndefinedFunctionError)

IO.puts Enum.reduce([1, 2, 3], 0, fn(x, acc) -> x + acc end)

stream = Stream.cycle([1, 2, 3])
Enum.take(stream, 10) |> Enum.each(&IO.write/1)
Enum.take(stream, 10) |> Enum.each(fn a -> IO.puts a end)
# IO.puts vals |> IO.puts
