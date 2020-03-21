defmodule KV.Registry do
  use GenServer

  # Client API will be here

  def start_link(opts) do
    # Callbacks defined in current __MODULE__
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def lookup(server, name) do
    GenServer.call(server, {:lookup, name})
  end

  def create(server, name) do
    GenServer.cast(server, {:create, name})
  end

  # Callbacks

  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  # The "@impl true" informs the compiler that our intention for the subsequent function definition is to define a callback.
  # Calls are synchronous and the server must send back a response
  @impl true
  def handle_call({:lookup, name}, _from, names) do
    {:reply, Map.fetch(names, name), names}
  end

  # Casts are asynchronous, the server won't send a response back
  @impl true
  def handle_cast({:create, name}, names) do
    if Map.has_key?(names, name) do
      {:noreply, names}
    else
      {:ok, bucket} = KV.Bucket.start_link([])
      {:noreply, Map.put(names, name, bucket)}
    end
  end
end
