defmodule KV.RegistryTest do
    use ExUnit.Case, async: true

    setup do
        # The start_supervised! function is injected by use ExUnit.Case. It does the job of starting the KV.Registry process, by calling its start_link/1 function. The advantage of using start_supervised! is that ExUnit will guarantee that the registry process will be shutdown before the next test starts.
        registry = start_supervised!(KV.Registry)
        %{registry: registry}
    end

    test "spawns buckets", %{registry: registry} do
        assert KV.Registry.lookup(registry, "shopping") == :error

        KV.Registry.create(registry, "shopping")
        assert {:ok, bucket} = KV.Registry.lookup(registry, "shopping")

        KV.Bucket.put(bucket, "milk", 1)
        assert KV.Bucket.get(bucket, "milk") == 1
    end

    test "removes buckets on exit", %{registry: registry} do
        KV.Registry.create(registry, "shopping")
    end
end