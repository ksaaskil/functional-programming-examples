defmodule KV.BucketTest do
  use ExUnit.Case, async: true

  # setup macro run before every test in the same process as the test
  setup do
    {:ok, bucket} = KV.Bucket.start_link([])
    %{bucket: bucket}
  end

  # Pattern match to "bucket" from test context
  test "stores values by key", %{bucket: bucket} do
    assert KV.Bucket.get(bucket, "milk") == nil

    KV.Bucket.put(bucket, "milk", 3)
    assert KV.Bucket.get(bucket, "milk") == 3
  end

  test "deletes values by key", %{bucket: bucket} do
    KV.Bucket.put(bucket, "milk", 3)
    assert KV.Bucket.get(bucket, "milk") == 3
    KV.Bucket.delete(bucket, "milk")
    assert KV.Bucket.get(bucket, "milk") == nil
  end

  test "shows distinction between client and server", %{bucket: bucket} do
    # Puts client to sleep
    Process.sleep(100)
    key = "milk"

    val =
      Agent.get_and_update(bucket, fn dict ->
        # Puts server to sleep
        Process.sleep(100)
        Map.pop(dict, key)
      end)

    assert val == nil
  end
end
