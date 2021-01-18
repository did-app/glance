defmodule EnvTest do
  use ExUnit.Case
  doctest Env

  test "greets the world" do
    assert Env.hello() == :world
  end
end
