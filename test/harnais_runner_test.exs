defmodule HarnaisRunnerTest do
  use ExUnit.Case
  doctest HarnaisRunner

  test "greets the world" do
    assert HarnaisRunner.hello() == :world
  end
end
